{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : GBNet.Replication.Delta
-- Description : Delta compression for bandwidth-efficient state replication
--
-- 'DeltaTracker' manages per-sequence snapshots for delta encoding against
-- acknowledged baselines. 'BaselineManager' provides a ring buffer of
-- confirmed snapshots per connection.
--
-- Delta compression only sends changed fields between states, dramatically
-- reducing bandwidth for entity replication.
module GBNet.Replication.Delta
  ( -- * Types
    BaselineSeq,
    noBaseline,

    -- * NetworkDelta typeclass
    NetworkDelta (..),

    -- * Delta tracker (sender side)
    DeltaTracker,
    newDeltaTracker,
    deltaEncode,
    deltaOnAck,
    deltaReset,
    deltaConfirmedSeq,

    -- * Baseline manager (receiver side)
    BaselineManager,
    newBaselineManager,
    pushBaseline,
    getBaseline,
    baselineReset,
    baselineCount,
    baselineIsEmpty,

    -- * Decoding
    deltaDecode,
  )
where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import GBNet.Reliability (MonoTime, elapsedMs)
import GBNet.Serialize.BitBuffer
  ( ReadResult (..),
    empty,
    fromBytes,
    readBits,
    toBytes,
    writeBits,
  )
import GBNet.Serialize.Class
  ( BitDeserialize (..),
    BitSerialize (..),
  )

-- | Sequence number used to identify baseline snapshots on the wire.
type BaselineSeq = Word16

-- | Sentinel value indicating no baseline is available (full state required).
noBaseline :: BaselineSeq
noBaseline = maxBound

-- | Signed difference between two baseline sequence numbers with wraparound.
baselineSeqDiff :: BaselineSeq -> BaselineSeq -> Int32
baselineSeqDiff s1 s2 = fromIntegral (s1 - s2 :: Word16)

-- | Wire header size for baseline sequence.
baselineSeqBits :: Int
baselineSeqBits = 16

-- | Typeclass for delta-compressed network types.
--
-- Implement this for types that benefit from delta compression.
-- The 'Delta' associated type contains only the changed fields.
--
-- Example:
--
-- @
-- data PlayerState = PlayerState { pos :: Vec3, health :: Word8 }
-- data PlayerDelta = PlayerDelta { dPos :: Maybe Vec3, dHealth :: Maybe Word8 }
--
-- instance NetworkDelta PlayerState where
--   type Delta PlayerState = PlayerDelta
--   diff current baseline = PlayerDelta
--     { dPos = if pos current /= pos baseline then Just (pos current) else Nothing
--     , dHealth = if health current /= health baseline then Just (health current) else Nothing
--     }
--   apply state delta = state
--     { pos = maybe (pos state) id (dPos delta)
--     , health = maybe (health state) id (dHealth delta)
--     }
-- @
class NetworkDelta a where
  -- | The delta type containing only changed fields.
  type Delta a

  -- | Compute a delta from a baseline to the current state.
  diff :: a -> a -> Delta a

  -- | Apply a delta to a state, updating changed fields.
  apply :: a -> Delta a -> a

-- | Tracks snapshots and encodes deltas against acknowledged baselines.
--
-- Used on the sender side to:
-- 1. Store pending snapshots awaiting ACK
-- 2. Encode current state as delta against confirmed baseline
-- 3. Promote ACK'd snapshots to confirmed baseline
data DeltaTracker a = DeltaTracker
  { -- | Unconfirmed snapshots awaiting ACK, ordered by sequence
    dtPending :: !(Seq (BaselineSeq, a)),
    -- | The most recently ACK-confirmed baseline
    dtConfirmed :: !(Maybe (BaselineSeq, a)),
    -- | Maximum pending snapshots before oldest are dropped
    dtMaxPending :: !Int
  }

-- | Create a new delta tracker with the given maximum pending snapshot count.
newDeltaTracker :: Int -> DeltaTracker a
newDeltaTracker maxPending =
  DeltaTracker
    { dtPending = Seq.empty,
      dtConfirmed = Nothing,
      dtMaxPending = maxPending
    }

-- | Encode a snapshot as delta against confirmed baseline.
--
-- Returns encoded bytes containing @[baseline_seq: u16][delta_payload]@.
-- Stores the snapshot as pending until ACK'd.
deltaEncode ::
  (NetworkDelta a, BitSerialize a, BitSerialize (Delta a)) =>
  BaselineSeq ->
  a ->
  DeltaTracker a ->
  (BS.ByteString, DeltaTracker a)
deltaEncode seq' current tracker =
  let -- Encode based on whether we have a confirmed baseline
      buf = case dtConfirmed tracker of
        Just (baseSeq, baseline) ->
          let delta = diff current baseline
           in bitSerialize delta $ writeBits (fromIntegral baseSeq) baselineSeqBits empty
        Nothing ->
          -- No baseline - write sentinel + full state
          bitSerialize current $ writeBits (fromIntegral noBaseline) baselineSeqBits empty

      -- Store pending snapshot
      pending = dtPending tracker
      pending' =
        if Seq.length pending >= dtMaxPending tracker
          then Seq.drop 1 pending Seq.|> (seq', current)
          else pending Seq.|> (seq', current)

      tracker' = tracker {dtPending = pending'}
   in (toBytes buf, tracker')

-- | Called when a sequence is ACK'd.
--
-- Promotes the matching snapshot to confirmed baseline and discards
-- older pending entries.
deltaOnAck :: BaselineSeq -> DeltaTracker a -> DeltaTracker a
deltaOnAck seq' tracker =
  case Seq.findIndexL (\(s, _) -> s == seq') (dtPending tracker) of
    Nothing -> tracker
    Just idx ->
      let (ackSeq, snapshot) = Seq.index (dtPending tracker) idx
          -- Drop everything older than the acked position
          pending' =
            Seq.filter (\(s, _) -> baselineSeqDiff s ackSeq >= 0) $
              Seq.drop (idx + 1) (dtPending tracker)
       in tracker
            { dtPending = pending',
              dtConfirmed = Just (ackSeq, snapshot)
            }

-- | Reset tracker state (e.g. on reconnect).
deltaReset :: DeltaTracker a -> DeltaTracker a
deltaReset tracker =
  tracker
    { dtPending = Seq.empty,
      dtConfirmed = Nothing
    }

-- | Returns the confirmed baseline sequence, if any.
deltaConfirmedSeq :: DeltaTracker a -> Maybe BaselineSeq
deltaConfirmedSeq = fmap fst . dtConfirmed

-- | Ring buffer of confirmed snapshots per connection.
--
-- Used on the receiving side to look up baselines referenced in
-- incoming delta payloads.
data BaselineManager a = BaselineManager
  { -- | Stored snapshots: (sequence, state, timestamp)
    bmSnapshots :: !(Seq (BaselineSeq, a, MonoTime)),
    -- | Maximum stored snapshots
    bmMaxSnapshots :: !Int,
    -- | Timeout in milliseconds before snapshots expire
    bmTimeoutMs :: !Double
  }

-- | Create a new baseline manager with the given capacity and expiration timeout.
newBaselineManager ::
  -- | Maximum snapshots to store
  Int ->
  -- | Timeout in milliseconds
  Double ->
  BaselineManager a
newBaselineManager maxSnapshots timeoutMs =
  BaselineManager
    { bmSnapshots = Seq.empty,
      bmMaxSnapshots = maxSnapshots,
      bmTimeoutMs = timeoutMs
    }

-- | Store a confirmed snapshot at the given sequence.
pushBaseline :: BaselineSeq -> a -> MonoTime -> BaselineManager a -> BaselineManager a
pushBaseline seq' state now manager =
  let -- Evict expired
      snapshots =
        Seq.filter (\(_, _, ts) -> elapsedMs ts now < bmTimeoutMs manager) $
          bmSnapshots manager
      -- Evict oldest if at capacity
      snapshots' =
        if Seq.length snapshots >= bmMaxSnapshots manager
          then Seq.drop 1 snapshots
          else snapshots
      -- Add new snapshot
      snapshots'' = snapshots' Seq.|> (seq', state, now)
   in manager {bmSnapshots = snapshots''}

-- | Look up a baseline by sequence number.
getBaseline :: BaselineSeq -> BaselineManager a -> Maybe a
getBaseline seq' manager =
  case Seq.findIndexR (\(s, _, _) -> s == seq') (bmSnapshots manager) of
    Nothing -> Nothing
    Just idx ->
      let (_, state, _) = Seq.index (bmSnapshots manager) idx
       in Just state

-- | Clear all stored baselines.
baselineReset :: BaselineManager a -> BaselineManager a
baselineReset manager = manager {bmSnapshots = Seq.empty}

-- | Number of stored baselines.
baselineCount :: BaselineManager a -> Int
baselineCount = Seq.length . bmSnapshots

-- | Whether the baseline buffer is empty.
baselineIsEmpty :: BaselineManager a -> Bool
baselineIsEmpty = Seq.null . bmSnapshots

-- | Decode a delta-encoded payload.
--
-- Requires access to the baseline manager to look up the referenced baseline.
deltaDecode ::
  (NetworkDelta a, BitDeserialize a, BitDeserialize (Delta a)) =>
  BS.ByteString ->
  BaselineManager a ->
  Either String a
deltaDecode dat baselines =
  let buf = fromBytes dat
   in case readBits baselineSeqBits buf of
        Left err -> Left err
        Right result ->
          let baseSeqVal = readValue result
              buf' = readBuffer result
              baseSeq = fromIntegral baseSeqVal :: BaselineSeq
           in if baseSeq == noBaseline
                then -- Full state
                  case bitDeserialize buf' of
                    Left err -> Left err
                    Right stateResult -> Right (readValue stateResult)
                else -- Delta against baseline
                  case getBaseline baseSeq baselines of
                    Nothing -> Left $ "Missing baseline for seq " ++ show baseSeq
                    Just baseline ->
                      case bitDeserialize buf' of
                        Left err -> Left err
                        Right deltaResult ->
                          let delta = readValue deltaResult
                           in Right (apply baseline delta)
