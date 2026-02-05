{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : GBNet.Reliability
-- Description : Reliable packet delivery with RTT estimation and ACK tracking
--
-- Jacobson\/Karels RTT estimation, adaptive RTO, fast retransmit on 3 NACKs,
-- bounded in-flight tracking, and rolling packet loss window.
module GBNet.Reliability
  ( -- * Constants
    initialRtoMillis,
    ackBitsWindow,
    rttAlpha,
    rttBeta,
    minRtoMs,
    maxRtoMs,
    lossWindowSize,
    fastRetransmitThreshold,
    defaultMaxSequenceDistance,
    defaultMaxInFlight,

    -- * Monotonic time
    MonoTime,
    elapsedMs,

    -- * Loss window
    LossWindow (..),
    emptyLossWindow,

    -- * Sequence buffer
    SequenceBuffer (..),
    newSequenceBuffer,
    sbInsert,
    sbExists,
    sbGet,

    -- * Sent packet record
    SentPacketRecord (..),

    -- * Reliable endpoint
    ReliableEndpoint (..),
    AckResult (..),
    newReliableEndpoint,
    withMaxInFlight,
    nextSequence,
    onPacketSent,
    onPacketsReceived,
    processAcks,
    getAckInfo,
    updateRtt,
    recordLossSample,
    packetLossPercent,
    isInFlight,
    packetsInFlight,
    rtoMs,
    srttMs,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (runST)
import Data.Bits (complement, popCount, shiftL, (.&.), (.|.))
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Class (MonoTime (..))
import GBNet.Types (ChannelId (..), SequenceNum (..))
import GBNet.Util (sequenceDiff, sequenceGreaterThan)

-- Constants

initialRtoMillis :: Double
initialRtoMillis = 100.0

ackBitsWindow :: Word16
ackBitsWindow = 64

rttAlpha :: Double
rttAlpha = 0.125

rttBeta :: Double
rttBeta = 0.25

minRtoMs :: Double
minRtoMs = 50.0

maxRtoMs :: Double
maxRtoMs = 2000.0

lossWindowSize :: Int
lossWindowSize = 256

fastRetransmitThreshold :: Word8
fastRetransmitThreshold = 3

defaultMaxSequenceDistance :: Word16
defaultMaxSequenceDistance = 32768

defaultMaxInFlight :: Int
defaultMaxInFlight = 256

-- | 256-bit loss window using 4 Word64 values.
-- Zero-allocation updates via bit operations.
data LossWindow = LossWindow
  { lwBits0 :: {-# UNPACK #-} !Word64, -- bits 0-63
    lwBits1 :: {-# UNPACK #-} !Word64, -- bits 64-127
    lwBits2 :: {-# UNPACK #-} !Word64, -- bits 128-191
    lwBits3 :: {-# UNPACK #-} !Word64 -- bits 192-255
  }
  deriving (Show, Eq)

instance NFData LossWindow where
  rnf (LossWindow b0 b1 b2 b3) = rnf b0 `seq` rnf b1 `seq` rnf b2 `seq` rnf b3

-- | Empty loss window (all successful).
emptyLossWindow :: LossWindow
emptyLossWindow = LossWindow 0 0 0 0

-- | Set a bit in the loss window. Index must be 0-255.
lwSetBit :: Int -> Bool -> LossWindow -> LossWindow
lwSetBit idx val lw
  | idx < 64 = lw {lwBits0 = setBitWord64 (idx .&. 63) val (lwBits0 lw)}
  | idx < 128 = lw {lwBits1 = setBitWord64 (idx .&. 63) val (lwBits1 lw)}
  | idx < 192 = lw {lwBits2 = setBitWord64 (idx .&. 63) val (lwBits2 lw)}
  | otherwise = lw {lwBits3 = setBitWord64 (idx .&. 63) val (lwBits3 lw)}
{-# INLINE lwSetBit #-}

-- | Count set bits (losses) in the first n entries.
lwCountLosses :: Int -> LossWindow -> Int
lwCountLosses n lw
  | n <= 0 = 0
  | n >= lossWindowSize = totalBits
  | n <= 64 = popCount (lwBits0 lw .&. maskN n)
  | n <= 128 = popCount (lwBits0 lw) + popCount (lwBits1 lw .&. maskN (n - 64))
  | n <= 192 = popCount (lwBits0 lw) + popCount (lwBits1 lw) + popCount (lwBits2 lw .&. maskN (n - 128))
  | otherwise = popCount (lwBits0 lw) + popCount (lwBits1 lw) + popCount (lwBits2 lw) + popCount (lwBits3 lw .&. maskN (n - 192))
  where
    totalBits = popCount (lwBits0 lw) + popCount (lwBits1 lw) + popCount (lwBits2 lw) + popCount (lwBits3 lw)
    maskN k = (1 `shiftL` k) - 1
{-# INLINE lwCountLosses #-}

-- | Set or clear a bit in a Word64.
setBitWord64 :: Int -> Bool -> Word64 -> Word64
setBitWord64 idx True w = w .|. (1 `shiftL` idx)
setBitWord64 idx False w = w .&. complement (1 `shiftL` idx)
{-# INLINE setBitWord64 #-}

-- Monotonic time

-- | Elapsed time in milliseconds.
elapsedMs :: MonoTime -> MonoTime -> Double
elapsedMs start now = fromIntegral (now - start) / 1e6
{-# INLINE elapsedMs #-}

-- SequenceBuffer

-- | Circular buffer indexed by SequenceNum (Word16 under the hood).
data SequenceBuffer a = SequenceBuffer
  { sbEntries :: !(Map Word16 (SequenceNum, a)),
    sbSequence :: !SequenceNum,
    sbSize :: !Int
  }
  deriving (Show)

newSequenceBuffer :: Int -> SequenceBuffer a
newSequenceBuffer size =
  SequenceBuffer
    { sbEntries = Map.empty,
      sbSequence = 0,
      sbSize = size
    }

sbInsert :: SequenceNum -> a -> SequenceBuffer a -> SequenceBuffer a
sbInsert seqNum val buf
  | sequenceGreaterThan seqNum (sbSequence buf) =
      let diff = fromIntegral (sequenceDiff seqNum (sbSequence buf)) :: Int
          cleared =
            if diff < sbSize buf
              then clearRange (sbSequence buf) diff buf
              else buf {sbEntries = Map.empty}
          idx = fromIntegral seqNum `mod` sbSize buf
       in cleared
            { sbSequence = seqNum,
              sbEntries = Map.insert (fromIntegral idx) (seqNum, val) (sbEntries cleared)
            }
  | otherwise =
      let idx = fromIntegral seqNum `mod` sbSize buf
       in buf {sbEntries = Map.insert (fromIntegral idx) (seqNum, val) (sbEntries buf)}

clearRange :: SequenceNum -> Int -> SequenceBuffer a -> SequenceBuffer a
clearRange currentSeq diff buf = go 0 (sbEntries buf)
  where
    go i entries
      | i >= diff = buf {sbEntries = entries}
      | otherwise =
          let s = currentSeq + fromIntegral (i + 1)
              idx = fromIntegral s `mod` sbSize buf
           in go (i + 1) (Map.delete (fromIntegral idx) entries)

sbExists :: SequenceNum -> SequenceBuffer a -> Bool
sbExists seqNum buf =
  let idx = fromIntegral seqNum `mod` sbSize buf
   in case Map.lookup (fromIntegral idx) (sbEntries buf) of
        Just (storedSeq, _) -> storedSeq == seqNum
        Nothing -> False

sbGet :: SequenceNum -> SequenceBuffer a -> Maybe a
sbGet seqNum buf =
  let idx = fromIntegral seqNum `mod` sbSize buf
   in case Map.lookup (fromIntegral idx) (sbEntries buf) of
        Just (storedSeq, v) | storedSeq == seqNum -> Just v
        _ -> Nothing

-- ReceivedBuffer (optimized for packet deduplication)

-- | Buffer size for received packets (must be power of 2 for fast modulo).
receivedBufferSize :: Int
receivedBufferSize = 256
{-# INLINE receivedBufferSize #-}

-- | Mask for fast modulo (size - 1).
receivedBufferMask :: Int
receivedBufferMask = receivedBufferSize - 1
{-# INLINE receivedBufferMask #-}

-- | High-performance received packet tracker using unboxed Word16 vector.
-- Stores sequence numbers directly - O(1) lookup via index, O(1) insert.
-- 512 bytes total (256 × Word16), cache-friendly contiguous memory.
data ReceivedBuffer = ReceivedBuffer
  { rbSeqs :: !(VU.Vector Word16), -- Unboxed: contiguous like C array
    rbHighest :: !SequenceNum -- Track highest for ack bit calculation
  }
  deriving (Show)

instance NFData ReceivedBuffer where
  rnf (ReceivedBuffer v h) = rnf v `seq` rnf h

-- | Create empty received buffer (all slots set to maxBound as "empty" sentinel).
newReceivedBuffer :: ReceivedBuffer
newReceivedBuffer =
  ReceivedBuffer
    { rbSeqs = VU.replicate receivedBufferSize maxBound,
      rbHighest = 0
    }

-- | O(1) check if sequence number was received. Single array index + comparison.
rbExists :: SequenceNum -> ReceivedBuffer -> Bool
rbExists (SequenceNum s) (ReceivedBuffer v _) =
  VU.unsafeIndex v (fromIntegral s .&. receivedBufferMask) == s
{-# INLINE rbExists #-}

-- | Batched insert - O(n) for n sequence numbers with ONE thaw/freeze.
-- This is the high-performance path for processing multiple received packets.
rbInsertMany :: [SequenceNum] -> ReceivedBuffer -> ReceivedBuffer
rbInsertMany [] buf = buf
rbInsertMany seqs buf = runST $ do
  mv <- VU.thaw (rbSeqs buf)
  newHighest <- go mv (rbHighest buf) seqs
  v' <- VU.unsafeFreeze mv
  return $! ReceivedBuffer v' newHighest
  where
    go _ !highest [] = return highest
    go mv !highest (sn@(SequenceNum s) : rest) = do
      VUM.unsafeWrite mv (fromIntegral s .&. receivedBufferMask) s
      let highest' = if sequenceGreaterThan sn highest then sn else highest
      go mv highest' rest
{-# INLINE rbInsertMany #-}

-- SentPacketRecord

data SentPacketRecord = SentPacketRecord
  { sprChannelId :: !ChannelId,
    sprChannelSequence :: !SequenceNum,
    sprSendTime :: !MonoTime,
    sprSize :: !Int,
    sprNackCount :: !Word8
  }
  deriving (Show)

instance NFData SentPacketRecord where
  rnf (SentPacketRecord cid cseq t s n) =
    rnf cid `seq` rnf cseq `seq` rnf t `seq` rnf s `seq` rnf n

-- | Ring buffer entry: stores sequence number for validation + record.
-- Nothing = slot empty, Just = occupied with given sequence.
data RingEntry = RingEmpty | RingEntry !SequenceNum !SentPacketRecord
  deriving (Show)

instance NFData RingEntry where
  rnf RingEmpty = ()
  rnf (RingEntry s r) = rnf s `seq` rnf r

-- | O(1) sent packet buffer using ring buffer indexed by sequence mod size.
-- All operations are O(1) except count and findOldest which are O(n).
data SentPacketBuffer = SentPacketBuffer
  { spbEntries :: !(V.Vector RingEntry),
    spbCount :: !Int -- Cached count for O(1) access
  }
  deriving (Show)

instance NFData SentPacketBuffer where
  rnf (SentPacketBuffer e c) = rnf e `seq` rnf c

-- | Buffer size (fixed at 256 for Word16 sequence space efficiency).
ringBufferSize :: Int
ringBufferSize = 256
{-# INLINE ringBufferSize #-}

-- | Create empty sent packet buffer.
newSentPacketBuffer :: Int -> SentPacketBuffer
newSentPacketBuffer _ =
  SentPacketBuffer
    { spbEntries = V.replicate ringBufferSize RingEmpty,
      spbCount = 0
    }

-- | Convert sequence to ring index. O(1).
seqToIdx :: SequenceNum -> Int
seqToIdx (SequenceNum s) = fromIntegral s .&. (ringBufferSize - 1)
{-# INLINE seqToIdx #-}

-- | O(1) lookup by sequence number.
spbLookup :: SequenceNum -> SentPacketBuffer -> Maybe SentPacketRecord
spbLookup seqNum buf =
  case V.unsafeIndex (spbEntries buf) (seqToIdx seqNum) of
    RingEntry storedSeq record
      | storedSeq == seqNum -> Just record
    _ -> Nothing
{-# INLINE spbLookup #-}

-- | O(1) membership test.
spbMember :: SequenceNum -> SentPacketBuffer -> Bool
spbMember seqNum buf =
  case V.unsafeIndex (spbEntries buf) (seqToIdx seqNum) of
    RingEntry storedSeq _
      | storedSeq == seqNum -> True
    _ -> False
{-# INLINE spbMember #-}

-- | O(1) insert using ST for mutation, returns new immutable buffer.
spbInsert :: SequenceNum -> SentPacketRecord -> SentPacketBuffer -> SentPacketBuffer
spbInsert seqNum record buf = runST $ do
  mvec <- V.thaw (spbEntries buf)
  let idx = seqToIdx seqNum
  old <- MV.unsafeRead mvec idx
  MV.unsafeWrite mvec idx (RingEntry seqNum record)
  vec' <- V.unsafeFreeze mvec
  let countDelta = case old of
        RingEmpty -> 1
        _ -> 0
  return $! buf {spbEntries = vec', spbCount = spbCount buf + countDelta}
{-# INLINE spbInsert #-}

-- | O(1) delete by sequence number.
spbDelete :: SequenceNum -> SentPacketBuffer -> SentPacketBuffer
spbDelete seqNum buf =
  case V.unsafeIndex (spbEntries buf) idx of
    RingEntry storedSeq _
      | storedSeq == seqNum -> runST $ do
          mvec <- V.thaw (spbEntries buf)
          MV.unsafeWrite mvec idx RingEmpty
          vec' <- V.unsafeFreeze mvec
          return $! buf {spbEntries = vec', spbCount = spbCount buf - 1}
    _ -> buf
  where
    idx = seqToIdx seqNum
{-# INLINE spbDelete #-}

-- | O(n) find oldest entry (for eviction). Only called when buffer full.
spbFindOldest :: SentPacketBuffer -> Maybe (SequenceNum, SentPacketRecord)
spbFindOldest buf = V.foldl' older Nothing (spbEntries buf)
  where
    older Nothing (RingEntry s r) = Just (s, r)
    older Nothing RingEmpty = Nothing
    older acc@(Just (_, r1)) (RingEntry s r2)
      | sprSendTime r2 < sprSendTime r1 = Just (s, r2)
      | otherwise = acc
    older acc RingEmpty = acc

-- ReliableEndpoint

-- | Result of processing ACKs.
data AckResult = AckResult
  { arAcked :: ![(ChannelId, SequenceNum)],
    arFastRetransmit :: ![(ChannelId, SequenceNum)]
  }
  deriving (Show, Eq)

instance NFData AckResult where
  rnf (AckResult a f) = rnf a `seq` rnf f

data ReliableEndpoint = ReliableEndpoint
  { reLocalSequence :: !SequenceNum,
    reRemoteSequence :: !SequenceNum,
    reAckBits :: !Word64,
    reSentPackets :: !SentPacketBuffer,
    reReceivedPackets :: !ReceivedBuffer,
    reMaxSequenceDistance :: !Word16,
    reMaxInFlight :: !Int,
    reSrtt :: !Double,
    reRttvar :: !Double,
    reRto :: !Double,
    reHasRttSample :: !Bool,
    reLossWindow :: !LossWindow,
    reLossWindowIndex :: !Int,
    reLossWindowCount :: !Int,
    reTotalSent :: !Word64,
    reTotalAcked :: !Word64,
    reTotalLost :: !Word64,
    rePacketsEvicted :: !Word64,
    reBytesSent :: !Word64,
    reBytesAcked :: !Word64
  }
  deriving (Show)

newReliableEndpoint :: Int -> ReliableEndpoint
newReliableEndpoint bufferSize =
  ReliableEndpoint
    { reLocalSequence = 0,
      reRemoteSequence = 0,
      reAckBits = 0,
      reSentPackets = newSentPacketBuffer bufferSize,
      reReceivedPackets = newReceivedBuffer,
      reMaxSequenceDistance = defaultMaxSequenceDistance,
      reMaxInFlight = defaultMaxInFlight,
      reSrtt = 0.0,
      reRttvar = 0.0,
      reRto = initialRtoMillis,
      reHasRttSample = False,
      reLossWindow = emptyLossWindow,
      reLossWindowIndex = 0,
      reLossWindowCount = 0,
      reTotalSent = 0,
      reTotalAcked = 0,
      reTotalLost = 0,
      rePacketsEvicted = 0,
      reBytesSent = 0,
      reBytesAcked = 0
    }

withMaxInFlight :: Int -> ReliableEndpoint -> ReliableEndpoint
withMaxInFlight maxFlight ep = ep {reMaxInFlight = maxFlight}

nextSequence :: ReliableEndpoint -> (SequenceNum, ReliableEndpoint)
nextSequence ep =
  let s = reLocalSequence ep
   in (s, ep {reLocalSequence = s + 1})

onPacketSent ::
  SequenceNum ->
  MonoTime ->
  ChannelId ->
  SequenceNum ->
  Int ->
  ReliableEndpoint ->
  ReliableEndpoint
onPacketSent seqNum sendTime channelId channelSeq size ep =
  let ep' =
        if spbCount (reSentPackets ep) >= reMaxInFlight ep
          then evictWorstInFlight ep
          else ep
      record =
        SentPacketRecord
          { sprChannelId = channelId,
            sprChannelSequence = channelSeq,
            sprSendTime = sendTime,
            sprSize = size,
            sprNackCount = 0
          }
   in ep'
        { reSentPackets = spbInsert seqNum record (reSentPackets ep'),
          reTotalSent = reTotalSent ep' + 1,
          reBytesSent = reBytesSent ep' + fromIntegral size
        }

evictWorstInFlight :: ReliableEndpoint -> ReliableEndpoint
evictWorstInFlight ep =
  case spbFindOldest (reSentPackets ep) of
    Nothing -> ep
    Just (worstSeq, _) ->
      ep
        { reSentPackets = spbDelete worstSeq (reSentPackets ep),
          rePacketsEvicted = rePacketsEvicted ep + 1
        }

-- | Process received packets - processes multiple sequence numbers with ONE thaw/freeze.
-- This is the high-performance API for game loops processing many packets per frame.
onPacketsReceived :: [SequenceNum] -> ReliableEndpoint -> ReliableEndpoint
onPacketsReceived seqNums ep =
  let validSeqs = filter isValid seqNums
      newBuf = rbInsertMany validSeqs (reReceivedPackets ep)
      (newRemote, newAckBits) = foldl' updateAckState (reRemoteSequence ep, reAckBits ep) validSeqs
   in ep {reReceivedPackets = newBuf, reRemoteSequence = newRemote, reAckBits = newAckBits}
  where
    isValid sn =
      let dist = fromIntegral (abs (sequenceDiff sn (reRemoteSequence ep))) :: Word32
       in dist <= fromIntegral (reMaxSequenceDistance ep) && not (rbExists sn (reReceivedPackets ep))

    updateAckState (!remote, !bits) sn
      | sequenceGreaterThan sn remote =
          let d = fromIntegral (sequenceDiff sn remote) :: Word64
              newBits
                | d < fromIntegral ackBitsWindow = (bits `shiftL` fromIntegral d) .|. (1 `shiftL` (fromIntegral d - 1))
                | otherwise = 0
           in (sn, newBits)
      | otherwise =
          let d = fromIntegral (sequenceDiff remote sn) :: Word64
           in if d > 0 && d <= fromIntegral ackBitsWindow
                then (remote, bits .|. (1 `shiftL` (fromIntegral d - 1)))
                else (remote, bits)

-- | Mutation to apply to sent packet buffer.
data BufferMutation
  = MutDelete !Int -- Delete at index
  | MutNack !Int !SentPacketRecord !Word8 -- Update nack count at index

-- | Process ACKs from received packet. O(ackBitsWindow) = O(64).
-- Two-phase: pure lookup pass, then single batched mutation.
processAcks :: SequenceNum -> Word64 -> MonoTime -> ReliableEndpoint -> (AckResult, ReliableEndpoint)
processAcks ackSeq ackBitsVal now ep =
  let buf = reSentPackets ep
      -- Phase 1: Pure lookup, collect decisions
      (acked, retrans, mutations, rttSum, rttCount, bytesAcked) =
        foldl' (processOne buf) ([], [], [], 0.0, 0 :: Int, 0) [0 .. fromIntegral ackBitsWindow]
      -- Phase 2: Apply all mutations in one ST block
      buf' = applyMutations mutations buf
      -- Update RTT with average of all samples
      ep' = case rttCount of
        0 -> ep
        _ -> updateRtt (rttSum / fromIntegral rttCount) ep
      -- Record loss samples for each ack (success = not lost)
      ep'' = foldl' (\e _ -> recordLossSample False e) ep' acked
      ep''' =
        ep''
          { reSentPackets = buf',
            reTotalAcked = reTotalAcked ep + fromIntegral (length acked),
            reBytesAcked = reBytesAcked ep + fromIntegral bytesAcked
          }
   in (AckResult acked retrans, ep''')
  where
    processOne buf (!acked, !retrans, !muts, !rttSum, !rttCnt, !bytes) i
      | i == 0 = checkSeq ackSeq True buf acked retrans muts rttSum rttCnt bytes
      | otherwise =
          let seqToCheck = ackSeq - fromIntegral i
              bitSet = (ackBitsVal .&. (1 `shiftL` (i - 1))) /= 0
           in checkSeq seqToCheck bitSet buf acked retrans muts rttSum rttCnt bytes

    checkSeq seqNum bitSet buf acked retrans muts rttSum rttCnt bytes =
      case spbLookup seqNum buf of
        Nothing -> (acked, retrans, muts, rttSum, rttCnt, bytes)
        Just record
          | bitSet ->
              let idx = seqToIdx seqNum
                  pair = (sprChannelId record, sprChannelSequence record)
                  rtt = elapsedMs (sprSendTime record) now
               in (pair : acked, retrans, MutDelete idx : muts, rttSum + rtt, rttCnt + 1, bytes + sprSize record)
          | otherwise ->
              let idx = seqToIdx seqNum
                  newNack = min 255 (sprNackCount record + 1)
                  retrans' =
                    if newNack == fastRetransmitThreshold
                      then (sprChannelId record, sprChannelSequence record) : retrans
                      else retrans
               in (acked, retrans', MutNack idx record newNack : muts, rttSum, rttCnt, bytes)

-- | Apply all mutations in one ST block. O(n) for one thaw/freeze, O(1) per mutation.
applyMutations :: [BufferMutation] -> SentPacketBuffer -> SentPacketBuffer
applyMutations [] buf = buf
applyMutations muts buf = runST $ do
  mvec <- V.thaw (spbEntries buf)
  deletions <- applyAll mvec muts 0
  vec' <- V.unsafeFreeze mvec
  return $! buf {spbEntries = vec', spbCount = spbCount buf - deletions}
  where
    applyAll _ [] !dels = return dels
    applyAll mvec (MutDelete idx : rest) !dels = do
      MV.unsafeWrite mvec idx RingEmpty
      applyAll mvec rest (dels + 1)
    applyAll mvec (MutNack idx record newNack : rest) !dels = do
      let seqNum = case V.unsafeIndex (spbEntries buf) idx of
            RingEntry s _ -> s
            RingEmpty -> SequenceNum 0 -- Should not happen
      MV.unsafeWrite mvec idx (RingEntry seqNum (record {sprNackCount = newNack}))
      applyAll mvec rest dels

updateRtt :: Double -> ReliableEndpoint -> ReliableEndpoint
updateRtt sampleMs ep
  | not (reHasRttSample ep) =
      let newSrtt = sampleMs
          newRttvar = sampleMs / 2.0
          newRto = clampRto (newSrtt + 4.0 * newRttvar)
       in ep
            { reSrtt = newSrtt,
              reRttvar = newRttvar,
              reRto = newRto,
              reHasRttSample = True
            }
  | otherwise =
      let newRttvar = (1.0 - rttBeta) * reRttvar ep + rttBeta * abs (sampleMs - reSrtt ep)
          newSrtt = (1.0 - rttAlpha) * reSrtt ep + rttAlpha * sampleMs
          newRto = clampRto (newSrtt + 4.0 * newRttvar)
       in ep
            { reSrtt = newSrtt,
              reRttvar = newRttvar,
              reRto = newRto
            }

clampRto :: Double -> Double
clampRto rto = max minRtoMs (min maxRtoMs rto)

recordLossSample :: Bool -> ReliableEndpoint -> ReliableEndpoint
recordLossSample lost ep =
  let idx = reLossWindowIndex ep `mod` lossWindowSize
      newWindow = lwSetBit idx lost (reLossWindow ep)
      newCount = min lossWindowSize (reLossWindowCount ep + 1)
   in ep
        { reLossWindow = newWindow,
          reLossWindowIndex = reLossWindowIndex ep + 1,
          reLossWindowCount = newCount
        }
{-# INLINE recordLossSample #-}

getAckInfo :: ReliableEndpoint -> (SequenceNum, Word64)
getAckInfo ep = (reRemoteSequence ep, reAckBits ep)
{-# INLINE getAckInfo #-}

rtoMs :: ReliableEndpoint -> Double
rtoMs = reRto
{-# INLINE rtoMs #-}

srttMs :: ReliableEndpoint -> Double
srttMs = reSrtt
{-# INLINE srttMs #-}

packetLossPercent :: ReliableEndpoint -> Float
packetLossPercent ep
  | count == 0 = 0.0
  | otherwise = fromIntegral lost / fromIntegral count
  where
    count = reLossWindowCount ep
    lost = lwCountLosses count (reLossWindow ep)
{-# INLINE packetLossPercent #-}

isInFlight :: SequenceNum -> ReliableEndpoint -> Bool
isInFlight seqNum ep = spbMember seqNum (reSentPackets ep)
{-# INLINE isInFlight #-}

packetsInFlight :: ReliableEndpoint -> Int
packetsInFlight = spbCount . reSentPackets
{-# INLINE packetsInFlight #-}
