{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
    SBEntry (..),
    SequenceBuffer,
    newSequenceBuffer,
    sbInsert,
    sbInsertMany,
    sbExists,
    sbGet,

    -- * Received packet buffer (opaque)
    ReceivedBuffer,

    -- * Sent packet buffer (opaque)
    RingEntry,
    SentPacketBuffer,

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
import Data.Bits (complement, popCount, shiftL, (.&.), (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Class (MonoTime (..))
import GBNet.Types (ChannelId (..), SequenceNum (..))
import GBNet.Util (sequenceDiff, sequenceGreaterThan)
import GBNet.ZeroCopy (zeroCopyMutate, zeroCopyMutate', zeroCopyMutateU')
import Optics ((%~), (&), (.~))
import Optics.TH (makeFieldLabelsNoPrefix)

-- Constants

-- | Initial retransmission timeout in milliseconds (before any RTT samples).
initialRtoMillis :: Double
initialRtoMillis = 100.0

-- | Number of ACK bits tracked per packet (64-bit bitfield).
ackBitsWindow :: Word16
ackBitsWindow = 64

-- | Jacobson\/Karels SRTT smoothing factor (1\/8).
rttAlpha :: Double
rttAlpha = 0.125

-- | Jacobson\/Karels RTTVAR smoothing factor (1\/4).
rttBeta :: Double
rttBeta = 0.25

-- | Minimum retransmission timeout in milliseconds.
minRtoMs :: Double
minRtoMs = 50.0

-- | Maximum retransmission timeout in milliseconds.
maxRtoMs :: Double
maxRtoMs = 2000.0

-- | Rolling loss window size (number of samples tracked).
lossWindowSize :: Int
lossWindowSize = 256

-- | Number of NACKs before triggering fast retransmit.
fastRetransmitThreshold :: Word8
fastRetransmitThreshold = 3

-- | Default maximum sequence distance before treating packets as stale.
defaultMaxSequenceDistance :: Word16
defaultMaxSequenceDistance = 32768

-- | Default maximum packets in flight.
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

makeFieldLabelsNoPrefix ''LossWindow

instance NFData LossWindow where
  rnf (LossWindow b0 b1 b2 b3) = rnf b0 `seq` rnf b1 `seq` rnf b2 `seq` rnf b3

-- | Empty loss window (all successful).
emptyLossWindow :: LossWindow
emptyLossWindow = LossWindow 0 0 0 0

-- | Set a bit in the loss window. Index must be 0-255.
lwSetBit :: Int -> Bool -> LossWindow -> LossWindow
lwSetBit idx val lw
  | idx < 64 = lw & #lwBits0 %~ setBitWord64 (idx .&. 63) val
  | idx < 128 = lw & #lwBits1 %~ setBitWord64 (idx .&. 63) val
  | idx < 192 = lw & #lwBits2 %~ setBitWord64 (idx .&. 63) val
  | otherwise = lw & #lwBits3 %~ setBitWord64 (idx .&. 63) val
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
elapsedMs start now = fromIntegral (unMonoTime (now - start)) / 1e6
{-# INLINE elapsedMs #-}

-- SequenceBuffer

-- | Ring buffer entry. Stores sequence number for validation.
data SBEntry a
  = SBEmpty
  | SBEntry !SequenceNum !a
  deriving (Show)

instance (NFData a) => NFData (SBEntry a) where
  rnf SBEmpty = ()
  rnf (SBEntry s a) = rnf s `seq` rnf a

-- | High-performance circular buffer using Vector ring buffer.
-- O(1) insert, O(1) lookup, O(1) exists via zero-copy mutation.
-- Size must be power of 2 for fast bitwise modulo.
data SequenceBuffer a = SequenceBuffer
  { sbEntries :: !(V.Vector (SBEntry a)),
    sbSequence :: !SequenceNum,
    sbSize :: !Int -- Keep for API compat, but use mask internally
  }
  deriving (Show)

instance (NFData a) => NFData (SequenceBuffer a) where
  rnf (SequenceBuffer e s sz) = rnf e `seq` rnf s `seq` rnf sz

makeFieldLabelsNoPrefix ''SequenceBuffer

-- | Create a new sequence buffer. Size is rounded up to power of 2.
newSequenceBuffer :: Int -> SequenceBuffer a
newSequenceBuffer requestedSize =
  let size = nextPowerOf2 requestedSize
   in SequenceBuffer
        { sbEntries = V.replicate size SBEmpty,
          sbSequence = 0,
          sbSize = size
        }

-- | Round up to next power of 2.
nextPowerOf2 :: Int -> Int
nextPowerOf2 n
  | n <= 1 = 1
  | otherwise = go 1
  where
    go p
      | p >= n = p
      | otherwise = go (p * 2)
{-# INLINE nextPowerOf2 #-}

-- | O(1) insert using zero-copy mutation.
sbInsert :: SequenceNum -> a -> SequenceBuffer a -> SequenceBuffer a
sbInsert seqNum val buf =
  let !idx = seqToIndex seqNum buf
      newHighest
        | sequenceGreaterThan seqNum (sbSequence buf) = seqNum
        | otherwise = sbSequence buf
      mutated = zeroCopyMutate (sbEntries buf) $ \mv ->
        MV.unsafeWrite mv idx (SBEntry seqNum val)
   in buf & #sbEntries .~ mutated & #sbSequence .~ newHighest
{-# INLINE sbInsert #-}

-- | Batched insert - O(n) with single thaw/freeze for n entries.
sbInsertMany :: [(SequenceNum, a)] -> SequenceBuffer a -> SequenceBuffer a
sbInsertMany [] buf = buf
sbInsertMany items buf =
  let (mutated, newHighest) = zeroCopyMutate' (sbEntries buf) $ \mv ->
        let go _ !highest [] = return highest
            go vec !highest ((seqNum, val) : rest) = do
              let !idx = seqToIndex seqNum buf
              MV.unsafeWrite vec idx (SBEntry seqNum val)
              let !next =
                    if sequenceGreaterThan seqNum highest then seqNum else highest
              go vec next rest
         in go mv (sbSequence buf) items
   in buf & #sbEntries .~ mutated & #sbSequence .~ newHighest
{-# INLINE sbInsertMany #-}

-- | O(1) existence check via direct index.
sbExists :: SequenceNum -> SequenceBuffer a -> Bool
sbExists seqNum buf =
  case V.unsafeIndex (sbEntries buf) (seqToIndex seqNum buf) of
    SBEntry storedSeq _ -> storedSeq == seqNum
    SBEmpty -> False
{-# INLINE sbExists #-}

-- | O(1) lookup via direct index.
sbGet :: SequenceNum -> SequenceBuffer a -> Maybe a
sbGet seqNum buf =
  case V.unsafeIndex (sbEntries buf) (seqToIndex seqNum buf) of
    SBEntry storedSeq v
      | storedSeq == seqNum -> Just v
    _ -> Nothing
{-# INLINE sbGet #-}

-- | Fast index using bitwise AND (size must be power of 2).
seqToIndex :: SequenceNum -> SequenceBuffer a -> Int
seqToIndex (SequenceNum s) buf = fromIntegral s .&. (sbSize buf - 1)
{-# INLINE seqToIndex #-}

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
-- 512 bytes total (256 x Word16), cache-friendly contiguous memory.
data ReceivedBuffer = ReceivedBuffer
  { rbSeqs :: !(VU.Vector Word16), -- Unboxed: contiguous like C array
    rbHighest :: !SequenceNum -- Track highest for ack bit calculation
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''ReceivedBuffer

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

-- | Batched insert - O(n) for n sequence numbers via zero-copy mutation.
rbInsertMany :: [SequenceNum] -> ReceivedBuffer -> ReceivedBuffer
rbInsertMany [] buf = buf
rbInsertMany seqs buf =
  let (mutated, newHighest) = zeroCopyMutateU' (rbSeqs buf) $ \mv ->
        go mv (rbHighest buf) seqs
   in ReceivedBuffer mutated newHighest
  where
    go _ !highest [] = return highest
    go mv !highest (sn@(SequenceNum s) : rest) = do
      VUM.unsafeWrite mv (fromIntegral s .&. receivedBufferMask) s
      let next = if sequenceGreaterThan sn highest then sn else highest
      go mv next rest
{-# INLINE rbInsertMany #-}

-- SentPacketRecord

-- | Record of a sent packet, tracked for ACK processing and RTT estimation.
data SentPacketRecord = SentPacketRecord
  { sprChannelId :: {-# UNPACK #-} !ChannelId,
    sprChannelSequence :: {-# UNPACK #-} !SequenceNum,
    sprSendTime :: {-# UNPACK #-} !MonoTime,
    sprSize :: {-# UNPACK #-} !Int,
    sprNackCount :: {-# UNPACK #-} !Word8
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''SentPacketRecord

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

makeFieldLabelsNoPrefix ''SentPacketBuffer

instance NFData SentPacketBuffer where
  rnf (SentPacketBuffer e c) = rnf e `seq` rnf c

-- | Buffer size (fixed at 256 for Word16 sequence space efficiency).
ringBufferSize :: Int
ringBufferSize = 256
{-# INLINE ringBufferSize #-}

-- | Create empty sent packet buffer. Fixed at 'ringBufferSize' (256) entries.
newSentPacketBuffer :: SentPacketBuffer
newSentPacketBuffer =
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

-- | O(1) insert using zero-copy mutation.
spbInsert :: SequenceNum -> SentPacketRecord -> SentPacketBuffer -> SentPacketBuffer
spbInsert seqNum record buf =
  let idx = seqToIdx seqNum
      (mutated, countDelta) = zeroCopyMutate' (spbEntries buf) $ \mvec -> do
        old <- MV.unsafeRead mvec idx
        MV.unsafeWrite mvec idx (RingEntry seqNum record)
        return $! case old of
          RingEmpty -> 1 :: Int
          _ -> 0
   in buf & #spbEntries .~ mutated & #spbCount %~ (+ countDelta)
{-# INLINE spbInsert #-}

-- | O(1) delete by sequence number.
spbDelete :: SequenceNum -> SentPacketBuffer -> SentPacketBuffer
spbDelete seqNum buf =
  case V.unsafeIndex (spbEntries buf) idx of
    RingEntry storedSeq _
      | storedSeq == seqNum ->
          let cleared = zeroCopyMutate (spbEntries buf) $ \mvec ->
                MV.unsafeWrite mvec idx RingEmpty
           in buf & #spbEntries .~ cleared & #spbCount %~ subtract 1
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

makeFieldLabelsNoPrefix ''AckResult

instance NFData AckResult where
  rnf (AckResult a f) = rnf a `seq` rnf f

-- | Core reliability state: sequence tracking, RTT estimation, loss detection,
-- and in-flight packet management.
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

instance NFData ReliableEndpoint where
  rnf (ReliableEndpoint ls rs ab sp rp msd mif srtt rv rto hrs lw lwi lwc ts ta tl pe bs ba) =
    rnf ls `seq`
      rnf rs `seq`
        rnf ab `seq`
          rnf sp `seq`
            rnf rp `seq`
              rnf msd `seq`
                rnf mif `seq`
                  rnf srtt `seq`
                    rnf rv `seq`
                      rnf rto `seq`
                        rnf hrs `seq`
                          rnf lw `seq`
                            rnf lwi `seq`
                              rnf lwc `seq`
                                rnf ts `seq`
                                  rnf ta `seq`
                                    rnf tl `seq`
                                      rnf pe `seq`
                                        rnf bs `seq`
                                          rnf ba

makeFieldLabelsNoPrefix ''ReliableEndpoint

-- | Create a new reliable endpoint.
newReliableEndpoint :: ReliableEndpoint
newReliableEndpoint =
  ReliableEndpoint
    { reLocalSequence = 0,
      reRemoteSequence = 0,
      reAckBits = 0,
      reSentPackets = newSentPacketBuffer,
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

-- | Override the maximum in-flight packet count.
withMaxInFlight :: Int -> ReliableEndpoint -> ReliableEndpoint
withMaxInFlight maxFlight ep = ep & #reMaxInFlight .~ maxFlight

-- | Allocate the next local sequence number.
nextSequence :: ReliableEndpoint -> (SequenceNum, ReliableEndpoint)
nextSequence ep =
  let s = reLocalSequence ep
   in (s, ep & #reLocalSequence .~ (s + 1))

-- | Record a sent packet for in-flight tracking and future ACK processing.
onPacketSent ::
  SequenceNum ->
  MonoTime ->
  ChannelId ->
  SequenceNum ->
  Int ->
  ReliableEndpoint ->
  ReliableEndpoint
onPacketSent seqNum sendTime channelId channelSeq size ep =
  let withRoom =
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
   in withRoom
        & #reSentPackets
        %~ spbInsert seqNum record
        & #reTotalSent
        %~ (+ 1)
        & #reBytesSent
        %~ (+ fromIntegral size)

evictWorstInFlight :: ReliableEndpoint -> ReliableEndpoint
evictWorstInFlight ep =
  case spbFindOldest (reSentPackets ep) of
    Nothing -> ep
    Just (worstSeq, _) ->
      ep
        & #reSentPackets
        %~ spbDelete worstSeq
        & #rePacketsEvicted
        %~ (+ 1)

-- | Process received packets - processes multiple sequence numbers with ONE thaw/freeze.
-- This is the high-performance API for game loops processing many packets per frame.
onPacketsReceived :: [SequenceNum] -> ReliableEndpoint -> ReliableEndpoint
onPacketsReceived seqNums ep =
  let validSeqs = filter isValid seqNums
      newBuf = rbInsertMany validSeqs (reReceivedPackets ep)
      (newRemote, newAckBits) = foldl' updateAckState (reRemoteSequence ep, reAckBits ep) validSeqs
   in ep & #reReceivedPackets .~ newBuf & #reRemoteSequence .~ newRemote & #reAckBits .~ newAckBits
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
  | MutNack !Int !SequenceNum !SentPacketRecord !Word8 -- idx, seqNum, record, newNack

-- | Process ACKs from received packet. O(ackBitsWindow) = O(64).
-- Two-phase: pure lookup pass, then single batched mutation.
processAcks :: SequenceNum -> Word64 -> MonoTime -> ReliableEndpoint -> (AckResult, ReliableEndpoint)
processAcks ackSeq ackBitsVal now ep =
  let buf = reSentPackets ep
      -- Phase 1: Pure lookup, collect decisions
      (acked, retrans, mutations, rttSum, rttCount, bytesAcked) =
        foldl' (processOne buf) ([], [], [], 0.0, 0 :: Int, 0) [0 .. fromIntegral ackBitsWindow]
      -- Phase 2: Apply all mutations in one ST block
      mutatedBuf = applyMutations mutations buf
      -- Update RTT with average of all samples
      withRtt = case rttCount of
        0 -> ep
        _ -> updateRtt (rttSum / fromIntegral rttCount) ep
      -- Record loss samples for each ack (success = not lost)
      withLoss = foldl' (\e _ -> recordLossSample False e) withRtt acked
   in ( AckResult acked retrans,
        withLoss
          & #reSentPackets
          .~ mutatedBuf
          & #reTotalAcked
          .~ (reTotalAcked ep + fromIntegral (length acked))
          & #reBytesAcked
          .~ (reBytesAcked ep + fromIntegral bytesAcked)
      )
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
                  updatedRetrans =
                    if newNack == fastRetransmitThreshold
                      then (sprChannelId record, sprChannelSequence record) : retrans
                      else retrans
               in (acked, updatedRetrans, MutNack idx seqNum record newNack : muts, rttSum, rttCnt, bytes)

-- | Apply all mutations in one ST block via zero-copy mutation.
applyMutations :: [BufferMutation] -> SentPacketBuffer -> SentPacketBuffer
applyMutations [] buf = buf
applyMutations muts buf =
  let (mutated, deletions) = zeroCopyMutate' (spbEntries buf) $ \mvec ->
        applyAll mvec muts 0
   in buf & #spbEntries .~ mutated & #spbCount %~ subtract deletions
  where
    applyAll _ [] !dels = return dels
    applyAll mvec (MutDelete idx : rest) !dels = do
      MV.unsafeWrite mvec idx RingEmpty
      applyAll mvec rest (dels + 1)
    applyAll mvec (MutNack idx seqNum record newNack : rest) !dels = do
      MV.unsafeWrite mvec idx (RingEntry seqNum (record & #sprNackCount .~ newNack))
      applyAll mvec rest dels

-- | Update SRTT and RTO using Jacobson\/Karels algorithm.
updateRtt :: Double -> ReliableEndpoint -> ReliableEndpoint
updateRtt sampleMs ep
  | not (reHasRttSample ep) =
      let newSrtt = sampleMs
          newRttvar = sampleMs / 2.0
          newRto = clampRto (newSrtt + 4.0 * newRttvar)
       in ep
            & #reSrtt
            .~ newSrtt
            & #reRttvar
            .~ newRttvar
            & #reRto
            .~ newRto
            & #reHasRttSample
            .~ True
  | otherwise =
      let newRttvar = (1.0 - rttBeta) * reRttvar ep + rttBeta * abs (sampleMs - reSrtt ep)
          newSrtt = (1.0 - rttAlpha) * reSrtt ep + rttAlpha * sampleMs
          newRto = clampRto (newSrtt + 4.0 * newRttvar)
       in ep
            & #reSrtt
            .~ newSrtt
            & #reRttvar
            .~ newRttvar
            & #reRto
            .~ newRto

clampRto :: Double -> Double
clampRto rto = max minRtoMs (min maxRtoMs rto)

-- | Record a loss\/success sample in the rolling loss window.
recordLossSample :: Bool -> ReliableEndpoint -> ReliableEndpoint
recordLossSample lost ep =
  let idx = reLossWindowIndex ep `mod` lossWindowSize
      newWindow = lwSetBit idx lost (reLossWindow ep)
      newCount = min lossWindowSize (reLossWindowCount ep + 1)
   in ep
        & #reLossWindow
        .~ newWindow
        & #reLossWindowIndex
        %~ (+ 1)
        & #reLossWindowCount
        .~ newCount
{-# INLINE recordLossSample #-}

-- | Get the current ACK sequence and bitfield for outgoing packet headers.
getAckInfo :: ReliableEndpoint -> (SequenceNum, Word64)
getAckInfo ep = (reRemoteSequence ep, reAckBits ep)
{-# INLINE getAckInfo #-}

-- | Current retransmission timeout in milliseconds.
rtoMs :: ReliableEndpoint -> Double
rtoMs = reRto
{-# INLINE rtoMs #-}

-- | Smoothed round-trip time in milliseconds.
srttMs :: ReliableEndpoint -> Double
srttMs = reSrtt
{-# INLINE srttMs #-}

-- | Current packet loss as a fraction (0.0 to 1.0).
packetLossPercent :: ReliableEndpoint -> Double
packetLossPercent ep
  | count == 0 = 0.0
  | otherwise = fromIntegral lost / fromIntegral count
  where
    count = reLossWindowCount ep
    lost = lwCountLosses count (reLossWindow ep)
{-# INLINE packetLossPercent #-}

-- | Check whether a sequence number is currently in flight.
isInFlight :: SequenceNum -> ReliableEndpoint -> Bool
isInFlight seqNum ep = spbMember seqNum (reSentPackets ep)
{-# INLINE isInFlight #-}

-- | Number of packets currently in flight.
packetsInFlight :: ReliableEndpoint -> Int
packetsInFlight = spbCount . reSentPackets
{-# INLINE packetsInFlight #-}
