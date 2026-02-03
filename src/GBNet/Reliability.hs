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

    -- * Sequence buffer
    SequenceBuffer,
    newSequenceBuffer,
    sbInsert,
    sbExists,
    sbGet,

    -- * Sent packet record
    SentPacketRecord (..),

    -- * Reliable endpoint
    ReliableEndpoint (..),
    AckResult,
    newReliableEndpoint,
    withMaxInFlight,
    nextSequence,
    onPacketSent,
    onPacketReceived,
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

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word16, Word32, Word64, Word8)
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

-- Monotonic time

-- | Monotonic time in nanoseconds.
type MonoTime = Word64

-- | Elapsed time in milliseconds.
elapsedMs :: MonoTime -> MonoTime -> Double
elapsedMs start now = fromIntegral (now - start) / 1e6

-- SequenceBuffer

-- | Circular buffer indexed by Word16 sequence numbers.
data SequenceBuffer a = SequenceBuffer
  { sbEntries :: !(Map Word16 (Word16, a)),
    sbSequence :: !Word16,
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

sbInsert :: Word16 -> a -> SequenceBuffer a -> SequenceBuffer a
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

clearRange :: Word16 -> Int -> SequenceBuffer a -> SequenceBuffer a
clearRange currentSeq diff buf = go 0 (sbEntries buf)
  where
    go i entries
      | i >= diff = buf {sbEntries = entries}
      | otherwise =
          let s = currentSeq + fromIntegral (i + 1)
              idx = fromIntegral s `mod` sbSize buf
           in go (i + 1) (Map.delete (fromIntegral idx) entries)

sbExists :: Word16 -> SequenceBuffer a -> Bool
sbExists seqNum buf =
  let idx = fromIntegral seqNum `mod` sbSize buf
   in case Map.lookup (fromIntegral idx) (sbEntries buf) of
        Just (storedSeq, _) -> storedSeq == seqNum
        Nothing -> False

sbGet :: Word16 -> SequenceBuffer a -> Maybe a
sbGet seqNum buf =
  let idx = fromIntegral seqNum `mod` sbSize buf
   in case Map.lookup (fromIntegral idx) (sbEntries buf) of
        Just (storedSeq, v) | storedSeq == seqNum -> Just v
        _ -> Nothing

-- SentPacketRecord

data SentPacketRecord = SentPacketRecord
  { sprChannelId :: !Word8,
    sprChannelSequence :: !Word16,
    sprSendTime :: !MonoTime,
    sprSize :: !Int,
    sprNackCount :: !Word8
  }
  deriving (Show)

-- ReliableEndpoint

-- | Result of processing ACKs: (acked pairs, fast retransmit candidates).
type AckResult = ([(Word8, Word16)], [(Word8, Word16)])

data ReliableEndpoint = ReliableEndpoint
  { reLocalSequence :: !Word16,
    reRemoteSequence :: !Word16,
    reAckBits :: !Word64,
    reSentPackets :: !(Map Word16 SentPacketRecord),
    reReceivedPackets :: !(SequenceBuffer Bool),
    reMaxSequenceDistance :: !Word16,
    reMaxInFlight :: !Int,
    reSrtt :: !Double,
    reRttvar :: !Double,
    reRto :: !Double,
    reHasRttSample :: !Bool,
    reLossWindow :: !(Vector Bool),
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
      reSentPackets = Map.empty,
      reReceivedPackets = newSequenceBuffer bufferSize,
      reMaxSequenceDistance = defaultMaxSequenceDistance,
      reMaxInFlight = defaultMaxInFlight,
      reSrtt = 0.0,
      reRttvar = 0.0,
      reRto = initialRtoMillis,
      reHasRttSample = False,
      reLossWindow = UV.replicate lossWindowSize False,
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

nextSequence :: ReliableEndpoint -> (Word16, ReliableEndpoint)
nextSequence ep =
  let s = reLocalSequence ep
   in (s, ep {reLocalSequence = s + 1})

onPacketSent ::
  Word16 ->
  MonoTime ->
  Word8 ->
  Word16 ->
  Int ->
  ReliableEndpoint ->
  ReliableEndpoint
onPacketSent seqNum sendTime channelId channelSeq size ep =
  let ep' =
        if Map.size (reSentPackets ep) >= reMaxInFlight ep
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
        { reSentPackets = Map.insert seqNum record (reSentPackets ep'),
          reTotalSent = reTotalSent ep' + 1,
          reBytesSent = reBytesSent ep' + fromIntegral size
        }

evictWorstInFlight :: ReliableEndpoint -> ReliableEndpoint
evictWorstInFlight ep =
  case Map.foldlWithKey' findOldest Nothing (reSentPackets ep) of
    Nothing -> ep
    Just (worstSeq, _) ->
      ep
        { reSentPackets = Map.delete worstSeq (reSentPackets ep),
          rePacketsEvicted = rePacketsEvicted ep + 1
        }
  where
    findOldest Nothing s r = Just (s, sprSendTime r)
    findOldest (Just (bestS, bestT)) s r
      | sprSendTime r < bestT = Just (s, sprSendTime r)
      | otherwise = Just (bestS, bestT)

onPacketReceived :: Word16 -> ReliableEndpoint -> ReliableEndpoint
onPacketReceived seqNum ep =
  let distance = fromIntegral (abs (sequenceDiff seqNum (reRemoteSequence ep))) :: Word32
   in if distance > fromIntegral (reMaxSequenceDistance ep)
        || sbExists seqNum (reReceivedPackets ep)
        then ep
        else
          let recvBuf = sbInsert seqNum True (reReceivedPackets ep)
              ep' = ep {reReceivedPackets = recvBuf}
           in if sequenceGreaterThan seqNum (reRemoteSequence ep')
                then
                  let diff = fromIntegral (sequenceDiff seqNum (reRemoteSequence ep')) :: Word64
                      newBits =
                        if diff <= fromIntegral ackBitsWindow
                          then
                            (reAckBits ep' `shiftL` fromIntegral diff)
                              .|. (1 `shiftL` (fromIntegral diff - 1))
                          else 0
                   in ep'
                        { reRemoteSequence = seqNum,
                          reAckBits = newBits
                        }
                else
                  let diff = fromIntegral (sequenceDiff (reRemoteSequence ep') seqNum) :: Word64
                   in if diff > 0 && diff <= fromIntegral ackBitsWindow
                        then ep' {reAckBits = reAckBits ep' .|. (1 `shiftL` (fromIntegral diff - 1))}
                        else ep'

processAcks :: Word16 -> Word64 -> MonoTime -> ReliableEndpoint -> (AckResult, ReliableEndpoint)
processAcks ackSeq ackBitsVal now ep =
  let directAck = [ackSeq | Map.member ackSeq (reSentPackets ep)]
      bitsAcks =
        [ ackSeq - (i + 1)
        | i <- [0 .. ackBitsWindow - 1],
          (ackBitsVal .&. (1 `shiftL` fromIntegral i)) /= 0,
          Map.member (ackSeq - (i + 1)) (reSentPackets ep)
        ]
      ackedSeqs = directAck ++ bitsAcks
      (acked, ep') = foldl (ackOneWithTime now) ([], ep) ackedSeqs
      inFlightSeqs = Map.keys (reSentPackets ep')
      (fastRetransmit, ep'') = foldl (nackOne ackSeq ackBitsVal) ([], ep') inFlightSeqs
   in ((acked, fastRetransmit), ep'')

ackOneWithTime ::
  MonoTime ->
  ([(Word8, Word16)], ReliableEndpoint) ->
  Word16 ->
  ([(Word8, Word16)], ReliableEndpoint)
ackOneWithTime now (pairs, ep) seqNum =
  case Map.lookup seqNum (reSentPackets ep) of
    Nothing -> (pairs, ep)
    Just record ->
      let rttSample = elapsedMs (sprSendTime record) now
          ep' =
            updateRtt rttSample $
              ep
                { reSentPackets = Map.delete seqNum (reSentPackets ep),
                  reTotalAcked = reTotalAcked ep + 1,
                  reBytesAcked = reBytesAcked ep + fromIntegral (sprSize record)
                }
          ep'' = recordLossSample False ep'
          pair = (sprChannelId record, sprChannelSequence record)
       in (pair : pairs, ep'')

nackOne ::
  Word16 ->
  Word64 ->
  ([(Word8, Word16)], ReliableEndpoint) ->
  Word16 ->
  ([(Word8, Word16)], ReliableEndpoint)
nackOne ackSeq ackBitsVal (retransmits, ep) seqNum
  | not (sequenceGreaterThan ackSeq seqNum) = (retransmits, ep)
  | otherwise =
      let diff = sequenceDiff ackSeq seqNum
       in if diff <= 0 || diff > fromIntegral ackBitsWindow
            then (retransmits, ep)
            else
              let bitIndex = fromIntegral diff - 1 :: Int
               in if (ackBitsVal .&. (1 `shiftL` bitIndex)) /= 0
                    then (retransmits, ep)
                    else case Map.lookup seqNum (reSentPackets ep) of
                      Nothing -> (retransmits, ep)
                      Just record ->
                        let newNack = min 255 (sprNackCount record + 1)
                            record' = record {sprNackCount = newNack}
                            ep' = ep {reSentPackets = Map.insert seqNum record' (reSentPackets ep)}
                            retransmits' =
                              if newNack == fastRetransmitThreshold
                                then (sprChannelId record, sprChannelSequence record) : retransmits
                                else retransmits
                         in (retransmits', ep')

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
      newWindow = reLossWindow ep UV.// [(idx, lost)]
      newCount = min lossWindowSize (reLossWindowCount ep + 1)
   in ep
        { reLossWindow = newWindow,
          reLossWindowIndex = reLossWindowIndex ep + 1,
          reLossWindowCount = newCount
        }

getAckInfo :: ReliableEndpoint -> (Word16, Word64)
getAckInfo ep = (reRemoteSequence ep, reAckBits ep)

rtoMs :: ReliableEndpoint -> Double
rtoMs = reRto

srttMs :: ReliableEndpoint -> Double
srttMs = reSrtt

packetLossPercent :: ReliableEndpoint -> Float
packetLossPercent ep
  | reLossWindowCount ep == 0 = 0.0
  | otherwise =
      let count = reLossWindowCount ep
          lost = UV.length $ UV.filter id $ UV.take count (reLossWindow ep)
       in fromIntegral lost / fromIntegral count

isInFlight :: Word16 -> ReliableEndpoint -> Bool
isInFlight seqNum ep = Map.member seqNum (reSentPackets ep)

packetsInFlight :: ReliableEndpoint -> Int
packetsInFlight = Map.size . reSentPackets
