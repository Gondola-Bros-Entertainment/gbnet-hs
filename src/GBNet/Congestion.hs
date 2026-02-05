-- |
-- Module      : GBNet.Congestion
-- Description : Binary congestion control and bandwidth tracking
--
-- Gaffer-style Good/Bad mode congestion control, byte-budget gating,
-- adaptive recovery timer, message batching, and bandwidth tracking.
module GBNet.Congestion
  ( -- * Constants
    congestionRateReduction,
    minSendRate,
    batchHeaderSize,
    batchLengthSize,
    maxBatchMessages,
    initialCwndPackets,
    minCwndBytes,
    minRecoverySecs,
    maxRecoverySecs,
    recoveryHalveIntervalSecs,
    quickDropThresholdSecs,
    initialSsthresh,

    -- * Congestion mode
    CongestionMode (..),
    CongestionPhase (..),

    -- * Binary congestion controller
    CongestionController (..),
    newCongestionController,
    ccRefillBudget,
    ccDeductBudget,
    ccUpdate,
    ccCanSend,

    -- * Congestion level query
    ccCongestionLevel,
    cwCongestionLevel,

    -- * Window-based congestion controller
    CongestionWindow (..),
    newCongestionWindow,
    cwOnAck,
    cwOnLoss,
    cwExitRecovery,
    cwOnSend,
    cwCanSend,
    cwUpdatePacing,
    cwCanSendPaced,
    cwSlowStartRestart,

    -- * Bandwidth tracking
    BandwidthTracker (..),
    newBandwidthTracker,
    btRecord,
    btBytesPerSecond,

    -- * Message batching
    batchMessages,
    unbatchMessages,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64, Word8)
import GBNet.Reliability (MonoTime, elapsedMs)
import GBNet.Stats (CongestionLevel (..))

-- Constants

congestionRateReduction :: Float
congestionRateReduction = 0.5

minSendRate :: Float
minSendRate = 1.0

batchHeaderSize :: Int
batchHeaderSize = 1

batchLengthSize :: Int
batchLengthSize = 2

maxBatchMessages :: Word8
maxBatchMessages = 255

initialCwndPackets :: Int
initialCwndPackets = 10

minCwndBytes :: Int
minCwndBytes = 1200

minRecoverySecs :: Double
minRecoverySecs = 1.0

maxRecoverySecs :: Double
maxRecoverySecs = 60.0

recoveryHalveIntervalSecs :: Double
recoveryHalveIntervalSecs = 10.0

quickDropThresholdSecs :: Double
quickDropThresholdSecs = 10.0

-- | Initial slow-start threshold (IEEE 754 positive infinity).
initialSsthresh :: Double
initialSsthresh = 1 / 0

-- | Additive increase per update tick when in Good mode (packets/sec).
sendRateIncrease :: Float
sendRateIncrease = 1.0

-- | Maximum send rate multiplier relative to base rate.
maxSendRateMultiplier :: Float
maxSendRateMultiplier = 4.0

-- | Binary congestion state.
data CongestionMode
  = CongestionGood
  | CongestionBad
  deriving (Eq, Show)

-- | Phase for window-based congestion control.
data CongestionPhase
  = SlowStart
  | Avoidance
  | Recovery
  deriving (Eq, Show)

-- | Binary congestion controller (Gaffer-style).
data CongestionController = CongestionController
  { ccMode :: !CongestionMode,
    ccGoodConditionsStart :: !(Maybe MonoTime),
    ccLossThreshold :: !Float,
    ccRttThresholdMs :: !Float,
    ccBaseSendRate :: !Float,
    ccCurrentSendRate :: !Float,
    ccBudgetBytesRemaining :: !Int,
    ccBytesPerTick :: !Int,
    ccAdaptiveRecoverySecs :: !Double,
    ccLastGoodEntry :: !(Maybe MonoTime),
    ccLastBadEntry :: !(Maybe MonoTime)
  }
  deriving (Show)

-- | Create a new congestion controller.
newCongestionController :: Float -> Float -> Float -> Double -> CongestionController
newCongestionController baseSendRate lossThreshold rttThresholdMs recoveryTimeMs =
  CongestionController
    { ccMode = CongestionGood,
      ccGoodConditionsStart = Nothing,
      ccLossThreshold = lossThreshold,
      ccRttThresholdMs = rttThresholdMs,
      ccBaseSendRate = baseSendRate,
      ccCurrentSendRate = baseSendRate,
      ccBudgetBytesRemaining = 0,
      ccBytesPerTick = 0,
      ccAdaptiveRecoverySecs = recoveryTimeMs / 1000.0,
      ccLastGoodEntry = Nothing,
      ccLastBadEntry = Nothing
    }

-- | Refill the byte budget at the start of each tick.
ccRefillBudget :: Int -> CongestionController -> CongestionController
ccRefillBudget mtu cc =
  let bytesPerTick = floor (ccCurrentSendRate cc * fromIntegral mtu)
   in cc
        { ccBytesPerTick = bytesPerTick,
          ccBudgetBytesRemaining = bytesPerTick
        }

-- | Deduct bytes from the send budget.
ccDeductBudget :: Int -> CongestionController -> CongestionController
ccDeductBudget bytes cc =
  cc {ccBudgetBytesRemaining = ccBudgetBytesRemaining cc - bytes}
{-# INLINE ccDeductBudget #-}

-- | Update congestion state based on current network conditions.
ccUpdate :: Float -> Float -> MonoTime -> CongestionController -> CongestionController
ccUpdate packetLoss rttMs now cc =
  let isBad = packetLoss > ccLossThreshold cc || rttMs > ccRttThresholdMs cc
   in case ccMode cc of
        CongestionGood
          | isBad ->
              -- Quick re-entry to Bad doubles recovery timer
              let recoveryMult = case ccLastGoodEntry cc of
                    Just goodEntry ->
                      if elapsedMs goodEntry now < quickDropThresholdSecs * 1000.0
                        then 2.0
                        else 1.0
                    Nothing -> 1.0
                  newRecovery = min maxRecoverySecs (ccAdaptiveRecoverySecs cc * recoveryMult)
                  newRate = max minSendRate (ccCurrentSendRate cc * congestionRateReduction)
               in cc
                    { ccMode = CongestionBad,
                      ccLastBadEntry = Just now,
                      ccCurrentSendRate = newRate,
                      ccGoodConditionsStart = Nothing,
                      ccAdaptiveRecoverySecs = newRecovery
                    }
          | otherwise ->
              -- Additive increase: ramp rate up toward max capacity
              let maxRate = ccBaseSendRate cc * maxSendRateMultiplier
                  newRate = min maxRate (ccCurrentSendRate cc + sendRateIncrease)
                  -- Halve recovery time after sustained good conditions
                  cc' = cc {ccCurrentSendRate = newRate}
               in case ccLastGoodEntry cc' of
                    Just goodEntry ->
                      let elapsed = elapsedMs goodEntry now / 1000.0
                          intervals = floor (elapsed / recoveryHalveIntervalSecs) :: Int
                       in if intervals > 0
                            then
                              let newRecovery =
                                    max
                                      minRecoverySecs
                                      (ccAdaptiveRecoverySecs cc' / (2.0 ^ intervals))
                               in cc'
                                    { ccAdaptiveRecoverySecs = newRecovery,
                                      ccLastGoodEntry = Just now
                                    }
                            else cc'
                    Nothing -> cc'
        CongestionBad
          | not isBad ->
              case ccGoodConditionsStart cc of
                Nothing ->
                  cc {ccGoodConditionsStart = Just now}
                Just start ->
                  let requiredMs = ccAdaptiveRecoverySecs cc * 1000.0
                   in if elapsedMs start now >= requiredMs
                        then
                          cc
                            { ccMode = CongestionGood,
                              ccLastGoodEntry = Just now,
                              ccCurrentSendRate = ccBaseSendRate cc,
                              ccGoodConditionsStart = Nothing
                            }
                        else cc
          | otherwise ->
              cc {ccGoodConditionsStart = Nothing}

-- | Check if a packet can be sent given packets sent and size.
ccCanSend :: Int -> Int -> CongestionController -> Bool
ccCanSend packetsSentThisCycle packetBytes cc =
  fromIntegral packetsSentThisCycle < ccCurrentSendRate cc
    && ccBudgetBytesRemaining cc >= packetBytes
{-# INLINE ccCanSend #-}

-- | Query congestion level from the binary controller.
ccCongestionLevel :: CongestionController -> CongestionLevel
ccCongestionLevel cc = case ccMode cc of
  CongestionBad -> CongestionCritical
  CongestionGood
    | budgetRatio < budgetElevatedThreshold -> CongestionElevated
    | otherwise -> CongestionNone
  where
    budgetRatio
      | ccBytesPerTick cc <= 0 = 1.0
      | otherwise = fromIntegral (ccBudgetBytesRemaining cc) / fromIntegral (ccBytesPerTick cc) :: Float
    budgetElevatedThreshold = 0.25

-- | Query congestion level from the window-based controller.
cwCongestionLevel :: CongestionWindow -> CongestionLevel
cwCongestionLevel cw
  | utilization > windowCriticalThreshold = CongestionCritical
  | utilization > windowHighThreshold = CongestionHigh
  | utilization > windowElevatedThreshold = CongestionElevated
  | otherwise = CongestionNone
  where
    utilization
      | cwCwnd cw <= 0 = 1.0
      | otherwise = fromIntegral (cwBytesInFlight cw) / cwCwnd cw
    windowElevatedThreshold = 0.7
    windowHighThreshold = 0.85
    windowCriticalThreshold = 0.95

-- | Window-based congestion controller (TCP-like).
data CongestionWindow = CongestionWindow
  { cwPhase :: !CongestionPhase,
    cwCwnd :: !Double,
    cwSsthresh :: !Double,
    cwBytesInFlight :: !Word64,
    cwMtu :: !Int,
    cwLastSendTime :: !(Maybe MonoTime),
    cwMinInterPacketDelay :: !Double -- milliseconds
  }
  deriving (Show)

-- | Create a new congestion window.
newCongestionWindow :: Int -> CongestionWindow
newCongestionWindow mtu =
  CongestionWindow
    { cwPhase = SlowStart,
      cwCwnd = fromIntegral (initialCwndPackets * mtu),
      cwSsthresh = initialSsthresh,
      cwBytesInFlight = 0,
      cwMtu = mtu,
      cwLastSendTime = Nothing,
      cwMinInterPacketDelay = 0.0
    }

-- | Called when bytes are acknowledged.
cwOnAck :: Int -> CongestionWindow -> CongestionWindow
cwOnAck bytes cw =
  let cw' = cw {cwBytesInFlight = cwBytesInFlight cw - fromIntegral (min bytes (fromIntegral (cwBytesInFlight cw)))}
   in case cwPhase cw' of
        SlowStart ->
          let newCwnd = cwCwnd cw' + fromIntegral bytes
           in if newCwnd >= cwSsthresh cw'
                then cw' {cwCwnd = newCwnd, cwPhase = Avoidance}
                else cw' {cwCwnd = newCwnd}
        Avoidance
          | cwCwnd cw' > 0 ->
              -- Additive increase: cwnd += mtu * bytes / cwnd
              let increase = fromIntegral (cwMtu cw') * fromIntegral bytes / cwCwnd cw'
               in cw' {cwCwnd = cwCwnd cw' + increase}
          | otherwise -> cw'
        Recovery ->
          cw' -- Conservative in recovery

-- | Called on packet loss detection.
cwOnLoss :: CongestionWindow -> CongestionWindow
cwOnLoss cw =
  let newSsthresh = max (fromIntegral minCwndBytes) (cwCwnd cw / 2.0)
   in cw
        { cwSsthresh = newSsthresh,
          cwCwnd = newSsthresh,
          cwPhase = Recovery
        }

-- | Exit recovery and return to avoidance.
cwExitRecovery :: CongestionWindow -> CongestionWindow
cwExitRecovery cw
  | cwPhase cw == Recovery = cw {cwPhase = Avoidance}
  | otherwise = cw

-- | Record bytes sent.
cwOnSend :: Int -> MonoTime -> CongestionWindow -> CongestionWindow
cwOnSend bytes now cw =
  cw
    { cwBytesInFlight = cwBytesInFlight cw + fromIntegral bytes,
      cwLastSendTime = Just now
    }

-- | Check if a packet can be sent.
cwCanSend :: Int -> CongestionWindow -> Bool
cwCanSend packetBytes cw =
  cwBytesInFlight cw + fromIntegral packetBytes <= floor (cwCwnd cw)
{-# INLINE cwCanSend #-}

-- | Update pacing delay from cwnd and RTT.
cwUpdatePacing :: Double -> CongestionWindow -> CongestionWindow
cwUpdatePacing rttMs cw
  | cwCwnd cw > 0 && rttMs > 0 =
      let packetsInWindow = cwCwnd cw / fromIntegral (cwMtu cw)
          delay =
            if packetsInWindow > 0
              then rttMs / packetsInWindow
              else 0.0
       in cw {cwMinInterPacketDelay = delay}
  | otherwise = cw

-- | Check if enough time has elapsed for pacing.
cwCanSendPaced :: MonoTime -> CongestionWindow -> Bool
cwCanSendPaced now cw =
  case cwLastSendTime cw of
    Nothing -> True
    Just lastSend -> elapsedMs lastSend now >= cwMinInterPacketDelay cw
{-# INLINE cwCanSendPaced #-}

-- | Reset to slow start if idle too long (RFC 2861).
-- Prevents bursting a stale window after an idle period.
cwSlowStartRestart :: Double -> MonoTime -> CongestionWindow -> CongestionWindow
cwSlowStartRestart rtoMs now cw =
  case cwLastSendTime cw of
    Nothing -> cw
    Just lastSend
      | elapsedMs lastSend now > ssrIdleThreshold * rtoMs ->
          let initialCwnd = fromIntegral (initialCwndPackets * cwMtu cw)
           in cw
                { cwPhase = SlowStart,
                  cwCwnd = initialCwnd,
                  cwSsthresh = cwCwnd cw
                }
      | otherwise -> cw
  where
    -- Idle for more than 2 RTOs triggers restart
    ssrIdleThreshold = 2.0

-- | Bandwidth tracker using a sliding window with cached byte total.
data BandwidthTracker = BandwidthTracker
  { btWindow :: !(Seq (MonoTime, Int)),
    btWindowDurationMs :: !Double,
    -- | Cached running total of bytes in window
    btTotalBytes :: !Int
  }
  deriving (Show)

-- | Create a new bandwidth tracker.
newBandwidthTracker :: Double -> BandwidthTracker
newBandwidthTracker windowDurationMs =
  BandwidthTracker
    { btWindow = Seq.empty,
      btWindowDurationMs = windowDurationMs,
      btTotalBytes = 0
    }

-- | Record bytes at the given time.
btRecord :: Int -> MonoTime -> BandwidthTracker -> BandwidthTracker
btRecord bytes now bt =
  let bt' = bt {btWindow = btWindow bt Seq.|> (now, bytes), btTotalBytes = btTotalBytes bt + bytes}
   in btCleanup now bt'

-- | Get bytes per second.
btBytesPerSecond :: BandwidthTracker -> Double
btBytesPerSecond bt
  | Seq.null (btWindow bt) = 0.0
  | otherwise =
      let elapsedSecs = btWindowDurationMs bt / 1000.0
       in if elapsedSecs > 0
            then fromIntegral (btTotalBytes bt) / elapsedSecs
            else 0.0

-- | Clean up old entries, maintaining the cached byte total.
btCleanup :: MonoTime -> BandwidthTracker -> BandwidthTracker
btCleanup now bt =
  let cutoffMs = btWindowDurationMs bt
      isStale (t, _) = elapsedMs t now >= cutoffMs
      -- Drop stale entries from the front, subtracting their bytes
      (stale, recent) = Seq.spanl isStale (btWindow bt)
      evictedBytes = sum $ snd <$> stale
   in bt {btWindow = recent, btTotalBytes = btTotalBytes bt - evictedBytes}

-- | Pack multiple messages into batched packets.
-- Wire format: [u8 count][u16 len][data]...
batchMessages :: [BS.ByteString] -> Int -> [BS.ByteString]
batchMessages messages maxSize = reverse $ go messages [] [] batchHeaderSize 0
  where
    -- Both accumulators are built in reverse (cons) for O(1) prepend.
    go [] currentBatch batchesAcc _ msgCount
      | msgCount > 0 = finalizeBatch msgCount currentBatch : batchesAcc
      | otherwise = batchesAcc
    go (msg : rest) currentBatch batchesAcc currentSize msgCount =
      let msgWireSize = batchLengthSize + BS.length msg
          shouldFinalize =
            (currentSize + msgWireSize > maxSize && msgCount > 0)
              || msgCount >= fromIntegral maxBatchMessages
       in if shouldFinalize
            then
              go
                (msg : rest)
                []
                (finalizeBatch msgCount currentBatch : batchesAcc)
                batchHeaderSize
                0
            else
              go
                rest
                (msg : currentBatch)
                batchesAcc
                (currentSize + msgWireSize)
                (msgCount + 1)

    finalizeBatch :: Int -> [BS.ByteString] -> BS.ByteString
    finalizeBatch count msgsRev =
      let msgs = reverse msgsRev
          countByte = BSB.word8 (fromIntegral count)
          msgBuilders = map (\m -> BSB.word16BE (fromIntegral (BS.length m)) <> BSB.byteString m) msgs
       in BSL.toStrict $ BSB.toLazyByteString $ countByte <> mconcat msgBuilders

-- | Unbatch a batched packet into individual messages.
unbatchMessages :: BS.ByteString -> Maybe [BS.ByteString]
unbatchMessages dat
  | BS.null dat = Nothing
  | otherwise =
      let msgCount = fromIntegral (BS.index dat 0) :: Int
       in readMessages msgCount 1 []
  where
    readMessages 0 _ acc = Just (reverse acc)
    readMessages n offset acc
      | offset + 2 > BS.length dat = Nothing
      | otherwise =
          let len =
                fromIntegral (BS.index dat offset) * 256
                  + fromIntegral (BS.index dat (offset + 1))
              msgStart = offset + 2
           in if msgStart + len > BS.length dat
                then Nothing
                else
                  let msg = BS.take len (BS.drop msgStart dat)
                   in readMessages (n - 1) (msgStart + len) (msg : acc)
