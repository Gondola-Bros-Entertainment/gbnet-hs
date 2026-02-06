{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Simulator
-- Description : Network condition simulator for testing
--
-- Simulates packet loss, latency, jitter, duplicates, reordering,
-- and bandwidth limiting for testing purposes.
module GBNet.Simulator
  ( -- * Simulator
    NetworkSimulator (..),
    newNetworkSimulator,
    simulatorProcessSend,
    simulatorReceiveReady,
    simulatorPendingCount,
    simulatorConfig,

    -- * Delayed packet
    DelayedPacket (..),
  )
where

import Control.Monad (when)
import Control.Monad.State.Strict (State, runState)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GBNet.Class (MonoTime (..))
import GBNet.Config (SimulationConfig (..))
import GBNet.Reliability (elapsedMs)
import GBNet.Util (nextRandom)
import Optics ((&), (.~))
import Optics.State (use)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Maximum extra delay (ms) applied to out-of-order packets.
outOfOrderMaxDelayMs :: Double
outOfOrderMaxDelayMs = 50.0

-- | Packets with total delay below this threshold (ms) are delivered immediately.
immediateDeliveryThresholdMs :: Double
immediateDeliveryThresholdMs = 1.0

-- | Maximum extra delay (ms) applied to duplicate packets.
duplicateJitterMs :: Double
duplicateJitterMs = 20.0

-- | A packet delayed for later delivery.
data DelayedPacket = DelayedPacket
  { dpData :: !BS.ByteString,
    dpAddr :: !Word64, -- Address key (simplified)
    dpDeliverAt :: !MonoTime
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''DelayedPacket

-- | Network condition simulator.
data NetworkSimulator = NetworkSimulator
  { nsConfig :: !SimulationConfig,
    nsDelayedPackets :: !(Seq DelayedPacket),
    nsTokenBucketTokens :: !Double,
    nsLastTokenRefill :: !MonoTime,
    nsRngState :: !Word64 -- Simple LCG RNG state
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''NetworkSimulator

-- | Create a new network simulator.
newNetworkSimulator :: SimulationConfig -> MonoTime -> NetworkSimulator
newNetworkSimulator config now =
  NetworkSimulator
    { nsConfig = config,
      nsDelayedPackets = Seq.empty,
      nsTokenBucketTokens = 0.0,
      nsLastTokenRefill = now,
      nsRngState = unMonoTime now -- Use time as initial seed
    }

-- | Process an outgoing packet through the simulator.
-- Returns (immediate packets, updated simulator).
simulatorProcessSend ::
  BS.ByteString ->
  Word64 ->
  MonoTime ->
  NetworkSimulator ->
  ([(BS.ByteString, Word64)], NetworkSimulator)
simulatorProcessSend dat addr now =
  runState $ do
    r1 <- nextRandomS
    r2 <- nextRandomS
    r3 <- nextRandomS
    r4 <- nextRandomS
    config <- use #nsConfig

    let lost = checkLoss config r1
    if lost
      then pure []
      else do
        overBandwidth <- checkBandwidthS (BS.length dat) now config
        if overBandwidth
          then pure []
          else do
            let totalDelayMs = calculateDelay config r2 r3 r4
                deliverAt = now + round (totalDelayMs * 1e6)
            immediate <- scheduleOrDeliverS dat addr deliverAt totalDelayMs
            scheduleDuplicateS dat addr now totalDelayMs config
            pure immediate

-- | Check if a packet should be dropped due to simulated loss.
checkLoss :: SimulationConfig -> Word64 -> Bool
checkLoss config rng =
  let threshold = realToFrac (simPacketLoss config) :: Double
   in threshold > 0.0 && randomDouble rng < threshold

-- | Check if bandwidth limit is exceeded, consuming tokens if available.
checkBandwidthS :: Int -> MonoTime -> SimulationConfig -> State NetworkSimulator Bool
checkBandwidthS packetSize now config
  | simBandwidthLimitBytesPerSec config <= 0 = pure False
  | otherwise = do
      refillTokensS now
      tokens <- use #nsTokenBucketTokens
      let needed = fromIntegral packetSize
      if tokens < needed
        then pure True
        else do
          #nsTokenBucketTokens .= (tokens - needed)
          pure False

-- | Calculate total delay including latency, jitter, and out-of-order reordering.
calculateDelay :: SimulationConfig -> Word64 -> Word64 -> Word64 -> Double
calculateDelay config rJitter rOoOChance rOoODelay =
  let baseLatency = fromIntegral (simLatencyMs config) :: Double
      jitter
        | simJitterMs config > 0 = randomDouble rJitter * fromIntegral (simJitterMs config)
        | otherwise = 0.0
      outOfOrderChance = realToFrac (simOutOfOrderChance config) :: Double
      extraDelay
        | outOfOrderChance > 0.0 && randomDouble rOoOChance < outOfOrderChance =
            randomDouble rOoODelay * outOfOrderMaxDelayMs
        | otherwise = 0.0
   in baseLatency + jitter + extraDelay

-- | Deliver a packet immediately or schedule it for delayed delivery.
scheduleOrDeliverS ::
  BS.ByteString -> Word64 -> MonoTime -> Double -> State NetworkSimulator [(BS.ByteString, Word64)]
scheduleOrDeliverS dat addr deliverAt totalDelayMs
  | totalDelayMs < immediateDeliveryThresholdMs = pure [(dat, addr)]
  | otherwise = do
      let delayed = DelayedPacket {dpData = dat, dpAddr = addr, dpDeliverAt = deliverAt}
      #nsDelayedPackets %= (Seq.|> delayed)
      pure []

-- | Maybe schedule a duplicate packet with extra jitter.
scheduleDuplicateS :: BS.ByteString -> Word64 -> MonoTime -> Double -> SimulationConfig -> State NetworkSimulator ()
scheduleDuplicateS dat addr now baseDelayMs config = do
  r <- nextRandomS
  let dupChance = realToFrac (simDuplicateChance config) :: Double
  when (dupChance > 0.0 && randomDouble r < dupChance) $ do
    let dupDeliverAt = now + round ((baseDelayMs + randomDouble r * duplicateJitterMs) * 1e6)
        dupPacket = DelayedPacket {dpData = dat, dpAddr = addr, dpDeliverAt = dupDeliverAt}
    #nsDelayedPackets %= (Seq.|> dupPacket)

-- | Retrieve packets ready for delivery.
-- Scans all delayed packets since they may not be sorted by delivery time.
simulatorReceiveReady :: MonoTime -> NetworkSimulator -> ([(BS.ByteString, Word64)], NetworkSimulator)
simulatorReceiveReady now sim =
  let (ready, notReady) = Seq.partition (\pkt -> dpDeliverAt pkt <= now) (nsDelayedPackets sim)
      results = map (\pkt -> (dpData pkt, dpAddr pkt)) (toList ready)
   in (results, sim & #nsDelayedPackets .~ notReady)

-- | Get count of pending delayed packets.
simulatorPendingCount :: NetworkSimulator -> Int
simulatorPendingCount = Seq.length . nsDelayedPackets

-- | Get the simulation configuration.
simulatorConfig :: NetworkSimulator -> SimulationConfig
simulatorConfig = nsConfig
{-# INLINE simulatorConfig #-}

-- | Refill token bucket based on elapsed time (State version).
refillTokensS :: MonoTime -> State NetworkSimulator ()
refillTokensS now = do
  sim <- use #nsLastTokenRefill
  config <- use #nsConfig
  tokens <- use #nsTokenBucketTokens
  let elapsedSecs = elapsedMs sim now / 1000.0
      refillRate = fromIntegral (simBandwidthLimitBytesPerSec config)
      newTokens = tokens + elapsedSecs * refillRate
      maxTokens = refillRate -- Cap at 1 second worth
      cappedTokens = min newTokens maxTokens
  #nsTokenBucketTokens .= cappedTokens
  #nsLastTokenRefill .= now

-- | Stateful wrapper around the shared 'nextRandom' from GBNet.Util.
nextRandomS :: State NetworkSimulator Word64
nextRandomS = do
  s <- use #nsRngState
  let (output, next) = nextRandom s
  #nsRngState .= next
  pure output

-- | Convert random Word64 to double in [0, 1).
-- Uses lower 32 bits; SplitMix output mixing ensures all bits are well-distributed.
randomDouble :: Word64 -> Double
randomDouble w = fromIntegral (w .&. 0xFFFFFFFF) / 4294967296.0
