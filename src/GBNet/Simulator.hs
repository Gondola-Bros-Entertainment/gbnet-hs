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

    -- * Delayed packet
    DelayedPacket (..),
  )
where

import qualified Data.ByteString as BS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GBNet.Config (SimulationConfig (..))
import GBNet.Reliability (MonoTime, elapsedMs)

-- | Maximum extra delay (ms) applied to out-of-order packets.
outOfOrderMaxDelayMs :: Double
outOfOrderMaxDelayMs = 50.0

-- | Packets with total delay below this threshold (ms) are delivered immediately.
immediateDeliveryThresholdMs :: Double
immediateDeliveryThresholdMs = 1.0

-- | A packet delayed for later delivery.
data DelayedPacket = DelayedPacket
  { dpData :: !BS.ByteString,
    dpAddr :: !Word64, -- Address key (simplified)
    dpDeliverAt :: !MonoTime
  }
  deriving (Show)

-- | Network condition simulator.
data NetworkSimulator = NetworkSimulator
  { nsConfig :: !SimulationConfig,
    nsDelayedPackets :: !(Seq DelayedPacket),
    nsTokenBucketTokens :: !Double,
    nsLastTokenRefill :: !MonoTime,
    nsRngState :: !Word64 -- Simple LCG RNG state
  }
  deriving (Show)

-- | Create a new network simulator.
newNetworkSimulator :: SimulationConfig -> MonoTime -> NetworkSimulator
newNetworkSimulator config now =
  NetworkSimulator
    { nsConfig = config,
      nsDelayedPackets = Seq.empty,
      nsTokenBucketTokens = 0.0,
      nsLastTokenRefill = now,
      nsRngState = now -- Use time as initial seed
    }

-- | Process an outgoing packet through the simulator.
-- Returns (immediate packets, updated simulator).
simulatorProcessSend ::
  BS.ByteString ->
  Word64 ->
  MonoTime ->
  NetworkSimulator ->
  ([(BS.ByteString, Word64)], NetworkSimulator)
simulatorProcessSend dat addr now sim =
  let -- Generate random values
      (r1, sim1) = nextRandom sim
      (r2, sim2) = nextRandom sim1
      (r3, sim3) = nextRandom sim2
      (r4, sim4) = nextRandom sim3

      config = nsConfig sim4

      -- Check packet loss
      lossThreshold = realToFrac (simPacketLoss config) :: Double
      isLost = lossThreshold > 0.0 && randomDouble r1 < lossThreshold
   in if isLost
        then ([], sim4)
        else
          let -- Check bandwidth limit
              (sim5, overBandwidth) =
                if simBandwidthLimitBytesPerSec config > 0
                  then
                    let sim' = refillTokens now sim4
                        needed = fromIntegral (BS.length dat)
                     in if nsTokenBucketTokens sim' < needed
                          then (sim', True)
                          else (sim' {nsTokenBucketTokens = nsTokenBucketTokens sim' - needed}, False)
                  else (sim4, False)
           in if overBandwidth
                then ([], sim5)
                else
                  let -- Calculate delay
                      baseLatency = fromIntegral (simLatencyMs config) :: Double
                      jitter =
                        if simJitterMs config > 0
                          then randomDouble r2 * fromIntegral (simJitterMs config)
                          else 0.0
                      delayMs = baseLatency + jitter

                      -- Out of order extra delay
                      outOfOrderChance = realToFrac (simOutOfOrderChance config) :: Double
                      extraDelay =
                        if outOfOrderChance > 0.0 && randomDouble r3 < outOfOrderChance
                          then randomDouble r4 * outOfOrderMaxDelayMs
                          else 0.0

                      totalDelayMs = delayMs + extraDelay
                      deliverAt = now + round totalDelayMs

                      -- Create delayed packet
                      delayed =
                        DelayedPacket
                          { dpData = dat,
                            dpAddr = addr,
                            dpDeliverAt = deliverAt
                          }

                      (immediate, sim6) =
                        if totalDelayMs < immediateDeliveryThresholdMs
                          then ([(dat, addr)], sim5)
                          else ([], sim5 {nsDelayedPackets = nsDelayedPackets sim5 Seq.|> delayed})

                      -- Handle duplicates
                      (r5, sim7) = nextRandom sim6
                      dupChance = realToFrac (simDuplicateChance config) :: Double
                      sim8 =
                        if dupChance > 0.0 && randomDouble r5 < dupChance
                          then
                            let dupDelayMs = delayMs + randomDouble r5 * 20.0
                                dupDeliverAt = now + round dupDelayMs
                                dupPacket =
                                  DelayedPacket
                                    { dpData = dat,
                                      dpAddr = addr,
                                      dpDeliverAt = dupDeliverAt
                                    }
                             in sim7 {nsDelayedPackets = nsDelayedPackets sim7 Seq.|> dupPacket}
                          else sim7
                   in (immediate, sim8)

-- | Retrieve packets ready for delivery.
simulatorReceiveReady :: MonoTime -> NetworkSimulator -> ([(BS.ByteString, Word64)], NetworkSimulator)
simulatorReceiveReady now = go []
  where
    go acc s =
      case Seq.viewl (nsDelayedPackets s) of
        Seq.EmptyL -> (reverse acc, s)
        pkt Seq.:< rest ->
          if dpDeliverAt pkt <= now
            then go ((dpData pkt, dpAddr pkt) : acc) (s {nsDelayedPackets = rest})
            else (reverse acc, s)

-- | Get count of pending delayed packets.
simulatorPendingCount :: NetworkSimulator -> Int
simulatorPendingCount = Seq.length . nsDelayedPackets

-- | Refill token bucket based on elapsed time.
refillTokens :: MonoTime -> NetworkSimulator -> NetworkSimulator
refillTokens now sim =
  let elapsedSecs = elapsedMs (nsLastTokenRefill sim) now / 1000.0
      refillRate = fromIntegral (simBandwidthLimitBytesPerSec (nsConfig sim))
      newTokens = nsTokenBucketTokens sim + elapsedSecs * refillRate
      maxTokens = refillRate -- Cap at 1 second worth
      cappedTokens = min newTokens maxTokens
   in sim
        { nsTokenBucketTokens = cappedTokens,
          nsLastTokenRefill = now
        }

-- | Simple LCG random number generator (returns next state).
nextRandom :: NetworkSimulator -> (Word64, NetworkSimulator)
nextRandom sim =
  let a = 6364136223846793005
      c = 1442695040888963407
      next = a * nsRngState sim + c
   in (next, sim {nsRngState = next})

-- | Convert random Word64 to double in [0, 1).
randomDouble :: Word64 -> Double
randomDouble w = fromIntegral (w `mod` 1000000) / 1000000.0
