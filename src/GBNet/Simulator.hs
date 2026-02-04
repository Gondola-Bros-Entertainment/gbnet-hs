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

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Foldable (toList)
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
simulatorProcessSend dat addr now =
  runState $ do
    -- Generate random values
    r1 <- nextRandomS
    r2 <- nextRandomS
    r3 <- nextRandomS
    r4 <- nextRandomS

    config <- gets nsConfig

    -- Check packet loss
    let lossThreshold = realToFrac (simPacketLoss config) :: Double
        isLost = lossThreshold > 0.0 && randomDouble r1 < lossThreshold
    if isLost
      then pure []
      else do
        -- Check bandwidth limit
        overBandwidth <-
          if simBandwidthLimitBytesPerSec config > 0
            then do
              refillTokensS now
              tokens <- gets nsTokenBucketTokens
              let needed = fromIntegral (BS.length dat)
              if tokens < needed
                then pure True
                else do
                  modify' $ \s -> s {nsTokenBucketTokens = tokens - needed}
                  pure False
            else pure False
        if overBandwidth
          then pure []
          else do
            -- Calculate delay
            let baseLatency = fromIntegral (simLatencyMs config) :: Double
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
                deliverAt = now + round (totalDelayMs * 1e6)

            -- Deliver immediately or delay
            immediate <-
              if totalDelayMs < immediateDeliveryThresholdMs
                then pure [(dat, addr)]
                else do
                  let delayed =
                        DelayedPacket
                          { dpData = dat,
                            dpAddr = addr,
                            dpDeliverAt = deliverAt
                          }
                  modify' $ \s -> s {nsDelayedPackets = nsDelayedPackets s Seq.|> delayed}
                  pure []

            -- Handle duplicates
            r5 <- nextRandomS
            let dupChance = realToFrac (simDuplicateChance config) :: Double
            if dupChance > 0.0 && randomDouble r5 < dupChance
              then do
                let dupDelayMs = delayMs + randomDouble r5 * duplicateJitterMs
                    dupDeliverAt = now + round (dupDelayMs * 1e6)
                    dupPacket =
                      DelayedPacket
                        { dpData = dat,
                          dpAddr = addr,
                          dpDeliverAt = dupDeliverAt
                        }
                modify' $ \s -> s {nsDelayedPackets = nsDelayedPackets s Seq.|> dupPacket}
              else pure ()
            pure immediate

-- | Retrieve packets ready for delivery.
-- Scans all delayed packets since they may not be sorted by delivery time.
simulatorReceiveReady :: MonoTime -> NetworkSimulator -> ([(BS.ByteString, Word64)], NetworkSimulator)
simulatorReceiveReady now sim =
  let (ready, notReady) = Seq.partition (\pkt -> dpDeliverAt pkt <= now) (nsDelayedPackets sim)
      results = map (\pkt -> (dpData pkt, dpAddr pkt)) (toList ready)
   in (results, sim {nsDelayedPackets = notReady})

-- | Get count of pending delayed packets.
simulatorPendingCount :: NetworkSimulator -> Int
simulatorPendingCount = Seq.length . nsDelayedPackets

-- | Refill token bucket based on elapsed time (State version).
refillTokensS :: MonoTime -> State NetworkSimulator ()
refillTokensS now = modify' $ \sim ->
  let elapsedSecs = elapsedMs (nsLastTokenRefill sim) now / 1000.0
      refillRate = fromIntegral (simBandwidthLimitBytesPerSec (nsConfig sim))
      newTokens = nsTokenBucketTokens sim + elapsedSecs * refillRate
      maxTokens = refillRate -- Cap at 1 second worth
      cappedTokens = min newTokens maxTokens
   in sim
        { nsTokenBucketTokens = cappedTokens,
          nsLastTokenRefill = now
        }

-- | SplitMix-style random number generator (State version).
-- Uses a different output function from the state update to avoid leaking state.
nextRandomS :: State NetworkSimulator Word64
nextRandomS = do
  s <- gets nsRngState
  let -- LCG state update
      a = 6364136223846793005
      c = 1442695040888963407
      next = a * s + c
      -- SplitMix-style output mixing (state is not exposed)
      z0 = next `xor` (next `shiftR` 30)
      z1 = z0 * 0xBF58476D1CE4E5B9
      z2 = z1 `xor` (z1 `shiftR` 27)
      z3 = z2 * 0x94D049BB133111EB
      output = z3 `xor` (z3 `shiftR` 31)
  modify' $ \sim -> sim {nsRngState = next}
  pure output

-- | Convert random Word64 to double in [0, 1).
-- Uses upper 32 bits for better uniformity with SplitMix output.
randomDouble :: Word64 -> Double
randomDouble w = fromIntegral (w .&. 0xFFFFFFFF) / 4294967296.0
