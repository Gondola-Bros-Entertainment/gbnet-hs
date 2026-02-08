{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Config
-- Description : Network configuration constants and structures
--
-- 'NetworkConfig' controls all tunable parameters: timeouts, MTU, channels,
-- congestion, etc. 'ChannelConfig' is re-exported from "GBNet.Channel".
module GBNet.Config
  ( -- * Constants
    defaultProtocolId,
    defaultMaxClients,
    defaultConnectionTimeoutMs,
    defaultKeepaliveIntervalMs,
    defaultConnectionRequestTimeoutMs,
    defaultConnectionRequestMaxRetries,
    defaultMtu,
    defaultFragmentThreshold,
    defaultFragmentTimeoutMs,
    defaultMaxFragments,
    defaultMaxReassemblyBufferSize,
    defaultPacketBufferSize,
    defaultAckBufferSize,
    defaultMaxSequenceDistance,
    defaultReliableRetryTimeMs,
    defaultMaxReliableRetries,
    defaultMaxChannels,
    defaultSendRateHz,
    defaultMaxPacketRateHz,
    defaultCongestionThreshold,
    defaultCongestionGoodRttThresholdMs,
    defaultCongestionBadLossThreshold,
    defaultCongestionRecoveryTimeMs,
    defaultDisconnectRetries,
    defaultDisconnectRetryTimeoutMs,
    defaultMaxInFlight,
    defaultChannelPriority,
    maxBackoffExponent,
    minMtu,
    maxMtu,
    maxChannelCount,
    defaultDeltaBaselineTimeoutMs,
    defaultMaxBaselineSnapshots,

    -- * Configuration
    NetworkConfig (..),
    defaultNetworkConfig,

    -- * Configuration errors
    ConfigError (..),
    validateConfig,

    -- * Simulation
    SimulationConfig (..),
    defaultSimulationConfig,
  )
where

import Data.Word (Word16, Word32, Word8)
import GBNet.Channel (ChannelConfig, defaultChannelConfig)
import Optics.TH (makeFieldLabelsNoPrefix)

-- Constants

-- | Default protocol identifier for packet validation.
defaultProtocolId :: Word32
defaultProtocolId = 0x12345678

-- | Default maximum simultaneous client connections.
defaultMaxClients :: Int
defaultMaxClients = 64

-- | Default connection timeout in milliseconds.
defaultConnectionTimeoutMs :: Double
defaultConnectionTimeoutMs = 10000.0

-- | Default keepalive interval in milliseconds.
defaultKeepaliveIntervalMs :: Double
defaultKeepaliveIntervalMs = 1000.0

-- | Default connection request timeout in milliseconds.
defaultConnectionRequestTimeoutMs :: Double
defaultConnectionRequestTimeoutMs = 5000.0

-- | Default maximum connection request retries before giving up.
defaultConnectionRequestMaxRetries :: Int
defaultConnectionRequestMaxRetries = 5

-- | Default maximum transmission unit in bytes.
defaultMtu :: Int
defaultMtu = 1200

-- | Default fragment threshold in bytes; messages above this are fragmented.
defaultFragmentThreshold :: Int
defaultFragmentThreshold = 1024

-- | Default fragment reassembly timeout in milliseconds.
defaultFragmentTimeoutMs :: Double
defaultFragmentTimeoutMs = 5000.0

-- | Default maximum fragments per message.
defaultMaxFragments :: Int
defaultMaxFragments = 256

-- | Default maximum reassembly buffer size in bytes.
defaultMaxReassemblyBufferSize :: Int
defaultMaxReassemblyBufferSize = 1024 * 1024

-- | Default packet buffer size (number of entries).
defaultPacketBufferSize :: Int
defaultPacketBufferSize = 256

-- | Default ACK buffer size (number of entries).
defaultAckBufferSize :: Int
defaultAckBufferSize = 256

-- | Default maximum sequence distance before treating packets as stale.
defaultMaxSequenceDistance :: Word16
defaultMaxSequenceDistance = 32768

-- | Default reliable retry time in milliseconds.
defaultReliableRetryTimeMs :: Double
defaultReliableRetryTimeMs = 100.0

-- | Default maximum reliable message retries before dropping.
defaultMaxReliableRetries :: Int
defaultMaxReliableRetries = 10

-- | Default maximum channel count.
defaultMaxChannels :: Int
defaultMaxChannels = 8

-- | Default send rate in packets per second.
defaultSendRateHz :: Double
defaultSendRateHz = 60.0

-- | Default maximum packet rate in packets per second.
defaultMaxPacketRateHz :: Double
defaultMaxPacketRateHz = 120.0

-- | Default congestion detection threshold (fraction of budget used).
defaultCongestionThreshold :: Double
defaultCongestionThreshold = 0.1

-- | Default RTT threshold in milliseconds for "good" congestion mode.
defaultCongestionGoodRttThresholdMs :: Double
defaultCongestionGoodRttThresholdMs = 250.0

-- | Default packet loss threshold for "bad" congestion mode.
defaultCongestionBadLossThreshold :: Double
defaultCongestionBadLossThreshold = 0.1

-- | Default congestion recovery time in milliseconds.
defaultCongestionRecoveryTimeMs :: Double
defaultCongestionRecoveryTimeMs = 10000.0

-- | Default number of disconnect packet retries.
defaultDisconnectRetries :: Int
defaultDisconnectRetries = 3

-- | Default disconnect retry timeout in milliseconds.
defaultDisconnectRetryTimeoutMs :: Double
defaultDisconnectRetryTimeoutMs = 500.0

-- | Default maximum packets in flight before back-pressure.
defaultMaxInFlight :: Int
defaultMaxInFlight = 256

-- | Default channel priority (0 = lowest, 255 = highest).
defaultChannelPriority :: Word8
defaultChannelPriority = 128

-- | Maximum exponential backoff exponent (caps at 2^5 = 32x RTO).
maxBackoffExponent :: Int
maxBackoffExponent = 5

-- | Minimum allowed MTU (RFC 791 minimum IP datagram size).
minMtu :: Int
minMtu = 576

-- | Maximum allowed MTU.
maxMtu :: Int
maxMtu = 65535

-- | Maximum channel count, constrained by 3-bit wire format (payload header).
maxChannelCount :: Int
maxChannelCount = 8

-- | Default maximum pending connection requests.
defaultMaxPending :: Int
defaultMaxPending = 256

-- | Default rate limit for connection requests per second.
defaultRateLimitPerSecond :: Int
defaultRateLimitPerSecond = 10

-- | Default delta baseline timeout in milliseconds.
defaultDeltaBaselineTimeoutMs :: Double
defaultDeltaBaselineTimeoutMs = 2000.0

-- | Default maximum baseline snapshots for delta compression.
defaultMaxBaselineSnapshots :: Int
defaultMaxBaselineSnapshots = 32

-- | Configuration validation errors.
data ConfigError
  = FragmentThresholdExceedsMtu
  | InvalidChannelCount
  | InvalidPacketBufferSize
  | InvalidMtu
  | TimeoutNotGreaterThanKeepalive
  | InvalidMaxClients
  | ChannelConfigsExceedMaxChannels
  | InvalidSendRate
  | InvalidMaxPacketRate
  | InvalidMaxInFlight
  | InvalidFragmentThreshold
  | SendRateExceedsMaxPacketRate
  | InvalidCongestionThreshold
  deriving (Eq, Show)

-- | Top-level network configuration.
data NetworkConfig = NetworkConfig
  { ncProtocolId :: !Word32,
    ncMaxClients :: !Int,
    ncConnectionTimeoutMs :: !Double,
    ncKeepaliveIntervalMs :: !Double,
    ncConnectionRequestTimeoutMs :: !Double,
    ncConnectionRequestMaxRetries :: !Int,
    ncMtu :: !Int,
    ncFragmentThreshold :: !Int,
    ncFragmentTimeoutMs :: !Double,
    ncMaxFragments :: !Int,
    ncMaxReassemblyBufferSize :: !Int,
    ncPacketBufferSize :: !Int,
    ncAckBufferSize :: !Int,
    ncMaxSequenceDistance :: !Word16,
    ncReliableRetryTimeMs :: !Double,
    ncMaxReliableRetries :: !Int,
    ncMaxInFlight :: !Int,
    ncMaxChannels :: !Int,
    ncDefaultChannelConfig :: !ChannelConfig,
    ncChannelConfigs :: ![ChannelConfig],
    ncSendRate :: !Double,
    ncMaxPacketRate :: !Double,
    ncCongestionThreshold :: !Double,
    ncCongestionGoodRttThreshold :: !Double,
    ncCongestionBadLossThreshold :: !Double,
    ncCongestionRecoveryTimeMs :: !Double,
    ncDisconnectRetries :: !Int,
    ncDisconnectRetryTimeoutMs :: !Double,
    ncMaxPending :: !Int,
    ncRateLimitPerSecond :: !Int,
    ncUseCwndCongestion :: !Bool,
    ncSimulation :: !(Maybe SimulationConfig),
    ncEnableConnectionMigration :: !Bool,
    ncDeltaBaselineTimeoutMs :: !Double,
    ncMaxBaselineSnapshots :: !Int
  }
  deriving (Eq, Show)

-- | Default network configuration.
defaultNetworkConfig :: NetworkConfig
defaultNetworkConfig =
  NetworkConfig
    { ncProtocolId = defaultProtocolId,
      ncMaxClients = defaultMaxClients,
      ncConnectionTimeoutMs = defaultConnectionTimeoutMs,
      ncKeepaliveIntervalMs = defaultKeepaliveIntervalMs,
      ncConnectionRequestTimeoutMs = defaultConnectionRequestTimeoutMs,
      ncConnectionRequestMaxRetries = defaultConnectionRequestMaxRetries,
      ncMtu = defaultMtu,
      ncFragmentThreshold = defaultFragmentThreshold,
      ncFragmentTimeoutMs = defaultFragmentTimeoutMs,
      ncMaxFragments = defaultMaxFragments,
      ncMaxReassemblyBufferSize = defaultMaxReassemblyBufferSize,
      ncPacketBufferSize = defaultPacketBufferSize,
      ncAckBufferSize = defaultAckBufferSize,
      ncMaxSequenceDistance = defaultMaxSequenceDistance,
      ncReliableRetryTimeMs = defaultReliableRetryTimeMs,
      ncMaxReliableRetries = defaultMaxReliableRetries,
      ncMaxInFlight = defaultMaxInFlight,
      ncMaxChannels = defaultMaxChannels,
      ncDefaultChannelConfig = defaultChannelConfig,
      ncChannelConfigs = [],
      ncSendRate = defaultSendRateHz,
      ncMaxPacketRate = defaultMaxPacketRateHz,
      ncCongestionThreshold = defaultCongestionThreshold,
      ncCongestionGoodRttThreshold = defaultCongestionGoodRttThresholdMs,
      ncCongestionBadLossThreshold = defaultCongestionBadLossThreshold,
      ncCongestionRecoveryTimeMs = defaultCongestionRecoveryTimeMs,
      ncDisconnectRetries = defaultDisconnectRetries,
      ncDisconnectRetryTimeoutMs = defaultDisconnectRetryTimeoutMs,
      ncMaxPending = defaultMaxPending,
      ncRateLimitPerSecond = defaultRateLimitPerSecond,
      ncUseCwndCongestion = False,
      ncSimulation = Nothing,
      ncEnableConnectionMigration = True,
      ncDeltaBaselineTimeoutMs = defaultDeltaBaselineTimeoutMs,
      ncMaxBaselineSnapshots = defaultMaxBaselineSnapshots
    }

-- | Validate configuration, returning an error if invalid.
validateConfig :: NetworkConfig -> Either ConfigError ()
validateConfig cfg
  | ncFragmentThreshold cfg > ncMtu cfg =
      Left FragmentThresholdExceedsMtu
  | ncMaxChannels cfg == 0 || ncMaxChannels cfg > maxChannelCount =
      Left InvalidChannelCount
  | ncPacketBufferSize cfg == 0 =
      Left InvalidPacketBufferSize
  | ncMtu cfg < minMtu || ncMtu cfg > maxMtu =
      Left InvalidMtu
  | ncConnectionTimeoutMs cfg <= ncKeepaliveIntervalMs cfg =
      Left TimeoutNotGreaterThanKeepalive
  | ncMaxClients cfg == 0 =
      Left InvalidMaxClients
  | length (ncChannelConfigs cfg) > ncMaxChannels cfg =
      Left ChannelConfigsExceedMaxChannels
  | not (isValidPositive (ncSendRate cfg)) =
      Left InvalidSendRate
  | not (isValidPositive (ncMaxPacketRate cfg)) =
      Left InvalidMaxPacketRate
  | ncMaxInFlight cfg == 0 =
      Left InvalidMaxInFlight
  | ncFragmentThreshold cfg == 0 =
      Left InvalidFragmentThreshold
  | ncSendRate cfg > ncMaxPacketRate cfg =
      Left SendRateExceedsMaxPacketRate
  | not (isFinite (ncCongestionGoodRttThreshold cfg))
      || not (isFinite (ncCongestionBadLossThreshold cfg))
      || not (isFinite (ncCongestionThreshold cfg)) =
      Left InvalidCongestionThreshold
  | otherwise = Right ()
  where
    isValidPositive x = x > 0 && not (isNaN x)
    isFinite x = not (isNaN x) && not (isInfinite x)

-- | Network condition simulation configuration.
data SimulationConfig = SimulationConfig
  { simPacketLoss :: !Double,
    simLatencyMs :: !Int,
    simJitterMs :: !Int,
    simDuplicateChance :: !Double,
    simOutOfOrderChance :: !Double,
    simBandwidthLimitBytesPerSec :: !Int
  }
  deriving (Eq, Show)

-- | Default simulation config (no simulation).
defaultSimulationConfig :: SimulationConfig
defaultSimulationConfig =
  SimulationConfig
    { simPacketLoss = 0.0,
      simLatencyMs = 0,
      simJitterMs = 0,
      simDuplicateChance = 0.0,
      simOutOfOrderChance = 0.0,
      simBandwidthLimitBytesPerSec = 0
    }

makeFieldLabelsNoPrefix ''NetworkConfig
makeFieldLabelsNoPrefix ''SimulationConfig
