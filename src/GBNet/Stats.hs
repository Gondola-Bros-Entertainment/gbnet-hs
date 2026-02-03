-- |
-- Module      : GBNet.Stats
-- Description : Network statistics tracking
--
-- Provides 'NetworkStats' for tracking connection health metrics.
module GBNet.Stats
  ( -- * Connection quality
    ConnectionQuality (..),
    assessConnectionQuality,

    -- * Network statistics
    NetworkStats (..),
    defaultNetworkStats,

    -- * Channel statistics
    ChannelStats (..),
    defaultChannelStats,

    -- * Reliability statistics
    ReliabilityStats (..),
    defaultReliabilityStats,
  )
where

import Data.Word (Word64)

-- | Connection quality assessment.
data ConnectionQuality
  = QualityExcellent
  | QualityGood
  | QualityFair
  | QualityPoor
  | QualityBad
  deriving (Eq, Show, Ord)

-- | Assess connection quality from RTT and packet loss.
assessConnectionQuality :: Float -> Float -> ConnectionQuality
assessConnectionQuality rttMs lossPercent
  | lossPercent > 10.0 || rttMs > 500.0 = QualityBad
  | lossPercent > 5.0 || rttMs > 250.0 = QualityPoor
  | lossPercent > 2.0 || rttMs > 150.0 = QualityFair
  | lossPercent > 0.5 || rttMs > 80.0 = QualityGood
  | otherwise = QualityExcellent

-- | Network statistics for a connection.
data NetworkStats = NetworkStats
  { nsPacketsSent :: !Word64,
    nsPacketsReceived :: !Word64,
    nsBytesSent :: !Word64,
    nsBytesReceived :: !Word64,
    nsRtt :: !Float,
    nsPacketLoss :: !Float,
    nsBandwidthUp :: !Float,
    nsBandwidthDown :: !Float,
    nsConnectionQuality :: !ConnectionQuality
  }
  deriving (Show)

-- | Default (zero) network statistics.
defaultNetworkStats :: NetworkStats
defaultNetworkStats =
  NetworkStats
    { nsPacketsSent = 0,
      nsPacketsReceived = 0,
      nsBytesSent = 0,
      nsBytesReceived = 0,
      nsRtt = 0.0,
      nsPacketLoss = 0.0,
      nsBandwidthUp = 0.0,
      nsBandwidthDown = 0.0,
      nsConnectionQuality = QualityExcellent
    }

-- | Per-channel statistics.
data ChannelStats = ChannelStats
  { csMessagesSent :: !Word64,
    csMessagesReceived :: !Word64,
    csMessagesDropped :: !Word64,
    csRetransmissions :: !Word64
  }
  deriving (Show)

-- | Default channel statistics.
defaultChannelStats :: ChannelStats
defaultChannelStats =
  ChannelStats
    { csMessagesSent = 0,
      csMessagesReceived = 0,
      csMessagesDropped = 0,
      csRetransmissions = 0
    }

-- | Reliability layer statistics.
data ReliabilityStats = ReliabilityStats
  { rsTotalSent :: !Word64,
    rsTotalAcked :: !Word64,
    rsTotalLost :: !Word64,
    rsEvicted :: !Word64,
    rsBytesSent :: !Word64,
    rsBytesAcked :: !Word64
  }
  deriving (Show)

-- | Default reliability statistics.
defaultReliabilityStats :: ReliabilityStats
defaultReliabilityStats =
  ReliabilityStats
    { rsTotalSent = 0,
      rsTotalAcked = 0,
      rsTotalLost = 0,
      rsEvicted = 0,
      rsBytesSent = 0,
      rsBytesAcked = 0
    }
