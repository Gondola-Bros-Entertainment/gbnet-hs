-- |
-- Module      : GBNet.Stats
-- Description : Network statistics tracking
--
-- Provides 'NetworkStats' for tracking connection health metrics.
module GBNet.Stats
  ( -- * Connection quality
    ConnectionQuality (..),
    assessConnectionQuality,

    -- * Congestion level
    CongestionLevel (..),

    -- * Network statistics
    NetworkStats (..),
    defaultNetworkStats,

    -- * Channel statistics
    ChannelStats (..),
    defaultChannelStats,

    -- * Reliability statistics
    ReliabilityStats (..),
    defaultReliabilityStats,

    -- * Socket statistics
    SocketStats (..),
    defaultSocketStats,
  )
where

import Data.Word (Word64)
import GBNet.Reliability (MonoTime)

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

-- | Congestion pressure level reported to the application.
--
-- Applications can use this to adapt their behavior:
--
-- * 'CongestionNone' — send freely
-- * 'CongestionElevated' — consider reducing non-essential traffic
-- * 'CongestionHigh' — drop low-priority data, reduce send rate
-- * 'CongestionCritical' — sends are being suppressed, only send essential data
data CongestionLevel
  = CongestionNone
  | CongestionElevated
  | CongestionHigh
  | CongestionCritical
  deriving (Eq, Show, Ord)

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
    nsConnectionQuality :: !ConnectionQuality,
    nsCongestionLevel :: !CongestionLevel
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
      nsConnectionQuality = QualityExcellent,
      nsCongestionLevel = CongestionNone
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

-- | Socket-level statistics.
data SocketStats = SocketStats
  { ssPacketsSent :: !Word64,
    ssPacketsReceived :: !Word64,
    ssBytesSent :: !Word64,
    ssBytesReceived :: !Word64,
    ssCrcDrops :: !Word64,
    ssLastSendTime :: !(Maybe MonoTime),
    ssLastReceiveTime :: !(Maybe MonoTime)
  }
  deriving (Show)

-- | Default socket statistics.
defaultSocketStats :: SocketStats
defaultSocketStats =
  SocketStats
    { ssPacketsSent = 0,
      ssPacketsReceived = 0,
      ssBytesSent = 0,
      ssBytesReceived = 0,
      ssCrcDrops = 0,
      ssLastSendTime = Nothing,
      ssLastReceiveTime = Nothing
    }
