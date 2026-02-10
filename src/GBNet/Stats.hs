{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- * Socket statistics
    SocketStats (..),
    defaultSocketStats,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Word (Word64)
import GBNet.Reliability (MonoTime)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Connection quality assessment.
data ConnectionQuality
  = QualityExcellent
  | QualityGood
  | QualityFair
  | QualityPoor
  | QualityBad
  deriving (Eq, Show, Ord)

instance NFData ConnectionQuality where rnf = rwhnf

-- Quality thresholds: (lossPercent, rttMs)
badLossThreshold, poorLossThreshold, fairLossThreshold, goodLossThreshold :: Double
badLossThreshold = 10.0
poorLossThreshold = 5.0
fairLossThreshold = 2.0
goodLossThreshold = 0.5

badRttThreshold, poorRttThreshold, fairRttThreshold, goodRttThreshold :: Double
badRttThreshold = 500.0
poorRttThreshold = 250.0
fairRttThreshold = 150.0
goodRttThreshold = 80.0

-- | Assess connection quality from RTT and packet loss.
assessConnectionQuality :: Double -> Double -> ConnectionQuality
assessConnectionQuality rttMs lossPercent
  | lossPercent > badLossThreshold || rttMs > badRttThreshold = QualityBad
  | lossPercent > poorLossThreshold || rttMs > poorRttThreshold = QualityPoor
  | lossPercent > fairLossThreshold || rttMs > fairRttThreshold = QualityFair
  | lossPercent > goodLossThreshold || rttMs > goodRttThreshold = QualityGood
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

instance NFData CongestionLevel where rnf = rwhnf

-- | Network statistics for a connection.
data NetworkStats = NetworkStats
  { nsPacketsSent :: !Word64,
    nsPacketsReceived :: !Word64,
    nsBytesSent :: !Word64,
    nsBytesReceived :: !Word64,
    nsRtt :: !Double,
    nsPacketLoss :: !Double,
    nsBandwidthUp :: !Double,
    nsBandwidthDown :: !Double,
    nsConnectionQuality :: !ConnectionQuality,
    nsCongestionLevel :: !CongestionLevel,
    nsDecryptionFailures :: !Word64
  }
  deriving (Show)

instance NFData NetworkStats where
  rnf ns =
    rnf (nsPacketsSent ns) `seq`
      rnf (nsPacketsReceived ns) `seq`
        rnf (nsBytesSent ns) `seq`
          rnf (nsBytesReceived ns) `seq`
            rnf (nsRtt ns) `seq`
              rnf (nsPacketLoss ns) `seq`
                rnf (nsBandwidthUp ns) `seq`
                  rnf (nsBandwidthDown ns) `seq`
                    rnf (nsConnectionQuality ns) `seq`
                      rnf (nsCongestionLevel ns) `seq`
                        rnf (nsDecryptionFailures ns)

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
      nsCongestionLevel = CongestionNone,
      nsDecryptionFailures = 0
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

makeFieldLabelsNoPrefix ''NetworkStats
makeFieldLabelsNoPrefix ''SocketStats
