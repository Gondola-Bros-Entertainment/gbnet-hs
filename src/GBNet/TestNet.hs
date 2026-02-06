{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.TestNet
-- Description : Pure deterministic network for testing
--
-- A simulated network that runs entirely in pure code.
-- Useful for:
-- - Unit testing connection logic
-- - Property-based testing
-- - Deterministic replay
-- - Network condition simulation
module GBNet.TestNet
  ( -- * Test network monad
    TestNet (..),
    runTestNet,

    -- * Test network state
    TestNetState (..),
    initialTestNetState,

    -- * Operations
    advanceTime,
    simulateLatency,
    simulateLoss,
    getPendingPackets,

    -- * Multi-peer world simulation
    TestWorld (..),
    newTestWorld,
    runPeerInWorld,
    deliverPackets,
    worldAdvanceTime,
  )
where

import Control.Monad.State.Strict (MonadState, State, get, runState)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GBNet.Class (MonadNetwork (..), MonadTime (..), MonoTime (..), NetError (..))
import GBNet.Security (validateAndStripCrc32)
import GBNet.Util (nextRandom, randomDouble)
import Network.Socket (SockAddr)
import Optics ((%), (%~), (&), (.~))
import Optics.State (use)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | A packet in transit.
data InFlightPacket = InFlightPacket
  { ifpFrom :: !SockAddr,
    ifpTo :: !SockAddr,
    ifpData :: !ByteString,
    ifpDeliverAt :: !MonoTime -- When this packet should be delivered
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''InFlightPacket

-- | Nanoseconds per millisecond.
nsPerMs :: Word64
nsPerMs = 1000000

-- | Test network configuration.
data TestNetConfig = TestNetConfig
  { tncLatencyNs :: !Word64, -- Simulated one-way latency in nanoseconds
    tncLossRate :: !Double, -- Packet loss probability (0.0 - 1.0)
    tncJitterNs :: !Word64 -- Random jitter range in nanoseconds
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''TestNetConfig

-- | Default test config (no latency, no loss).
defaultTestNetConfig :: TestNetConfig
defaultTestNetConfig =
  TestNetConfig
    { tncLatencyNs = 0,
      tncLossRate = 0.0,
      tncJitterNs = 0
    }

-- | State of the test network.
data TestNetState = TestNetState
  { tnsCurrentTime :: !MonoTime,
    tnsLocalAddr :: !SockAddr,
    tnsInFlight :: !(Seq InFlightPacket), -- Packets in transit
    tnsInbox :: !(Seq (ByteString, SockAddr)), -- Delivered packets
    tnsConfig :: !TestNetConfig,
    tnsRng :: !Word64,
    tnsClosed :: !Bool
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''TestNetState

-- | Create initial test network state.
initialTestNetState :: SockAddr -> TestNetState
initialTestNetState localAddr =
  TestNetState
    { tnsCurrentTime = 0,
      tnsLocalAddr = localAddr,
      tnsInFlight = Seq.empty,
      tnsInbox = Seq.empty,
      tnsConfig = defaultTestNetConfig,
      tnsRng = 42, -- Deterministic seed
      tnsClosed = False
    }

-- | The test network monad.
newtype TestNet a = TestNet {unTestNet :: State TestNetState a}
  deriving (Functor, Applicative, Monad, MonadState TestNetState)

-- | Run a test network computation.
runTestNet :: TestNet a -> TestNetState -> (a, TestNetState)
runTestNet (TestNet m) = runState m

instance MonadTime TestNet where
  getMonoTime = use #tnsCurrentTime

instance MonadNetwork TestNet where
  netSend toAddr bytes = do
    st <- get
    if tnsClosed st
      then pure (Left NetSocketClosed)
      else do
        let cfg = tnsConfig st
            (r1, rng') = nextRandom (tnsRng st)
        if randomDouble r1 < tncLossRate cfg
          then do
            #tnsRng .= rng'
            pure (Right ())
          else do
            let (r2, rng'') = nextRandom rng'
                jitterRange = tncJitterNs cfg
                jitter = if jitterRange == 0 then 0 else r2 `mod` (jitterRange + 1)
                deliverAt = tnsCurrentTime st + MonoTime (tncLatencyNs cfg) + MonoTime jitter
                pkt =
                  InFlightPacket
                    { ifpFrom = tnsLocalAddr st,
                      ifpTo = toAddr,
                      ifpData = bytes,
                      ifpDeliverAt = deliverAt
                    }
            #tnsInFlight %= (Seq.|> pkt)
            #tnsRng .= rng''
            pure (Right ())

  netRecv = do
    inbox <- use #tnsInbox
    case Seq.viewl inbox of
      Seq.EmptyL -> pure Nothing
      (bytes, from) Seq.:< rest -> do
        #tnsInbox .= rest
        -- Validate and strip CRC, matching the IO backend behavior
        case validateAndStripCrc32 bytes of
          Nothing -> pure Nothing -- Drop corrupt packets
          Just validated -> pure (Just (validated, from))

  netClose = #tnsClosed .= True

-- | Advance time and deliver packets that are ready.
advanceTime :: MonoTime -> TestNet ()
advanceTime newTime = do
  st <- get
  let (ready, stillInFlight) =
        Seq.partition (\p -> ifpDeliverAt p <= newTime) (tnsInFlight st)
      -- Only deliver packets addressed to us
      delivered =
        (\p -> (ifpData p, ifpFrom p))
          <$> Seq.filter (\p -> ifpTo p == tnsLocalAddr st) ready
  #tnsCurrentTime .= newTime
  #tnsInFlight .= stillInFlight
  #tnsInbox %= (Seq.>< delivered)

-- | Configure simulated one-way latency (in milliseconds).
simulateLatency :: Word64 -> TestNet ()
simulateLatency ms = #tnsConfig % #tncLatencyNs .= ms * nsPerMs

-- | Configure simulated packet loss.
simulateLoss :: Double -> TestNet ()
simulateLoss rate = #tnsConfig % #tncLossRate .= rate

-- | Get all packets currently in flight (for debugging/testing).
getPendingPackets :: TestNet [InFlightPacket]
getPendingPackets = toList <$> use #tnsInFlight

-- -----------------------------------------------------------------------------
-- Multi-peer world simulation
-- -----------------------------------------------------------------------------

-- | A world containing multiple peers for integration testing.
-- Allows deterministic simulation of multi-peer scenarios.
data TestWorld = TestWorld
  { -- | State for each peer, keyed by their address
    twPeers :: !(Map.Map SockAddr TestNetState),
    -- | Global simulation time
    twGlobalTime :: !MonoTime
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''TestWorld

-- | Create a new test world with no peers.
newTestWorld :: TestWorld
newTestWorld =
  TestWorld
    { twPeers = Map.empty,
      twGlobalTime = 0
    }

-- | Run a TestNet operation for a specific peer in the world.
-- If the peer doesn't exist, it's created with the given address.
runPeerInWorld :: SockAddr -> TestNet a -> TestWorld -> (a, TestWorld)
runPeerInWorld addr action world =
  let peerState =
        Map.findWithDefault
          (initialTestNetState addr) {tnsCurrentTime = twGlobalTime world}
          addr
          (twPeers world)
      (result, peerState') = runTestNet action peerState
      world' = world & #twPeers %~ Map.insert addr peerState'
   in (result, world')

-- | Deliver all ready packets between peers.
-- Packets are moved from sender's outFlight to receiver's inbox
-- if the delivery time has passed.
deliverPackets :: TestWorld -> TestWorld
deliverPackets world =
  let time = twGlobalTime world
      -- Collect all packets from all peers
      allPackets =
        concatMap
          (toList . tnsInFlight)
          (Map.elems (twPeers world))
      -- Partition into ready and not ready
      (ready, notReady) = partition (\p -> ifpDeliverAt p <= time) allPackets
      -- Clear all peer outFlight, then redistribute
      clearedPeers = Map.map (\ps -> ps & #tnsInFlight .~ Seq.empty) (twPeers world)
      -- Put not-ready packets back to their senders
      peersWithPending = foldr putBackPending clearedPeers notReady
      -- Deliver ready packets to their destinations
      peersWithDelivered = foldr deliverOne peersWithPending ready
   in world & #twPeers .~ peersWithDelivered
  where
    putBackPending pkt =
      Map.adjust
        (\ps -> ps & #tnsInFlight %~ (Seq.|> pkt))
        (ifpFrom pkt)

    deliverOne pkt =
      Map.adjust
        (\ps -> ps & #tnsInbox %~ (Seq.|> (ifpData pkt, ifpFrom pkt)))
        (ifpTo pkt)

-- | Advance time for all peers in the world.
worldAdvanceTime :: MonoTime -> TestWorld -> TestWorld
worldAdvanceTime newTime world =
  let world' = world & #twGlobalTime .~ newTime
      updatedPeers =
        Map.map
          (\ps -> ps & #tnsCurrentTime .~ newTime)
          (twPeers world')
   in deliverPackets (world' & #twPeers .~ updatedPeers)
