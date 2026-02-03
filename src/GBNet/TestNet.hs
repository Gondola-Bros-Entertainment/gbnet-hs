{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    worldGetPeerState,
  )
where

import Control.Monad.State.Strict (MonadState, State, get, gets, modify', put, runState)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GBNet.Class (MonadNetwork (..), MonadTime (..), MonoTime, NetError (..))
import Network.Socket (SockAddr)
import System.Random (StdGen, mkStdGen, randomR)

-- | A packet in transit.
data InFlightPacket = InFlightPacket
  { ifpFrom :: !SockAddr,
    ifpTo :: !SockAddr,
    ifpData :: !ByteString,
    ifpDeliverAt :: !MonoTime -- When this packet should be delivered
  }
  deriving (Show)

-- | Test network configuration.
data TestNetConfig = TestNetConfig
  { tncLatencyMs :: !Word64, -- Simulated one-way latency
    tncLossRate :: !Double, -- Packet loss probability (0.0 - 1.0)
    tncJitterMs :: !Word64 -- Random jitter range
  }
  deriving (Show)

-- | Default test config (no latency, no loss).
defaultTestNetConfig :: TestNetConfig
defaultTestNetConfig =
  TestNetConfig
    { tncLatencyMs = 0,
      tncLossRate = 0.0,
      tncJitterMs = 0
    }

-- | State of the test network.
data TestNetState = TestNetState
  { tnsCurrentTime :: !MonoTime,
    tnsLocalAddr :: !SockAddr,
    tnsInFlight :: !(Seq InFlightPacket), -- Packets in transit
    tnsInbox :: !(Seq (ByteString, SockAddr)), -- Delivered packets
    tnsConfig :: !TestNetConfig,
    tnsRng :: !StdGen,
    tnsClosed :: !Bool
  }
  deriving (Show)

-- | Create initial test network state.
initialTestNetState :: SockAddr -> TestNetState
initialTestNetState localAddr =
  TestNetState
    { tnsCurrentTime = 0,
      tnsLocalAddr = localAddr,
      tnsInFlight = Seq.empty,
      tnsInbox = Seq.empty,
      tnsConfig = defaultTestNetConfig,
      tnsRng = mkStdGen 42, -- Deterministic seed
      tnsClosed = False
    }

-- | The test network monad.
newtype TestNet a = TestNet {unTestNet :: State TestNetState a}
  deriving (Functor, Applicative, Monad, MonadState TestNetState)

-- | Run a test network computation.
runTestNet :: TestNet a -> TestNetState -> (a, TestNetState)
runTestNet (TestNet m) = runState m

instance MonadTime TestNet where
  getMonoTime = gets tnsCurrentTime

instance MonadNetwork TestNet where
  netSend toAddr bytes = do
    st <- get
    if tnsClosed st
      then pure (Left NetSocketClosed)
      else do
        let cfg = tnsConfig st
            (shouldDrop, rng') = randomR (0.0, 1.0) (tnsRng st) :: (Double, StdGen)
        if shouldDrop < tncLossRate cfg
          then do
            -- Packet lost
            put st {tnsRng = rng'}
            pure (Right ())
          else do
            -- Calculate delivery time
            let (jitter, rng'') = randomR (0, tncJitterMs cfg) rng'
                deliverAt = tnsCurrentTime st + tncLatencyMs cfg + jitter
                pkt =
                  InFlightPacket
                    { ifpFrom = tnsLocalAddr st,
                      ifpTo = toAddr,
                      ifpData = bytes,
                      ifpDeliverAt = deliverAt
                    }
            put st {tnsInFlight = tnsInFlight st Seq.|> pkt, tnsRng = rng''}
            pure (Right ())

  netRecv = do
    st <- get
    case Seq.viewl (tnsInbox st) of
      Seq.EmptyL -> pure Nothing
      (bytes, from) Seq.:< rest -> do
        put st {tnsInbox = rest}
        pure (Just (bytes, from))

  netClose = modify' $ \st -> st {tnsClosed = True}

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
  put
    st
      { tnsCurrentTime = newTime,
        tnsInFlight = stillInFlight,
        tnsInbox = tnsInbox st Seq.>< delivered
      }

-- | Configure simulated latency.
simulateLatency :: Word64 -> TestNet ()
simulateLatency ms = modify' $ \st ->
  st {tnsConfig = (tnsConfig st) {tncLatencyMs = ms}}

-- | Configure simulated packet loss.
simulateLoss :: Double -> TestNet ()
simulateLoss rate = modify' $ \st ->
  st {tnsConfig = (tnsConfig st) {tncLossRate = rate}}

-- | Get all packets currently in flight (for debugging/testing).
getPendingPackets :: TestNet [InFlightPacket]
getPendingPackets = gets (toList . tnsInFlight)

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
      world' = world {twPeers = Map.insert addr peerState' (twPeers world)}
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
      clearedPeers = Map.map (\ps -> ps {tnsInFlight = Seq.empty}) (twPeers world)
      -- Put not-ready packets back to their senders
      peersWithPending = foldr putBackPending clearedPeers notReady
      -- Deliver ready packets to their destinations
      peersWithDelivered = foldr deliverOne peersWithPending ready
   in world {twPeers = peersWithDelivered}
  where
    putBackPending pkt =
      Map.adjust
        (\ps -> ps {tnsInFlight = tnsInFlight ps Seq.|> pkt})
        (ifpFrom pkt)

    deliverOne pkt =
      Map.adjust
        ( \ps ->
            ps {tnsInbox = tnsInbox ps Seq.|> (ifpData pkt, ifpFrom pkt)}
        )
        (ifpTo pkt)

-- | Advance time for all peers in the world.
worldAdvanceTime :: MonoTime -> TestWorld -> TestWorld
worldAdvanceTime newTime world =
  let world' = world {twGlobalTime = newTime}
      updatedPeers =
        Map.map
          (\ps -> ps {tnsCurrentTime = newTime})
          (twPeers world')
   in deliverPackets world' {twPeers = updatedPeers}

-- | Get a peer's state from the world (for inspection in tests).
worldGetPeerState :: SockAddr -> TestWorld -> Maybe TestNetState
worldGetPeerState addr world = Map.lookup addr (twPeers world)
