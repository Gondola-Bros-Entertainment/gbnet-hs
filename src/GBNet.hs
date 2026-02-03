-- |
-- Module      : GBNet
-- Description : Game networking library with bitpacked serialization
--
-- This is the main entry point for gbnet-hs. Import this module for
-- convenient access to the full API.
--
-- @
-- import GBNet
--
-- main = do
--   let addr = localhost 7777
--   Right (peer, sock) <- newPeer addr defaultNetworkConfig =<< getMonoTimeIO
--   evalNetT (gameLoop peer) (newNetState sock addr)
-- @
--
-- For explicit imports, use the individual modules:
--
-- @
-- import GBNet.Class (MonadNetwork, MonadTime)
-- import GBNet.Net (NetT, runNetT, NetState)
-- import GBNet.Peer (NetPeer, peerTick, PeerEvent(..))
-- @
module GBNet
  ( -- * Effect Classes
    MonadTime (..),
    MonadNetwork (..),
    MonoTime,
    NetError (..),

    -- * Network Monad
    NetT,
    runNetT,
    evalNetT,
    execNetT,
    NetState (..),
    newNetState,

    -- * IO Backend
    initNetState,
    getMonoTimeIO,

    -- * Socket Address Utilities
    SockAddr (..),
    localhost,
    ipv4,
    anyAddr,

    -- * Peer Networking
    NetPeer (..),
    PeerId (..),
    peerIdFromAddr,
    PeerEvent (..),
    ConnectionDirection (..),
    DisconnectReason (..),

    -- ** Peer Creation
    newPeer,
    newPeerState,

    -- ** Peer Operations (Polymorphic)
    peerTick,
    peerRecvAllM,
    peerSendAllM,
    peerShutdownM,

    -- ** Peer Operations (Pure)
    peerConnect,
    peerDisconnect,
    peerProcess,
    peerSend,
    peerBroadcast,

    -- ** Peer Queries
    peerCount,
    peerIsConnected,
    peerStats,
    peerLocalAddr,
    peerConnectedIds,

    -- * Configuration
    NetworkConfig (..),
    defaultNetworkConfig,
    ChannelConfig (..),
    defaultChannelConfig,
    DeliveryMode (..),

    -- * Testing
    TestNet,
    runTestNet,
    TestNetState (..),
    initialTestNetState,
    TestWorld (..),
    newTestWorld,
    runPeerInWorld,
    deliverPackets,
    worldAdvanceTime,

    -- * Serialization
    BitSerialize (..),
    BitDeserialize (..),
    BitBuffer,
    empty,
    toBytes,
    fromBytes,

    -- * Statistics
    NetworkStats (..),

    -- * Replication: Delta Compression
    NetworkDelta (..),
    DeltaTracker,
    newDeltaTracker,
    deltaEncode,
    deltaOnAck,
    BaselineManager,
    newBaselineManager,
    pushBaseline,
    getBaseline,
    deltaDecode,

    -- * Replication: Interest Management
    InterestManager (..),
    RadiusInterest,
    newRadiusInterest,
    GridInterest,
    newGridInterest,
    filterRelevant,

    -- * Replication: Priority Accumulator
    PriorityAccumulator,
    newPriorityAccumulator,
    register,
    unregister,
    accumulate,
    drainTop,

    -- * Replication: Snapshot Interpolation
    Interpolatable (..),
    SnapshotBuffer,
    newSnapshotBuffer,
    pushSnapshot,
    sampleSnapshot,
  )
where

import Data.Word (Word16, Word8)
import GBNet.Channel (ChannelConfig (..), DeliveryMode (..), defaultChannelConfig)
import GBNet.Class
import GBNet.Config (NetworkConfig (..), defaultNetworkConfig)
import GBNet.Connection (DisconnectReason (..))
import GBNet.Delta (BaselineManager, DeltaTracker, NetworkDelta (..), deltaDecode, deltaEncode, deltaOnAck, getBaseline, newBaselineManager, newDeltaTracker, pushBaseline)
import GBNet.Interest (GridInterest, InterestManager (..), RadiusInterest, newGridInterest, newRadiusInterest)
import qualified GBNet.Interest as Interest
import GBNet.Interpolation (Interpolatable (..), SnapshotBuffer, newSnapshotBuffer, pushSnapshot, sampleSnapshot)
import GBNet.Net
import GBNet.Net.IO (initNetState)
import GBNet.Peer
import GBNet.Priority (PriorityAccumulator, accumulate, drainTop, newPriorityAccumulator, register, unregister)
import GBNet.Serialize.BitBuffer (BitBuffer, empty, fromBytes, toBytes)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import GBNet.Stats (NetworkStats (..))
import GBNet.TestNet
import Network.Socket (SockAddr (..))
import qualified Network.Socket as NS

-- | Create a localhost address on the given port.
--
-- @
-- let addr = localhost 7777
-- @
localhost :: Word16 -> SockAddr
localhost port = SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127, 0, 0, 1))

-- | Create an IPv4 address from tuple and port.
--
-- @
-- let addr = ipv4 (192, 168, 1, 100) 7777
-- @
ipv4 :: (Word8, Word8, Word8, Word8) -> Word16 -> SockAddr
ipv4 (a, b, c, d) port =
  SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (a, b, c, d))

-- | Bind to any interface on the given port.
--
-- @
-- let addr = anyAddr 7777
-- @
anyAddr :: Word16 -> SockAddr
anyAddr port = SockAddrInet (fromIntegral port) 0

-- | Filter entities to only those relevant to an observer.
--
-- @
-- let nearby = filterRelevant interest observerPos entities
-- @
filterRelevant ::
  (InterestManager a) =>
  a ->
  Interest.Position ->
  [(Interest.Position, b)] ->
  [b]
filterRelevant mgr observerPos =
  map snd . filter (\(entPos, _) -> Interest.relevant mgr entPos observerPos)
