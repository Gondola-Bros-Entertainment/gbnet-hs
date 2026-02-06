-- |
-- Module      : GBNet
-- Description : Game networking library with zero-copy Storable serialization
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
--   netState <- newNetState sock addr
--   evalNetT (gameLoop peer) netState
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
    MonoTime (..),
    NetError (..),

    -- * Domain Types
    ChannelId (..),
    channelIdToInt,
    SequenceNum (..),
    MessageId (..),

    -- * Network Monad
    NetT,
    runNetT,
    evalNetT,
    execNetT,
    NetState,
    newNetState,

    -- * IO Backend
    initNetState,
    getMonoTimeIO,

    -- * Socket Address Utilities
    SockAddr (..),
    localhost,
    ipv4,
    anyAddr,

    -- * Socket
    SocketError (..),
    UdpSocket,

    -- * Peer Networking
    NetPeer,
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

    -- ** Pure Processing Types
    PeerResult (..),
    IncomingPacket (..),
    RawPacket (..),

    -- ** Peer Queries
    peerCount,
    peerIsConnected,
    peerStats,
    peerLocalAddr,
    peerConnectedIds,
    peerConfig,

    -- * Connection
    ConnectionState (..),
    ConnectionError (..),
    ChannelError (..),

    -- * Configuration
    NetworkConfig (..),
    defaultNetworkConfig,
    ConfigError (..),
    validateConfig,
    SimulationConfig (..),
    defaultSimulationConfig,
    ChannelConfig (..),
    defaultChannelConfig,
    unreliableConfig,
    reliableOrderedConfig,
    reliableSequencedConfig,
    DeliveryMode (..),

    -- * Simulation
    NetworkSimulator,
    newNetworkSimulator,
    simulatorProcessSend,
    simulatorReceiveReady,
    simulatorPendingCount,
    simulatorConfig,

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

    -- * Serialization (Storable-based, zero-copy)
    serialize,
    deserialize,
    deriveStorable,

    -- * Statistics
    NetworkStats (..),
    CongestionLevel (..),

    -- * Replication: Delta Compression
    NetworkDelta (..),
    BaselineSeq,
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
    getPriority,

    -- * Replication: Snapshot Interpolation
    Interpolatable (..),
    SnapshotBuffer,
    newSnapshotBuffer,
    newSnapshotBufferWithConfig,
    pushSnapshot,
    sampleSnapshot,
    snapshotReady,
  )
where

import Data.Word (Word16, Word8)
import GBNet.Channel (ChannelConfig (..), ChannelError (..), DeliveryMode (..), defaultChannelConfig, reliableOrderedConfig, reliableSequencedConfig, unreliableConfig)
import GBNet.Class
import GBNet.Config (ConfigError (..), NetworkConfig (..), SimulationConfig (..), defaultNetworkConfig, defaultSimulationConfig, validateConfig)
import GBNet.Connection (ConnectionError (..), ConnectionState (..), DisconnectReason (..))
import GBNet.Net
import GBNet.Net.IO (initNetState)
import GBNet.Peer
import GBNet.Replication.Delta (BaselineManager, BaselineSeq, DeltaTracker, NetworkDelta (..), deltaDecode, deltaEncode, deltaOnAck, getBaseline, newBaselineManager, newDeltaTracker, pushBaseline)
import GBNet.Replication.Interest (GridInterest, InterestManager (..), RadiusInterest, newGridInterest, newRadiusInterest)
import qualified GBNet.Replication.Interest as Interest
import GBNet.Replication.Interpolation (Interpolatable (..), SnapshotBuffer, newSnapshotBuffer, newSnapshotBufferWithConfig, pushSnapshot, sampleSnapshot, snapshotReady)
import GBNet.Replication.Priority (PriorityAccumulator, accumulate, drainTop, getPriority, newPriorityAccumulator, register, unregister)
import GBNet.Serialize (deserialize, serialize)
import GBNet.Serialize.TH (deriveStorable)
import GBNet.Simulator (NetworkSimulator, newNetworkSimulator, simulatorConfig, simulatorPendingCount, simulatorProcessSend, simulatorReceiveReady)
import GBNet.Socket (SocketError (..), UdpSocket)
import GBNet.Stats (CongestionLevel (..), NetworkStats (..))
import GBNet.TestNet
import GBNet.Types
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

-- | Get the network configuration from a peer.
peerConfig :: NetPeer -> NetworkConfig
peerConfig = npConfig
