-- |
-- Module      : GBNet
-- Description : Game networking library with bitpacked serialization
--
-- This is the main entry point for gbnet-hs. Import this module for
-- convenient access to the full API.
--
-- For explicit imports, use the individual modules:
--
-- @
-- import GBNet.Class (MonadNetwork, MonadTime)
-- import GBNet.Net (NetT, runNetT, NetState)
-- import GBNet.Net.IO (initNetState)
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

    -- * Peer Networking
    NetPeer (..),
    PeerId (..),
    peerIdFromAddr,
    PeerEvent (..),
    ConnectionDirection (..),

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
  )
where

import GBNet.Channel (ChannelConfig (..), DeliveryMode (..), defaultChannelConfig)
import GBNet.Class
import GBNet.Config (NetworkConfig (..), defaultNetworkConfig)
import GBNet.Net
import GBNet.Net.IO (initNetState)
import GBNet.Peer
import GBNet.Serialize.BitBuffer (BitBuffer, empty, fromBytes, toBytes)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import GBNet.Stats (NetworkStats (..))
import GBNet.TestNet
