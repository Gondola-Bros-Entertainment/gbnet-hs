{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Peer.Internal
-- Description : Shared types, constants, and helpers for Peer sub-modules
--
-- This internal module provides the core types ('NetPeer', 'PeerId', etc.),
-- named constants, and small helpers used by all Peer sub-modules.
-- It is not exposed to library consumers; import 'GBNet.Peer' instead.
module GBNet.Peer.Internal
  ( -- * Peer identifier
    PeerId (..),
    peerIdFromAddr,

    -- * Connection direction
    ConnectionDirection (..),

    -- * Events
    PeerEvent (..),

    -- * Pure processing types
    IncomingPacket (..),
    RawPacket (..),
    PeerResult (..),

    -- * Pending connection
    PendingConnection (..),

    -- * Net peer
    NetPeer (..),
    newPeerState,

    -- * Constants
    cookieSecretSize,
    fragmentTimeoutMs,
    fragmentMaxBufferSize,

    -- * Helpers
    withConnection,
    queueRawPacket,
    drainPeerSendQueue,
    queueControlPacket,
    cleanupPeer,
    removePending,
    generateCookieSecret,
  )
where

import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64, Word8)
import GBNet.Class (MonoTime (..))
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection
  ( Connection,
    DisconnectReason (..),
  )
import GBNet.Fragment (FragmentAssembler)
import GBNet.Packet
  ( Packet (..),
    PacketHeader (..),
    PacketType,
    serializePacket,
  )
import GBNet.Security (RateLimiter, appendCrc32, newRateLimiter)
import GBNet.Socket (UdpSocket)
import GBNet.Types (ChannelId (..))
import GBNet.Util (nextRandom)
import Network.Socket (SockAddr)
import Optics ((%~), (&), (.~))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Peer identifier wrapping a socket address.
newtype PeerId = PeerId {unPeerId :: SockAddr}
  deriving (Eq, Ord, Show)

-- | Create a PeerId from a SockAddr.
peerIdFromAddr :: SockAddr -> PeerId
peerIdFromAddr = PeerId

-- | Direction of connection establishment.
data ConnectionDirection
  = -- | They connected to us
    Inbound
  | -- | We connected to them
    Outbound
  deriving (Eq, Show)

-- | Events emitted by peer processing.
data PeerEvent
  = -- | A peer connected
    PeerConnected !PeerId !ConnectionDirection
  | -- | A peer disconnected
    PeerDisconnected !PeerId !DisconnectReason
  | -- | Received a message from a peer
    PeerMessage !PeerId !ChannelId !BS.ByteString
  | -- | A peer's address changed (connection migration)
    PeerMigrated !PeerId !PeerId -- old, new
  deriving (Eq, Show)

-- | An incoming packet from the network (CRC already validated and stripped).
data IncomingPacket = IncomingPacket
  { ipFrom :: !PeerId,
    -- | CRC-validated payload
    ipData :: !BS.ByteString
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''IncomingPacket

-- | An outgoing packet ready to send (with CRC appended).
data RawPacket = RawPacket
  { rpTo :: !PeerId,
    -- | Data with CRC appended
    rpData :: !BS.ByteString
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''RawPacket

-- | State of a pending connection (mid-handshake).
data PendingConnection = PendingConnection
  { pcDirection :: !ConnectionDirection,
    pcServerSalt :: !Word64,
    pcClientSalt :: !Word64,
    pcCreatedAt :: !MonoTime,
    pcRetryCount :: !Int,
    pcLastRetry :: !MonoTime
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''PendingConnection

-- | Cookie secret size in bytes.
cookieSecretSize :: Int
cookieSecretSize = 32

-- | Fragment reassembly timeout in milliseconds.
fragmentTimeoutMs :: Double
fragmentTimeoutMs = 5000.0

-- | Fragment reassembly max buffer size.
fragmentMaxBufferSize :: Int
fragmentMaxBufferSize = 1024 * 1024 -- 1MB

-- | A network peer that can accept and initiate connections.
data NetPeer = NetPeer
  { npSocket :: !UdpSocket,
    -- | Local address this peer is bound to (for polymorphic operations)
    npLocalAddr :: !SockAddr,
    npConnections :: !(Map PeerId Connection),
    npPending :: !(Map PeerId PendingConnection),
    npConfig :: !NetworkConfig,
    npRateLimiter :: !RateLimiter,
    npCookieSecret :: !BS.ByteString,
    npRngState :: !Word64,
    npFragmentAssemblers :: !(Map PeerId FragmentAssembler),
    -- | Tracks last migration time per connection to rate-limit migrations
    npMigrationCooldowns :: !(Map Word64 MonoTime),
    -- | Queued outgoing control packets (for pure API)
    npSendQueue :: !(Seq RawPacket),
    -- | Number of packets dropped by rate limiting
    npRateLimitDrops :: !Word64
  }

makeFieldLabelsNoPrefix ''NetPeer

-- | Result of pure packet processing.
data PeerResult = PeerResult
  { -- | Updated peer state
    prPeer :: !NetPeer,
    -- | Events that occurred during processing
    prEvents :: ![PeerEvent],
    -- | Packets to send (call @peerSendAllM@ with these)
    prOutgoing :: ![RawPacket]
  }

makeFieldLabelsNoPrefix ''PeerResult

-- | Create peer state (pure). Used internally and for polymorphic backends.
newPeerState ::
  UdpSocket ->
  SockAddr ->
  NetworkConfig ->
  MonoTime ->
  NetPeer
newPeerState sock localAddr config now =
  let rng0 = unMonoTime now
      (secret, rng1) = generateCookieSecret rng0
   in NetPeer
        { npSocket = sock,
          npLocalAddr = localAddr,
          npConnections = Map.empty,
          npPending = Map.empty,
          npConfig = config,
          npRateLimiter = newRateLimiter (ncRateLimitPerSecond config) now,
          npCookieSecret = secret,
          npRngState = rng1,
          npFragmentAssemblers = Map.empty,
          npMigrationCooldowns = Map.empty,
          npSendQueue = Seq.empty,
          npRateLimitDrops = 0
        }

-- | Generate a pseudo-random cookie secret.
generateCookieSecret :: Word64 -> (BS.ByteString, Word64)
generateCookieSecret seed = go seed cookieSecretSize []
  where
    go s 0 acc = (BS.pack (reverse acc), s)
    go s n acc =
      let (r, s') = nextRandom s
       in go s' (n - 1) (fromIntegral @Word64 @Word8 r : acc)

-- -----------------------------------------------------------------------------
-- Helper functions for pure API
-- -----------------------------------------------------------------------------

-- | Update a connection by PeerId, returning unchanged peer if not found.
withConnection :: PeerId -> (Connection -> Connection) -> NetPeer -> NetPeer
withConnection pid f peer = peer & #npConnections %~ Map.adjust f pid
{-# INLINE withConnection #-}

-- | Queue a raw packet for sending.
queueRawPacket :: RawPacket -> NetPeer -> NetPeer
queueRawPacket pkt peer = peer & #npSendQueue %~ (Seq.|> pkt)

-- | Drain the peer's send queue.
drainPeerSendQueue :: NetPeer -> ([RawPacket], NetPeer)
drainPeerSendQueue peer =
  (toList (npSendQueue peer), peer & #npSendQueue .~ Seq.empty)

-- | Serialize and queue a control packet.
queueControlPacket :: PacketType -> BS.ByteString -> PeerId -> NetPeer -> NetPeer
queueControlPacket ptype payload pid peer =
  let header =
        PacketHeader
          { packetType = ptype,
            sequenceNum = 0,
            ack = 0,
            ackBitfield = 0
          }
      pkt = Packet {pktHeader = header, pktPayload = payload}
      raw = appendCrc32 (serializePacket pkt)
   in queueRawPacket (RawPacket pid raw) peer

-- | Clean up per-peer state (fragment assemblers, migration cooldowns).
cleanupPeer :: PeerId -> NetPeer -> NetPeer
cleanupPeer peerId peer =
  peer & #npFragmentAssemblers %~ Map.delete peerId

-- | Remove a peer from pending.
removePending :: PeerId -> NetPeer -> NetPeer
removePending peerId peer = peer & #npPending %~ Map.delete peerId
