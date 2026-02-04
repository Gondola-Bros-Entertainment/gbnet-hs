-- |
-- Module      : GBNet.Peer
-- Description : Unified peer networking API
--
-- NetPeer provides a symmetric abstraction for game networking.
-- A peer can accept incoming connections (server-like), initiate
-- outgoing connections (client-like), or do both (P2P/mesh).
--
-- Polymorphic game loop pattern:
--
-- @
-- gameLoop peer = do
--   (events, peer') <- peerTick [(channel, msg)] peer
--   -- handle events
--   gameLoop peer'
-- @
module GBNet.Peer
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

    -- * Net peer
    NetPeer (..),
    newPeer,
    newPeerState,

    -- * Connection management
    peerConnect,
    peerDisconnect,

    -- * Pure processing
    peerProcess,

    -- * Polymorphic IO helpers
    peerRecvAllM,
    peerSendAllM,
    peerShutdownM,
    peerTick,

    -- * Internal (used by pure processing)
    drainPeerSendQueue,
    drainAllConnectionQueues,

    -- * Sending
    peerSend,
    peerBroadcast,

    -- * Queries
    peerCount,
    peerIsConnected,
    peerStats,
    peerLocalAddr,
    peerConnectedIds,
  )
where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64, Word8)
import GBNet.Class (MonadNetwork (..), MonadTime (..), MonoTime)
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection
  ( Connection,
    ConnectionError (..),
    DisconnectReason (..),
    OutgoingPacket (..),
    newConnection,
    processIncomingHeader,
  )
import qualified GBNet.Connection as Conn
import GBNet.Fragment
  ( FragmentAssembler,
    newFragmentAssembler,
    processFragment,
  )
import GBNet.Packet
  ( Packet (..),
    PacketHeader (..),
    PacketType (..),
    deserializePacket,
    serializePacket,
  )
import GBNet.Reliability (elapsedMs)
import GBNet.Security (RateLimiter, appendCrc32, newRateLimiter, rateLimiterAllow)
import GBNet.Serialize.BitBuffer (ReadResult (..), empty, fromBytes, toBytes)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import GBNet.Socket
  ( SocketError (..),
    UdpSocket,
    newUdpSocket,
    socketLocalAddr,
  )
import GBNet.Stats (NetworkStats)
import Network.Socket (SockAddr)

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

-- | Events emitted by 'peerProcess'.
data PeerEvent
  = -- | A peer connected
    PeerConnected !PeerId !ConnectionDirection
  | -- | A peer disconnected
    PeerDisconnected !PeerId !DisconnectReason
  | -- | Received a message from a peer
    PeerMessage !PeerId !Word8 !BS.ByteString
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

-- | An outgoing packet ready to send (with CRC appended).
data RawPacket = RawPacket
  { rpTo :: !PeerId,
    -- | Data with CRC appended
    rpData :: !BS.ByteString
  }
  deriving (Eq, Show)

-- | Result of pure packet processing via 'peerProcess'.
data PeerResult = PeerResult
  { -- | Updated peer state
    prPeer :: !NetPeer,
    -- | Events that occurred during processing
    prEvents :: ![PeerEvent],
    -- | Packets to send (call 'peerSendAllM' with these)
    prOutgoing :: ![RawPacket]
  }

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

-- | Cookie secret size in bytes.
cookieSecretSize :: Int
cookieSecretSize = 32

-- | Fragment reassembly timeout in milliseconds.
fragmentTimeoutMs :: Double
fragmentTimeoutMs = 5000.0

-- | Fragment reassembly max buffer size.
fragmentMaxBufferSize :: Int
fragmentMaxBufferSize = 1024 * 1024 -- 1MB

-- | Migration cooldown in milliseconds.
migrationCooldownMs :: Double
migrationCooldownMs = 5000.0

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
    npSendQueue :: !(Seq RawPacket)
  }

-- | Create a new peer bound to the given address.
-- Returns the peer and socket. The socket is also stored in the peer for
-- backward compatibility, but new code should use the polymorphic API.
newPeer ::
  SockAddr ->
  NetworkConfig ->
  MonoTime ->
  IO (Either SocketError (NetPeer, UdpSocket))
newPeer addr config now = do
  socketResult <- newUdpSocket addr
  case socketResult of
    Left err -> return $ Left err
    Right sock -> do
      localAddrResult <- socketLocalAddr sock
      let localAddr = case localAddrResult of
            Left _ -> addr -- Fallback to bind address
            Right a -> a
      let peer = newPeerState sock localAddr config now
      return $ Right (peer, sock)

-- | Create peer state (pure). Used internally and for polymorphic backends.
newPeerState ::
  UdpSocket ->
  SockAddr ->
  NetworkConfig ->
  MonoTime ->
  NetPeer
newPeerState sock localAddr config now =
  let rng0 = now
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
          npSendQueue = Seq.empty
        }

-- | Generate a pseudo-random cookie secret.
generateCookieSecret :: Word64 -> (BS.ByteString, Word64)
generateCookieSecret seed = go seed cookieSecretSize []
  where
    go s 0 acc = (BS.pack (reverse acc), s)
    go s n acc =
      let (r, s') = nextRandom s
       in go s' (n - 1) (fromIntegral (r `mod` 256) : acc)

-- | Simple LCG random number generator.
nextRandom :: Word64 -> (Word64, Word64)
nextRandom s =
  let a = 6364136223846793005
      c = 1442695040888963407
      next = a * s + c
   in (next, next)

-- -----------------------------------------------------------------------------
-- Helper functions for pure API
-- -----------------------------------------------------------------------------

-- | Queue a raw packet for sending.
queueRawPacket :: RawPacket -> NetPeer -> NetPeer
queueRawPacket pkt peer = peer {npSendQueue = npSendQueue peer Seq.|> pkt}

-- | Drain the peer's send queue.
drainPeerSendQueue :: NetPeer -> ([RawPacket], NetPeer)
drainPeerSendQueue peer =
  (toList (npSendQueue peer), peer {npSendQueue = Seq.empty})

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

-- -----------------------------------------------------------------------------
-- Connection management
-- -----------------------------------------------------------------------------

-- | Initiate an outgoing connection to a peer (pure).
peerConnect :: PeerId -> MonoTime -> NetPeer -> NetPeer
peerConnect peerId now peer
  | Map.member peerId (npConnections peer) = peer -- Already connected
  | Map.member peerId (npPending peer) = peer -- Already pending
  | otherwise =
      -- Generate client salt
      let (salt, rng') = nextRandom (npRngState peer)
          pending =
            PendingConnection
              { pcDirection = Outbound,
                pcServerSalt = 0, -- Will be filled when we receive challenge
                pcClientSalt = salt,
                pcCreatedAt = now,
                pcRetryCount = 0,
                pcLastRetry = now
              }
          peer' =
            peer
              { npPending = Map.insert peerId pending (npPending peer),
                npRngState = rng'
              }
       in -- Queue connection request
          queueControlPacket ConnectionRequest BS.empty peerId peer'

-- | Disconnect a specific peer (pure).
peerDisconnect :: PeerId -> MonoTime -> NetPeer -> NetPeer
peerDisconnect peerId _now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> peer
    Just _ ->
      let peer' = queueControlPacket Disconnect BS.empty peerId peer
       in peer' {npConnections = Map.delete peerId (npConnections peer')}

-- -----------------------------------------------------------------------------
-- Pure processing
-- -----------------------------------------------------------------------------

-- | Pure packet processing function.
-- Given the current time and a list of incoming packets, returns the updated
-- peer state, events that occurred, and packets to send.
--
-- This is the core of the game loop - it's completely pure and deterministic.
-- Use 'peerRecvAllM' to get incoming packets and 'peerSendAllM' to send outgoing.
peerProcess :: MonoTime -> [IncomingPacket] -> NetPeer -> PeerResult
peerProcess now packets peer =
  let -- Convert IncomingPackets to internal format
      internalPackets = map (\ip -> (ipFrom ip, ipData ip)) packets

      -- Process incoming packets
      (events1, peer1) = processPacketsPure internalPackets now peer []

      -- Update all connections and collect messages
      (events2, peer2) = updateConnections now peer1

      -- Drain connection send queues into peer send queue
      peer3 = drainAllConnectionQueues now peer2

      -- Retry pending outbound connections
      peer4 = retryPendingConnectionsPure now peer3

      -- Cleanup expired pending connections
      (events3, peer5) = cleanupPending now peer4

      -- Drain the peer send queue to get outgoing packets
      (outgoing, peer6) = drainPeerSendQueue peer5
   in PeerResult peer6 (events1 ++ events2 ++ events3) outgoing

-- | Drain send queues from all connections into the peer's send queue.
drainAllConnectionQueues :: MonoTime -> NetPeer -> NetPeer
drainAllConnectionQueues _now peer =
  let peerIds = Map.keys (npConnections peer)
   in foldl drainConnectionQueue peer peerIds
  where
    drainConnectionQueue p peerId =
      case Map.lookup peerId (npConnections p) of
        Nothing -> p
        Just conn ->
          let (connPackets, conn') = Conn.drainSendQueue conn
              p' = p {npConnections = Map.insert peerId conn' (npConnections p)}
              -- Convert OutgoingPackets to RawPackets
              rawPackets = map (outgoingToRaw peerId) connPackets
           in foldl (flip queueRawPacket) p' rawPackets

    outgoingToRaw peerId (OutgoingPacket hdr ptype payload) =
      let header = hdr {packetType = ptype}
          pkt = Packet {pktHeader = header, pktPayload = payload}
          raw = appendCrc32 (serializePacket pkt)
       in RawPacket peerId raw

-- -----------------------------------------------------------------------------
-- Polymorphic IO helpers
-- -----------------------------------------------------------------------------

-- | Receive all available packets (polymorphic, non-blocking).
-- Returns immediately if no data is available.
peerRecvAllM :: (MonadNetwork m) => m [IncomingPacket]
peerRecvAllM = go []
  where
    go acc = do
      result <- netRecv
      case result of
        Nothing -> pure (reverse acc)
        Just (dat, addr) ->
          let pkt = IncomingPacket (PeerId addr) dat
           in go (pkt : acc)

-- | Send all outgoing packets (polymorphic).
peerSendAllM :: (MonadNetwork m) => [RawPacket] -> m ()
peerSendAllM = mapM_ sendOne
  where
    sendOne (RawPacket pid dat) = netSend (unPeerId pid) dat

-- | Shutdown the peer (polymorphic).
-- Sends disconnect packets to all connections and closes the network.
peerShutdownM :: (MonadNetwork m) => NetPeer -> m ()
peerShutdownM peer = do
  let peerIds = Map.keys (npConnections peer)
      peer' = foldr (queueControlPacket Disconnect BS.empty) peer peerIds
      (outgoing, _) = drainPeerSendQueue peer'
  peerSendAllM outgoing
  netClose

-- | Convenient single-function tick for game loops (polymorphic).
--
-- Combines receive, process, queue messages, and send into one call.
-- Takes a list of (channel, message) pairs to send and returns events.
--
-- @
-- gameLoop peer = do
--   (events, peer') <- peerTick [(ch, msg)] peer
--   -- handle events
--   gameLoop peer'
-- @
peerTick ::
  (MonadNetwork m) =>
  [(Word8, BS.ByteString)] ->
  NetPeer ->
  m ([PeerEvent], NetPeer)
peerTick messages peer = do
  now <- getMonoTime
  -- 1. Receive all available packets
  packets <- peerRecvAllM
  -- 2. Queue messages to all connections
  let peer1 = foldl (queueMessage now) peer messages
  -- 3. Process packets (pure)
  let result = peerProcess now packets peer1
      peer2 = prPeer result
      events = prEvents result
      outgoing = prOutgoing result
  -- 4. Send all outgoing packets
  peerSendAllM outgoing
  pure (events, peer2)
  where
    queueMessage now p (ch, msg) = peerBroadcast ch msg Nothing now p

-- | Process received packets (pure).
processPacketsPure ::
  [(PeerId, BS.ByteString)] ->
  MonoTime ->
  NetPeer ->
  [PeerEvent] ->
  ([PeerEvent], NetPeer)
processPacketsPure [] _ peer events = (reverse events, peer)
processPacketsPure ((peerId, dat) : rest) now peer events =
  let (newEvents, peer') = handlePacket peerId dat now peer
   in processPacketsPure rest now peer' (reverse newEvents ++ events)

-- | Handle a single received packet (pure).
handlePacket ::
  PeerId ->
  BS.ByteString ->
  MonoTime ->
  NetPeer ->
  ([PeerEvent], NetPeer)
handlePacket peerId dat now peer =
  case parsePacket dat of
    Nothing -> ([], peer)
    Just pkt ->
      let ptype = packetType (pktHeader pkt)
       in handlePacketByType peerId pkt now peer ptype

-- | Parse packet from raw data.
parsePacket :: BS.ByteString -> Maybe Packet
parsePacket dat =
  case deserializePacket dat of
    Left _ -> Nothing
    Right pkt -> Just pkt

-- | Handle a packet by its type (pure).
handlePacketByType ::
  PeerId ->
  Packet ->
  MonoTime ->
  NetPeer ->
  PacketType ->
  ([PeerEvent], NetPeer)
handlePacketByType peerId pkt now peer ptype = case ptype of
  -- Incoming connection request
  ConnectionRequest -> handleConnectionRequest peerId now peer
  -- Challenge from server (we're connecting outbound)
  ConnectionChallenge -> handleConnectionChallenge peerId pkt now peer
  -- Response to our challenge (they're connecting inbound)
  ConnectionResponse -> handleConnectionResponse peerId pkt now peer
  -- Connection accepted
  ConnectionAccepted -> handleConnectionAccepted peerId now peer
  -- Connection denied
  ConnectionDenied ->
    let reason = decodeDenyReason (pktPayload pkt)
     in ([PeerDisconnected peerId (ReasonUnknown reason)], removePending peerId peer)
  -- Disconnect
  Disconnect -> handleDisconnect peerId peer
  -- Payload from connected peer (or potential migration)
  Payload ->
    if Map.member peerId (npConnections peer)
      then handlePayload peerId pkt now peer
      else tryMigrationOrIgnore peerId pkt now peer
  -- Keepalive
  Keepalive -> handleKeepalive peerId pkt now peer

-- | Handle incoming connection request (pure).
handleConnectionRequest :: PeerId -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionRequest peerId now peer
  | Map.member peerId (npConnections peer) =
      -- Already connected, resend accept
      let peer' = queueControlPacket ConnectionAccepted BS.empty peerId peer
       in ([], peer')
  | Map.member peerId (npPending peer) =
      -- Already pending, resend challenge with stored salt
      case Map.lookup peerId (npPending peer) of
        Just pending ->
          let saltPayload = encodeSalt (pcServerSalt pending)
              peer' = queueControlPacket ConnectionChallenge saltPayload peerId peer
           in ([], peer')
        Nothing -> ([], peer)
  | otherwise =
      -- Check rate limit
      let addrKey = sockAddrToKey (unPeerId peerId)
          (allowed, rl') = rateLimiterAllow addrKey now (npRateLimiter peer)
          peer' = peer {npRateLimiter = rl'}
       in if not allowed
            then ([], peer')
            else -- Check max connections
              if Map.size (npConnections peer') >= ncMaxClients (npConfig peer')
                then
                  let reason = encodeDenyReason denyReasonServerFull
                      peer'' = queueControlPacket ConnectionDenied reason peerId peer'
                   in ([], peer'')
                else
                  -- Create pending connection and send challenge with salt
                  let (salt, rng') = nextRandom (npRngState peer')
                      pending =
                        PendingConnection
                          { pcDirection = Inbound,
                            pcServerSalt = salt,
                            pcClientSalt = 0,
                            pcCreatedAt = now,
                            pcRetryCount = 0,
                            pcLastRetry = now
                          }
                      peer'' =
                        peer'
                          { npPending = Map.insert peerId pending (npPending peer'),
                            npRngState = rng'
                          }
                      saltPayload = encodeSalt salt
                      peer''' = queueControlPacket ConnectionChallenge saltPayload peerId peer''
                   in ([], peer''')

-- | Handle connection challenge (we're outbound, received their challenge) (pure).
handleConnectionChallenge :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionChallenge peerId pkt _now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> ([], peer) -- Not expecting this
    Just pending
      | pcDirection pending /= Outbound -> ([], peer)
      | otherwise ->
          -- Extract server_salt from payload
          case decodeSalt (pktPayload pkt) of
            Nothing -> ([], peer) -- Invalid challenge
            Just serverSalt ->
              -- Store server salt and send our response with client salt
              let pending' = pending {pcServerSalt = serverSalt}
                  peer' = peer {npPending = Map.insert peerId pending' (npPending peer)}
                  saltPayload = encodeSalt (pcClientSalt pending')
                  peer'' = queueControlPacket ConnectionResponse saltPayload peerId peer'
               in ([], peer'')

-- | Handle connection response (we're inbound, received their response) (pure).
handleConnectionResponse :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionResponse peerId pkt now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> ([], peer)
    Just pending
      | pcDirection pending /= Inbound -> ([], peer)
      | otherwise ->
          -- Extract and validate client_salt from payload
          case decodeSalt (pktPayload pkt) of
            Nothing -> ([], peer) -- Invalid response
            Just clientSalt
              -- Validate: client salt must not be zero or same as server salt
              | clientSalt == 0 || clientSalt == pcServerSalt pending ->
                  let reason = encodeDenyReason denyReasonInvalidChallenge
                      peer' = queueControlPacket ConnectionDenied reason peerId peer
                   in ([], removePending peerId peer')
              | otherwise ->
                  -- Accept the connection
                  let conn = newConnection (npConfig peer) clientSalt now
                      conn' = Conn.markConnected now $ Conn.touchRecvTime now conn
                      peer' =
                        peer
                          { npConnections = Map.insert peerId conn' (npConnections peer),
                            npPending = Map.delete peerId (npPending peer)
                          }
                      peer'' = queueControlPacket ConnectionAccepted BS.empty peerId peer'
                   in ([PeerConnected peerId Inbound], peer'')

-- | Handle connection accepted (we're outbound, they accepted) (pure).
handleConnectionAccepted :: PeerId -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionAccepted peerId now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing ->
      -- Might be duplicate accept, ignore
      ([], peer)
    Just pending
      | pcDirection pending /= Outbound -> ([], peer)
      | otherwise ->
          -- Promote to connected
          let conn = newConnection (npConfig peer) (pcClientSalt pending) now
              conn' = Conn.markConnected now $ Conn.touchRecvTime now conn
              peer' =
                peer
                  { npConnections = Map.insert peerId conn' (npConnections peer),
                    npPending = Map.delete peerId (npPending peer)
                  }
           in ([PeerConnected peerId Outbound], peer')

-- | Handle disconnect packet (pure).
handleDisconnect :: PeerId -> NetPeer -> ([PeerEvent], NetPeer)
handleDisconnect peerId peer =
  if Map.member peerId (npConnections peer)
    then
      ( [PeerDisconnected peerId ReasonRequested],
        peer {npConnections = Map.delete peerId (npConnections peer)}
      )
    else ([], removePending peerId peer)

-- | Payload header: channel (3 bits) + is_fragment (1 bit) + reserved (4 bits)
-- Encoded in first byte: [is_fragment:1][reserved:4][channel:3]
-- Channel is low 3 bits, is_fragment is high bit (0x80)

-- | Fragment flag bit in payload header.
payloadFragmentFlag :: Word8
payloadFragmentFlag = 0x80

-- | Channel mask in payload header.
payloadChannelMask :: Word8
payloadChannelMask = 0x07

-- | Decode payload header byte.
decodePayloadHeader :: Word8 -> (Word8, Bool)
decodePayloadHeader b =
  let channel = b .&. payloadChannelMask
      isFragment = (b .&. payloadFragmentFlag) /= 0
   in (channel, isFragment)
{-# INLINE decodePayloadHeader #-}

-- | Handle payload packet (pure).
handlePayload :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handlePayload peerId pkt now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> ([], peer)
    Just conn ->
      let -- Process header for ACKs and reliability
          conn' = processIncomingHeader (pktHeader pkt) now conn
          conn'' = Conn.touchRecvTime now conn'
          payload = pktPayload pkt
          peer' = peer {npConnections = Map.insert peerId conn'' (npConnections peer)}
       in if BS.length payload < 1
            then ([], peer')
            else
              let headerByte = BS.head payload
                  (channel, isFragment) = decodePayloadHeader headerByte
                  msgData = BS.tail payload
               in if isFragment
                    then handleFragment peerId channel msgData now peer'
                    else ([PeerMessage peerId channel msgData], peer')

-- | Handle a fragment, reassembling if complete (pure).
handleFragment :: PeerId -> Word8 -> BS.ByteString -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleFragment peerId channel fragData now peer =
  let assemblers = npFragmentAssemblers peer
      assembler =
        Map.findWithDefault
          (newFragmentAssembler fragmentTimeoutMs fragmentMaxBufferSize)
          peerId
          assemblers
      (maybeComplete, assembler') = processFragment fragData now assembler
      peer' = peer {npFragmentAssemblers = Map.insert peerId assembler' assemblers}
   in case maybeComplete of
        Nothing -> ([], peer')
        Just completeData -> ([PeerMessage peerId channel completeData], peer')

-- | Try to migrate an existing connection to a new address, or ignore the packet (pure).
tryMigrationOrIgnore :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
tryMigrationOrIgnore newPeerId pkt now peer
  | not (ncEnableConnectionMigration (npConfig peer)) = ([], peer)
  | otherwise =
      case findMigrationCandidate pkt now peer of
        Nothing -> ([], peer) -- No matching connection
        Just (oldPeerId, conn, migrationToken) ->
          -- Check migration cooldown
          case Map.lookup migrationToken (npMigrationCooldowns peer) of
            Just lastMigration
              | elapsedMs lastMigration now < migrationCooldownMs ->
                  ([], peer) -- Still in cooldown
            _ ->
              -- Perform migration
              let peer' =
                    peer
                      { npConnections =
                          Map.insert newPeerId conn $
                            Map.delete oldPeerId (npConnections peer),
                        npMigrationCooldowns =
                          Map.insert migrationToken now (npMigrationCooldowns peer),
                        -- Also migrate fragment assembler if exists
                        npFragmentAssemblers =
                          case Map.lookup oldPeerId (npFragmentAssemblers peer) of
                            Nothing -> npFragmentAssemblers peer
                            Just asm ->
                              Map.insert newPeerId asm $
                                Map.delete oldPeerId (npFragmentAssemblers peer)
                      }
                  event = PeerMigrated oldPeerId newPeerId
                  -- Now process the payload with the new peer ID
                  (payloadEvents, peer'') = handlePayload newPeerId pkt now peer'
               in (event : payloadEvents, peer'')

-- | Find a connection that could match this packet (for migration).
-- Returns (oldPeerId, connection, migrationToken) if found.
findMigrationCandidate :: Packet -> MonoTime -> NetPeer -> Maybe (PeerId, Connection, Word64)
findMigrationCandidate pkt _now peer =
  let header = pktHeader pkt
      incomingSeq = sequenceNum header
      maxDistance = ncMaxSequenceDistance (npConfig peer)
   in -- Find a connection whose sequence is within range
      foldr
        ( \(pid, conn) acc ->
            case acc of
              Just _ -> acc -- Already found one
              Nothing ->
                let remoteSeq = Conn.connRemoteSeq conn
                    diff = abs (fromIntegral incomingSeq - fromIntegral remoteSeq :: Int)
                 in if diff <= fromIntegral maxDistance
                      then Just (pid, conn, migrationTokenFor conn)
                      else Nothing
        )
        Nothing
        (Map.toList (npConnections peer))

-- | Generate a migration token for a connection.
-- This should be stable across the connection lifetime.
migrationTokenFor :: Connection -> Word64
migrationTokenFor = Conn.connClientSalt

-- | Handle keepalive packet (pure).
handleKeepalive :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleKeepalive peerId pkt now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> ([], peer)
    Just conn ->
      let -- Process header for ACKs
          conn' = processIncomingHeader (pktHeader pkt) now conn
          conn'' = Conn.touchRecvTime now conn'
          peer' = peer {npConnections = Map.insert peerId conn'' (npConnections peer)}
       in ([], peer')

-- | Update all connections and collect messages/disconnects.
updateConnections :: MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
updateConnections now peer =
  let (events, conns') = Map.foldrWithKey updateOne ([], Map.empty) (npConnections peer)
   in (events, peer {npConnections = conns'})
  where
    updateOne peerId conn (evts, conns) =
      case Conn.updateTick now conn of
        Left _err ->
          -- Connection timed out
          (PeerDisconnected peerId ReasonTimeout : evts, conns)
        Right conn' ->
          -- Collect messages from all channels
          let (msgs, conn'') = collectMessages peerId conn' []
           in (msgs ++ evts, Map.insert peerId conn'' conns)

    collectMessages peerId conn acc =
      let numChannels = Conn.channelCount conn
       in collectFromChannels peerId 0 numChannels conn acc

    collectFromChannels peerId ch maxCh conn acc
      | ch >= maxCh = (acc, conn)
      | otherwise =
          let (maybeMsg, conn') = Conn.receiveMessage ch conn
           in case maybeMsg of
                Nothing -> collectFromChannels peerId (ch + 1) maxCh conn' acc
                Just msg ->
                  let evt = PeerMessage peerId ch msg
                   in collectFromChannels peerId ch maxCh conn' (evt : acc)

-- | Retry pending outbound connections (pure).
retryPendingConnectionsPure :: MonoTime -> NetPeer -> NetPeer
retryPendingConnectionsPure now peer =
  let outbound = Map.toList $ Map.filter (\p -> pcDirection p == Outbound) (npPending peer)
   in foldl (retryOne now) peer outbound
  where
    retryOne t p (peerId, pending) =
      let elapsed = elapsedMs (pcLastRetry pending) t
          retryInterval = ncConnectionRequestTimeoutMs (npConfig p) / fromIntegral (ncConnectionRequestMaxRetries (npConfig p) + 1)
       in if elapsed > retryInterval && pcRetryCount pending < ncConnectionRequestMaxRetries (npConfig p)
            then
              let pending' = pending {pcRetryCount = pcRetryCount pending + 1, pcLastRetry = t}
                  p' = p {npPending = Map.insert peerId pending' (npPending p)}
               in queueControlPacket ConnectionRequest BS.empty peerId p'
            else p

-- | Cleanup expired pending connections.
cleanupPending :: MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
cleanupPending now peer =
  let timeout = ncConnectionRequestTimeoutMs (npConfig peer)
      (expired, kept) = Map.partition (\p -> elapsedMs (pcCreatedAt p) now > timeout) (npPending peer)
      events = map (\(pid, _) -> PeerDisconnected pid ReasonTimeout) (Map.toList expired)
   in (events, peer {npPending = kept})

-- | Remove a peer from pending.
removePending :: PeerId -> NetPeer -> NetPeer
removePending peerId peer = peer {npPending = Map.delete peerId (npPending peer)}

-- | Encode a Word64 salt to bytes.
encodeSalt :: Word64 -> BS.ByteString
encodeSalt salt = toBytes $ bitSerialize salt empty
{-# INLINE encodeSalt #-}

-- | Decode a Word64 salt from bytes.
decodeSalt :: BS.ByteString -> Maybe Word64
decodeSalt bs
  | BS.length bs < 8 = Nothing
  | otherwise =
      case bitDeserialize (fromBytes bs) of
        Left _ -> Nothing
        Right result -> Just (readValue result)
{-# INLINE decodeSalt #-}

-- | Deny reason codes.
denyReasonServerFull :: Word8
denyReasonServerFull = 1

denyReasonInvalidChallenge :: Word8
denyReasonInvalidChallenge = 2

-- | Encode a deny reason to bytes.
encodeDenyReason :: Word8 -> BS.ByteString
encodeDenyReason = BS.singleton

-- | Decode a deny reason from bytes.
decodeDenyReason :: BS.ByteString -> Word8
decodeDenyReason bs
  | BS.null bs = 0
  | otherwise = BS.head bs

-- | Convert SockAddr to a key for rate limiting.
sockAddrToKey :: SockAddr -> Word64
sockAddrToKey addr =
  -- Simple hash of the address string
  let str = show addr
   in fromIntegral $ sum (map fromEnum str)

-- | Send a message to a connected peer.
peerSend ::
  PeerId ->
  Word8 ->
  BS.ByteString ->
  MonoTime ->
  NetPeer ->
  Either ConnectionError NetPeer
peerSend peerId channel dat now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> Left ErrNotConnected
    Just conn ->
      case Conn.sendMessage channel dat now conn of
        Left err -> Left err
        Right conn' ->
          Right peer {npConnections = Map.insert peerId conn' (npConnections peer)}

-- | Broadcast a message to all connected peers.
-- This queues the message and drains connection queues so packets are ready to send.
peerBroadcast ::
  Word8 ->
  BS.ByteString ->
  Maybe PeerId ->
  MonoTime ->
  NetPeer ->
  NetPeer
peerBroadcast channel dat except now peer =
  let peerIds = filter (\p -> Just p /= except) $ Map.keys (npConnections peer)
      -- Queue message to each connection's channel
      peer' = foldr (\pid p -> fromRight p (peerSend pid channel dat now p)) peer peerIds
   in -- Drain connection queues to npSendQueue so packets are ready
      drainAllConnectionQueues now peer'

-- | Get number of connected peers.
peerCount :: NetPeer -> Int
peerCount = Map.size . npConnections

-- | Check if a peer is connected.
peerIsConnected :: PeerId -> NetPeer -> Bool
peerIsConnected peerId peer = Map.member peerId (npConnections peer)

-- | Get stats for a connected peer.
peerStats :: PeerId -> NetPeer -> Maybe NetworkStats
peerStats peerId peer =
  Conn.connectionStats <$> Map.lookup peerId (npConnections peer)

-- | Get the local address.
peerLocalAddr :: NetPeer -> IO (Either SocketError SockAddr)
peerLocalAddr peer = socketLocalAddr (npSocket peer)

-- | Get list of all connected peer IDs.
peerConnectedIds :: NetPeer -> [PeerId]
peerConnectedIds = Map.keys . npConnections
