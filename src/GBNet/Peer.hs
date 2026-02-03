-- |
-- Module      : GBNet.Peer
-- Description : Unified peer networking API
--
-- NetPeer provides a symmetric abstraction for game networking.
-- A peer can accept incoming connections (server-like), initiate
-- outgoing connections (client-like), or do both (P2P/mesh).
module GBNet.Peer
  ( -- * Peer identifier
    PeerId (..),
    peerIdFromAddr,

    -- * Connection direction
    ConnectionDirection (..),

    -- * Events
    PeerEvent (..),

    -- * Net peer
    NetPeer,
    newPeer,

    -- * Connection management
    peerConnect,
    peerDisconnect,
    peerShutdown,

    -- * Main loop
    peerUpdate,

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

import Control.Monad (foldM, foldM_)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64, Word8)
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
import GBNet.Serialize.BitBuffer (ReadResult (..), empty, fromBytes, toBytes)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import GBNet.Reliability (MonoTime, elapsedMs)
import GBNet.Security (RateLimiter, appendCrc32, newRateLimiter, rateLimiterAllow, rateLimiterCleanup, validateAndStripCrc32)
import GBNet.Socket
  ( SocketError (..),
    UdpSocket,
    closeSocket,
    newUdpSocket,
    socketLocalAddr,
    socketRecvFrom,
    socketSendTo,
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

-- | Events emitted by 'peerUpdate'.
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
    npConnections :: !(Map PeerId Connection),
    npPending :: !(Map PeerId PendingConnection),
    npConfig :: !NetworkConfig,
    npRateLimiter :: !RateLimiter,
    npCookieSecret :: !BS.ByteString,
    npRngState :: !Word64,
    npFragmentAssemblers :: !(Map PeerId FragmentAssembler),
    -- | Tracks last migration time per connection to rate-limit migrations
    npMigrationCooldowns :: !(Map Word64 MonoTime)
  }

-- | Create a new peer bound to the given address.
newPeer ::
  SockAddr ->
  NetworkConfig ->
  MonoTime ->
  IO (Either SocketError NetPeer)
newPeer addr config now = do
  socketResult <- newUdpSocket addr
  case socketResult of
    Left err -> return $ Left err
    Right sock -> do
      -- Generate cookie secret from time-based RNG
      let rng0 = now
          (secret, rng1) = generateCookieSecret rng0
      return $
        Right
          NetPeer
            { npSocket = sock,
              npConnections = Map.empty,
              npPending = Map.empty,
              npConfig = config,
              npRateLimiter = newRateLimiter (ncRateLimitPerSecond config),
              npCookieSecret = secret,
              npRngState = rng1,
              npFragmentAssemblers = Map.empty,
              npMigrationCooldowns = Map.empty
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

-- | Initiate an outgoing connection to a peer.
peerConnect :: PeerId -> MonoTime -> NetPeer -> IO NetPeer
peerConnect peerId now peer
  | Map.member peerId (npConnections peer) = return peer -- Already connected
  | Map.member peerId (npPending peer) = return peer -- Already pending
  | otherwise = do
      -- Generate client salt
      let (salt, rng') = nextRandom (npRngState peer)
      let pending =
            PendingConnection
              { pcDirection = Outbound,
                pcServerSalt = 0, -- Will be filled when we receive challenge
                pcClientSalt = salt,
                pcCreatedAt = now,
                pcRetryCount = 0,
                pcLastRetry = now
              }
      let peer' =
            peer
              { npPending = Map.insert peerId pending (npPending peer),
                npRngState = rng'
              }
      -- Send connection request
      sendRawPacket ConnectionRequest peerId now peer'

-- | Disconnect a specific peer.
peerDisconnect :: PeerId -> MonoTime -> NetPeer -> IO NetPeer
peerDisconnect peerId now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> return peer
    Just _ -> do
      peer' <- sendRawPacket Disconnect peerId now peer
      return $ peer' {npConnections = Map.delete peerId (npConnections peer')}

-- | Shutdown the peer, disconnecting all connections.
peerShutdown :: MonoTime -> NetPeer -> IO ()
peerShutdown now peer = do
  -- Send disconnect to all connected peers
  let peerIds = Map.keys (npConnections peer)
  foldM_ (\p pid -> sendRawPacket Disconnect pid now p) peer peerIds
  closeSocket (npSocket peer)

-- | Process incoming packets and return events.
-- Call this once per game tick.
peerUpdate :: MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
peerUpdate now peer = do
  -- Receive all pending packets
  (packets, peer1) <- receiveAll now peer []

  -- Process each packet
  (events1, peer2) <- processPackets packets now peer1 []

  -- Update all connections and collect messages
  let (events2, peer3) = updateConnections now peer2

  -- Send queued packets from connections
  peer4 <- sendQueuedPackets now peer3

  -- Retry pending outbound connections
  peer5 <- retryPendingConnections now peer4

  -- Cleanup expired pending connections
  let (events3, peer6) = cleanupPending now peer5

  -- Cleanup rate limiter
  let peer7 = peer6 {npRateLimiter = rateLimiterCleanup now (npRateLimiter peer6)}

  return (events1 ++ events2 ++ events3, peer7)

-- | Receive all available packets from the socket.
receiveAll ::
  MonoTime ->
  NetPeer ->
  [(PeerId, BS.ByteString)] ->
  IO ([(PeerId, BS.ByteString)], NetPeer)
receiveAll now peer acc = do
  result <- socketRecvFrom now (npSocket peer)
  case result of
    Left SocketWouldBlock -> return (reverse acc, peer)
    Left _ -> return (reverse acc, peer)
    Right (dat, addr, sock') -> do
      let peer' = peer {npSocket = sock'}
      case validateAndStripCrc32 dat of
        Nothing -> receiveAll now peer' acc
        Just validated ->
          let peerId = PeerId addr
           in receiveAll now peer' ((peerId, validated) : acc)

-- | Process received packets.
processPackets ::
  [(PeerId, BS.ByteString)] ->
  MonoTime ->
  NetPeer ->
  [PeerEvent] ->
  IO ([PeerEvent], NetPeer)
processPackets [] _ peer events = return (reverse events, peer)
processPackets ((peerId, dat) : rest) now peer events = do
  (newEvents, peer') <- handlePacket peerId dat now peer
  processPackets rest now peer' (reverse newEvents ++ events)

-- | Handle a single received packet.
handlePacket ::
  PeerId ->
  BS.ByteString ->
  MonoTime ->
  NetPeer ->
  IO ([PeerEvent], NetPeer)
handlePacket peerId dat now peer =
  case parsePacket dat of
    Nothing -> return ([], peer)
    Just pkt ->
      let ptype = packetType (pktHeader pkt)
       in handlePacketByType peerId pkt now peer ptype

-- | Parse packet from raw data.
parsePacket :: BS.ByteString -> Maybe Packet
parsePacket dat =
  case deserializePacket dat of
    Left _ -> Nothing
    Right pkt -> Just pkt

-- | Handle a packet by its type.
handlePacketByType ::
  PeerId ->
  Packet ->
  MonoTime ->
  NetPeer ->
  PacketType ->
  IO ([PeerEvent], NetPeer)
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
     in return ([PeerDisconnected peerId (ReasonUnknown reason)], removePending peerId peer)
  -- Disconnect
  Disconnect -> handleDisconnect peerId peer
  -- Payload from connected peer (or potential migration)
  Payload ->
    if Map.member peerId (npConnections peer)
      then handlePayload peerId pkt now peer
      else tryMigrationOrIgnore peerId pkt now peer
  -- Keepalive
  Keepalive -> handleKeepalive peerId pkt now peer

-- | Handle incoming connection request.
handleConnectionRequest :: PeerId -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleConnectionRequest peerId now peer
  | Map.member peerId (npConnections peer) = do
      -- Already connected, resend accept
      peer' <- sendRawPacket ConnectionAccepted peerId now peer
      return ([], peer')
  | Map.member peerId (npPending peer) = do
      -- Already pending, resend challenge with stored salt
      case Map.lookup peerId (npPending peer) of
        Just pending -> do
          let saltPayload = encodeSalt (pcServerSalt pending)
          peer' <- sendRawPacketWithPayload ConnectionChallenge saltPayload peerId now peer
          return ([], peer')
        Nothing -> return ([], peer)
  | otherwise = do
      -- Check rate limit
      let addrKey = sockAddrToKey (unPeerId peerId)
      let (allowed, rl') = rateLimiterAllow addrKey now (npRateLimiter peer)
      let peer' = peer {npRateLimiter = rl'}
      if not allowed
        then return ([], peer')
        else do
          -- Check max connections
          if Map.size (npConnections peer') >= ncMaxClients (npConfig peer')
            then do
              let reason = encodeDenyReason denyReasonServerFull
              peer'' <- sendRawPacketWithPayload ConnectionDenied reason peerId now peer'
              return ([], peer'')
            else do
              -- Create pending connection and send challenge with salt
              let (salt, rng') = nextRandom (npRngState peer')
              let pending =
                    PendingConnection
                      { pcDirection = Inbound,
                        pcServerSalt = salt,
                        pcClientSalt = 0,
                        pcCreatedAt = now,
                        pcRetryCount = 0,
                        pcLastRetry = now
                      }
              let peer'' =
                    peer'
                      { npPending = Map.insert peerId pending (npPending peer'),
                        npRngState = rng'
                      }
              let saltPayload = encodeSalt salt
              peer''' <- sendRawPacketWithPayload ConnectionChallenge saltPayload peerId now peer''
              return ([], peer''')

-- | Handle connection challenge (we're outbound, received their challenge).
handleConnectionChallenge :: PeerId -> Packet -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleConnectionChallenge peerId pkt now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> return ([], peer) -- Not expecting this
    Just pending
      | pcDirection pending /= Outbound -> return ([], peer)
      | otherwise ->
          -- Extract server_salt from payload
          case decodeSalt (pktPayload pkt) of
            Nothing -> return ([], peer) -- Invalid challenge
            Just serverSalt -> do
              -- Store server salt and send our response with client salt
              let pending' = pending {pcServerSalt = serverSalt}
              let peer' = peer {npPending = Map.insert peerId pending' (npPending peer)}
              let saltPayload = encodeSalt (pcClientSalt pending')
              peer'' <- sendRawPacketWithPayload ConnectionResponse saltPayload peerId now peer'
              return ([], peer'')

-- | Handle connection response (we're inbound, received their response).
handleConnectionResponse :: PeerId -> Packet -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleConnectionResponse peerId pkt now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> return ([], peer)
    Just pending
      | pcDirection pending /= Inbound -> return ([], peer)
      | otherwise ->
          -- Extract and validate client_salt from payload
          case decodeSalt (pktPayload pkt) of
            Nothing -> return ([], peer) -- Invalid response
            Just clientSalt
              -- Validate: client salt must not be zero or same as server salt
              | clientSalt == 0 || clientSalt == pcServerSalt pending -> do
                  let reason = encodeDenyReason denyReasonInvalidChallenge
                  peer' <- sendRawPacketWithPayload ConnectionDenied reason peerId now peer
                  return ([], removePending peerId peer')
              | otherwise -> do
                  -- Accept the connection
                  let conn = newConnection (npConfig peer) clientSalt now
                  let conn' = Conn.touchRecvTime now conn
                  let peer' =
                        peer
                          { npConnections = Map.insert peerId conn' (npConnections peer),
                            npPending = Map.delete peerId (npPending peer)
                          }
                  peer'' <- sendRawPacket ConnectionAccepted peerId now peer'
                  return ([PeerConnected peerId Inbound], peer'')

-- | Handle connection accepted (we're outbound, they accepted).
handleConnectionAccepted :: PeerId -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleConnectionAccepted peerId now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing ->
      -- Might be duplicate accept, ignore
      return ([], peer)
    Just pending
      | pcDirection pending /= Outbound -> return ([], peer)
      | otherwise -> do
          -- Promote to connected
          let conn = newConnection (npConfig peer) (pcClientSalt pending) now
          let conn' = Conn.touchRecvTime now conn
          let peer' =
                peer
                  { npConnections = Map.insert peerId conn' (npConnections peer),
                    npPending = Map.delete peerId (npPending peer)
                  }
          return ([PeerConnected peerId Outbound], peer')

-- | Handle disconnect packet.
handleDisconnect :: PeerId -> NetPeer -> IO ([PeerEvent], NetPeer)
handleDisconnect peerId peer =
  if Map.member peerId (npConnections peer)
    then
      return
        ( [PeerDisconnected peerId ReasonRequested],
          peer {npConnections = Map.delete peerId (npConnections peer)}
        )
    else return ([], removePending peerId peer)

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

-- | Handle payload packet.
handlePayload :: PeerId -> Packet -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handlePayload peerId pkt now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> return ([], peer)
    Just conn ->
      let -- Process header for ACKs and reliability
          conn' = processIncomingHeader (pktHeader pkt) now conn
          conn'' = Conn.touchRecvTime now conn'
          payload = pktPayload pkt
          peer' = peer {npConnections = Map.insert peerId conn'' (npConnections peer)}
       in if BS.length payload < 1
            then return ([], peer')
            else
              let headerByte = BS.head payload
                  (channel, isFragment) = decodePayloadHeader headerByte
                  msgData = BS.tail payload
               in if isFragment
                    then handleFragment peerId channel msgData now peer'
                    else return ([PeerMessage peerId channel msgData], peer')

-- | Handle a fragment, reassembling if complete.
handleFragment :: PeerId -> Word8 -> BS.ByteString -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleFragment peerId channel fragData now peer =
  let assemblers = npFragmentAssemblers peer
      assembler = Map.findWithDefault
                    (newFragmentAssembler fragmentTimeoutMs fragmentMaxBufferSize)
                    peerId
                    assemblers
      (maybeComplete, assembler') = processFragment fragData now assembler
      peer' = peer {npFragmentAssemblers = Map.insert peerId assembler' assemblers}
   in case maybeComplete of
        Nothing -> return ([], peer')
        Just completeData -> return ([PeerMessage peerId channel completeData], peer')

-- | Try to migrate an existing connection to a new address, or ignore the packet.
tryMigrationOrIgnore :: PeerId -> Packet -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
tryMigrationOrIgnore newPeerId pkt now peer
  | not (ncEnableConnectionMigration (npConfig peer)) = return ([], peer)
  | otherwise =
      case findMigrationCandidate pkt now peer of
        Nothing -> return ([], peer) -- No matching connection
        Just (oldPeerId, conn, migrationToken) ->
          -- Check migration cooldown
          case Map.lookup migrationToken (npMigrationCooldowns peer) of
            Just lastMigration
              | elapsedMs lastMigration now < migrationCooldownMs ->
                  return ([], peer) -- Still in cooldown
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
               in -- Now process the payload with the new peer ID
                  do
                    (payloadEvents, peer'') <- handlePayload newPeerId pkt now peer'
                    return (event : payloadEvents, peer'')

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

-- | Handle keepalive packet.
handleKeepalive :: PeerId -> Packet -> MonoTime -> NetPeer -> IO ([PeerEvent], NetPeer)
handleKeepalive peerId pkt now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> return ([], peer)
    Just conn ->
      let -- Process header for ACKs
          conn' = processIncomingHeader (pktHeader pkt) now conn
          conn'' = Conn.touchRecvTime now conn'
          peer' = peer {npConnections = Map.insert peerId conn'' (npConnections peer)}
       in return ([], peer')

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

-- | Send queued packets from all connections.
sendQueuedPackets :: MonoTime -> NetPeer -> IO NetPeer
sendQueuedPackets now peer = do
  let peerIds = Map.keys (npConnections peer)
  foldM (sendForPeer now) peer peerIds
  where
    sendForPeer t p peerId =
      case Map.lookup peerId (npConnections p) of
        Nothing -> return p
        Just conn ->
          let (packets, conn') = Conn.drainSendQueue conn
              p' = p {npConnections = Map.insert peerId conn' (npConnections p)}
           in sendPacketList packets peerId t p'

    sendPacketList [] _ _ p = return p
    sendPacketList (OutgoingPacket hdr ptype payload : rest) peerId t p = do
      -- Construct header with the packet type
      let header = hdr {packetType = ptype}
      let pkt = Packet {pktHeader = header, pktPayload = payload}
      let packetData = serializePacket pkt
      let dataWithCrc = appendCrc32 packetData
      result <- socketSendTo dataWithCrc (unPeerId peerId) t (npSocket p)
      case result of
        Left _ -> sendPacketList rest peerId t p
        Right (_, sock') -> sendPacketList rest peerId t (p {npSocket = sock'})

-- | Retry pending outbound connections.
retryPendingConnections :: MonoTime -> NetPeer -> IO NetPeer
retryPendingConnections now peer = do
  let outbound = Map.toList $ Map.filter (\p -> pcDirection p == Outbound) (npPending peer)
  foldM (retryOne now) peer outbound
  where
    retryOne t p (peerId, pending) =
      let elapsed = elapsedMs (pcLastRetry pending) t
          retryInterval = ncConnectionRequestTimeoutMs (npConfig p) / fromIntegral (ncConnectionRequestMaxRetries (npConfig p) + 1)
       in if elapsed > retryInterval && pcRetryCount pending < ncConnectionRequestMaxRetries (npConfig p)
            then do
              let pending' = pending {pcRetryCount = pcRetryCount pending + 1, pcLastRetry = t}
              let p' = p {npPending = Map.insert peerId pending' (npPending p)}
              sendRawPacket ConnectionRequest peerId t p'
            else return p

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

-- | Send a raw control packet with optional payload.
sendRawPacketWithPayload :: PacketType -> BS.ByteString -> PeerId -> MonoTime -> NetPeer -> IO NetPeer
sendRawPacketWithPayload ptype payload peerId now peer = do
  let header =
        PacketHeader
          { packetType = ptype,
            sequenceNum = 0,
            ack = 0,
            ackBitfield = 0
          }
  let pkt = Packet {pktHeader = header, pktPayload = payload}
  let packetData = serializePacket pkt
  let dataWithCrc = appendCrc32 packetData
  result <- socketSendTo dataWithCrc (unPeerId peerId) now (npSocket peer)
  case result of
    Left _ -> return peer
    Right (_, sock') -> return peer {npSocket = sock'}

-- | Send a raw control packet (no payload).
sendRawPacket :: PacketType -> PeerId -> MonoTime -> NetPeer -> IO NetPeer
sendRawPacket ptype = sendRawPacketWithPayload ptype BS.empty

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
peerBroadcast ::
  Word8 ->
  BS.ByteString ->
  Maybe PeerId ->
  MonoTime ->
  NetPeer ->
  NetPeer
peerBroadcast channel dat except now peer =
  let peerIds = filter (\p -> Just p /= except) $ Map.keys (npConnections peer)
   in foldr (\pid p -> fromRight p (peerSend pid channel dat now p)) peer peerIds

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
