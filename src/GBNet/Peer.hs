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

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word64, Word8)
import GBNet.Class (MonadNetwork (..), MonadTime (..), MonoTime (..))
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection
  ( Connection,
    ConnectionError (..),
    DisconnectReason (..),
    OutgoingPacket (..),
    newConnection,
    processIncomingHeader,
    receiveIncomingPayload,
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
import GBNet.Types (ChannelId (..), SequenceNum (..))
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
    npSendQueue :: !(Seq RawPacket),
    -- | Number of packets dropped by rate limiting
    npRateLimitDrops :: !Word64
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
       in go s' (n - 1) (fromIntegral (r `mod` 256) : acc)

-- | SplitMix-style random number generator.
-- Uses a different output function from the state update to avoid leaking state.
nextRandom :: Word64 -> (Word64, Word64)
nextRandom s =
  let -- LCG state update
      a = 6364136223846793005
      c = 1442695040888963407
      next = a * s + c
      -- SplitMix-style output mixing (state is not exposed)
      z0 = next `xor` (next `shiftR` 30)
      z1 = z0 * 0xBF58476D1CE4E5B9
      z2 = z1 `xor` (z1 `shiftR` 27)
      z3 = z2 * 0x94D049BB133111EB
      output = z3 `xor` (z3 `shiftR` 31)
   in (output, next)

-- -----------------------------------------------------------------------------
-- Helper functions for pure API
-- -----------------------------------------------------------------------------

-- | Update a connection by PeerId, returning unchanged peer if not found.
withConnection :: PeerId -> (Connection -> Connection) -> NetPeer -> NetPeer
withConnection pid f peer = peer {npConnections = Map.adjust f pid (npConnections peer)}
{-# INLINE withConnection #-}

-- | Decode channel sequence from payload bytes.
-- Returns (channelSeq, remaining data) or Nothing if too short.
decodeChannelSeq :: BS.ByteString -> Maybe (SequenceNum, BS.ByteString)
decodeChannelSeq bs
  | BS.length bs < 2 = Nothing
  | otherwise =
      let chSeq =
            SequenceNum $
              (fromIntegral (BS.index bs 0) `shiftL` 8)
                .|. fromIntegral (BS.index bs 1)
       in Just (chSeq, BS.drop 2 bs)
{-# INLINE decodeChannelSeq #-}

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
-- Transitions the connection to Disconnecting state for graceful shutdown
-- with retries, rather than removing it immediately.
peerDisconnect :: PeerId -> MonoTime -> NetPeer -> NetPeer
peerDisconnect peerId now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> peer
    Just _ -> withConnection peerId (Conn.disconnect ReasonRequested now) peer

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
peerProcess now packets peer0 =
  let (events, peer1) = runState (peerProcessS now packets) peer0
      (outgoing, peer2) = drainPeerSendQueue peer1
   in PeerResult peer2 events outgoing

-- | State-based peer processing.
peerProcessS :: MonoTime -> [IncomingPacket] -> State NetPeer [PeerEvent]
peerProcessS now packets = do
  -- Process incoming packets
  let internalPackets = map (\ip -> (ipFrom ip, ipData ip)) packets
  events1 <- processPacketsPureS internalPackets now

  -- Update all connections and collect messages
  events2 <- updateConnectionsS now

  -- Drain connection send queues into peer send queue
  modify' (drainAllConnectionQueues now)

  -- Retry pending outbound connections
  modify' (retryPendingConnectionsPure now)

  -- Cleanup expired pending connections
  events3 <- cleanupPendingS now

  pure (events1 ++ events2 ++ events3)

-- | Drain send queues from all connections into the peer's send queue.
drainAllConnectionQueues :: MonoTime -> NetPeer -> NetPeer
drainAllConnectionQueues _now peer =
  let peerIds = Map.keys (npConnections peer)
   in foldl' drainConnectionQueue peer peerIds
  where
    drainConnectionQueue p peerId =
      case Map.lookup peerId (npConnections p) of
        Nothing -> p
        Just conn ->
          let (connPackets, conn') = Conn.drainSendQueue conn
              p' = p {npConnections = Map.insert peerId conn' (npConnections p)}
              -- Convert OutgoingPackets to RawPackets
              rawPackets = map (outgoingToRaw peerId) connPackets
           in foldl' (flip queueRawPacket) p' rawPackets

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
  [(ChannelId, BS.ByteString)] ->
  NetPeer ->
  m ([PeerEvent], NetPeer)
peerTick messages peer = do
  now <- getMonoTime
  -- 1. Receive all available packets
  packets <- peerRecvAllM
  -- 2. Queue messages to all connections
  let peer1 = foldl' (queueMessage now) peer messages
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

-- | Process received packets (State version).
processPacketsPureS ::
  [(PeerId, BS.ByteString)] ->
  MonoTime ->
  State NetPeer [PeerEvent]
processPacketsPureS packets now = do
  eventss <- mapM (\(pid, dat) -> handlePacketS pid dat now) packets
  pure (concat eventss)

-- | Handle a single received packet (State version).
handlePacketS ::
  PeerId ->
  BS.ByteString ->
  MonoTime ->
  State NetPeer [PeerEvent]
handlePacketS peerId dat now =
  case parsePacket dat of
    Nothing -> pure []
    Just pkt -> do
      let ptype = packetType (pktHeader pkt)
      handlePacketByTypeS peerId pkt now ptype

-- | Parse packet from raw data.
parsePacket :: BS.ByteString -> Maybe Packet
parsePacket dat =
  case deserializePacket dat of
    Left _ -> Nothing
    Right pkt -> Just pkt

-- | Handle a packet by its type (State version).
handlePacketByTypeS ::
  PeerId ->
  Packet ->
  MonoTime ->
  PacketType ->
  State NetPeer [PeerEvent]
handlePacketByTypeS peerId pkt now ptype = case ptype of
  -- Incoming connection request
  ConnectionRequest -> handleConnectionRequestS peerId now
  -- Challenge from server (we're connecting outbound)
  ConnectionChallenge -> handleConnectionChallengeS peerId pkt now
  -- Response to our challenge (they're connecting inbound)
  ConnectionResponse -> handleConnectionResponseS peerId pkt now
  -- Connection accepted
  ConnectionAccepted -> handleConnectionAcceptedS peerId now
  -- Connection denied
  ConnectionDenied -> do
    let reason = decodeDenyReason (pktPayload pkt)
    modify' (removePending peerId)
    pure [PeerDisconnected peerId (ReasonUnknown reason)]
  -- Disconnect
  Disconnect -> handleDisconnectS peerId
  -- Payload from connected peer (or potential migration)
  Payload -> do
    conns <- gets npConnections
    if Map.member peerId conns
      then handlePayloadS peerId pkt now
      else handleMigrationS peerId pkt now
  -- Keepalive
  Keepalive -> do
    modify' $ withConnection peerId $ \conn ->
      Conn.touchRecvTime now $ processIncomingHeader (pktHeader pkt) now conn
    pure []

-- | Handle incoming connection request (State version).
handleConnectionRequestS :: PeerId -> MonoTime -> State NetPeer [PeerEvent]
handleConnectionRequestS peerId now = do
  conns <- gets npConnections
  pending <- gets npPending
  if Map.member peerId conns
    then do
      -- Already connected, resend accept
      modify' $ queueControlPacket ConnectionAccepted BS.empty peerId
      pure []
    else case Map.lookup peerId pending of
      Just p -> do
        -- Already pending, resend challenge with stored salt
        let saltPayload = encodeSalt (pcServerSalt p)
        modify' $ queueControlPacket ConnectionChallenge saltPayload peerId
        pure []
      Nothing -> do
        -- Check rate limit
        rl <- gets npRateLimiter
        let addrKey = sockAddrToKey (unPeerId peerId)
            (allowed, rl') = rateLimiterAllow addrKey now rl
        modify' $ \peer -> peer {npRateLimiter = rl'}
        if not allowed
          then do
            modify' $ \peer -> peer {npRateLimitDrops = npRateLimitDrops peer + 1}
            pure []
          else do
            peer <- gets id
            let pendingSize = Map.size (npPending peer)
                connSize = Map.size (npConnections peer)
                maxClients = ncMaxClients (npConfig peer)
            if pendingSize >= maxClients
              then do
                modify' $ \p -> p {npRateLimitDrops = npRateLimitDrops p + 1}
                pure []
              else
                if connSize >= maxClients
                  then do
                    let reason = encodeDenyReason denyReasonServerFull
                    modify' $ queueControlPacket ConnectionDenied reason peerId
                    pure []
                  else do
                    -- Create pending connection and send challenge with salt
                    rng <- gets npRngState
                    let (salt, rng') = nextRandom rng
                        newPending =
                          PendingConnection
                            { pcDirection = Inbound,
                              pcServerSalt = salt,
                              pcClientSalt = 0,
                              pcCreatedAt = now,
                              pcRetryCount = 0,
                              pcLastRetry = now
                            }
                    modify' $ \p ->
                      p
                        { npPending = Map.insert peerId newPending (npPending p),
                          npRngState = rng'
                        }
                    let saltPayload = encodeSalt salt
                    modify' $ queueControlPacket ConnectionChallenge saltPayload peerId
                    pure []

-- | Handle connection challenge (we're outbound, received their challenge) (State version).
handleConnectionChallengeS :: PeerId -> Packet -> MonoTime -> State NetPeer [PeerEvent]
handleConnectionChallengeS peerId pkt _now = do
  pending <- gets npPending
  case Map.lookup peerId pending of
    Nothing -> pure [] -- Not expecting this
    Just p
      | pcDirection p /= Outbound -> pure []
      | otherwise ->
          case decodeSalt (pktPayload pkt) of
            Nothing -> pure [] -- Invalid challenge
            Just serverSalt -> do
              let p' = p {pcServerSalt = serverSalt}
              modify' $ \peer -> peer {npPending = Map.insert peerId p' (npPending peer)}
              let saltPayload = encodeSalt (pcClientSalt p')
              modify' $ queueControlPacket ConnectionResponse saltPayload peerId
              pure []

-- | Handle connection response (we're inbound, received their response) (State version).
handleConnectionResponseS :: PeerId -> Packet -> MonoTime -> State NetPeer [PeerEvent]
handleConnectionResponseS peerId pkt now = do
  pending <- gets npPending
  case Map.lookup peerId pending of
    Nothing -> pure []
    Just p
      | pcDirection p /= Inbound -> pure []
      | otherwise ->
          case decodeSalt (pktPayload pkt) of
            Nothing -> pure [] -- Invalid response
            Just clientSalt
              | clientSalt == 0 || clientSalt == pcServerSalt p -> do
                  let reason = encodeDenyReason denyReasonInvalidChallenge
                  modify' $ queueControlPacket ConnectionDenied reason peerId
                  modify' $ removePending peerId
                  pure []
              | otherwise -> do
                  -- Accept the connection
                  config <- gets npConfig
                  let conn = Conn.markConnected now $ Conn.touchRecvTime now $ newConnection config clientSalt now
                  modify' $ \peer ->
                    peer
                      { npConnections = Map.insert peerId conn (npConnections peer),
                        npPending = Map.delete peerId (npPending peer)
                      }
                  modify' $ queueControlPacket ConnectionAccepted BS.empty peerId
                  pure [PeerConnected peerId Inbound]

-- | Handle connection accepted (we're outbound, they accepted) (State version).
handleConnectionAcceptedS :: PeerId -> MonoTime -> State NetPeer [PeerEvent]
handleConnectionAcceptedS peerId now = do
  pending <- gets npPending
  case Map.lookup peerId pending of
    Nothing -> pure [] -- Might be duplicate accept, ignore
    Just p
      | pcDirection p /= Outbound -> pure []
      | otherwise -> do
          -- Promote to connected
          config <- gets npConfig
          let conn = Conn.markConnected now $ Conn.touchRecvTime now $ newConnection config (pcClientSalt p) now
          modify' $ \peer ->
            peer
              { npConnections = Map.insert peerId conn (npConnections peer),
                npPending = Map.delete peerId (npPending peer)
              }
          pure [PeerConnected peerId Outbound]

-- | Handle disconnect packet (State version).
handleDisconnectS :: PeerId -> State NetPeer [PeerEvent]
handleDisconnectS peerId = do
  conns <- gets npConnections
  if Map.member peerId conns
    then do
      modify' $ \peer ->
        cleanupPeer peerId peer {npConnections = Map.delete peerId (npConnections peer)}
      pure [PeerDisconnected peerId ReasonRequested]
    else do
      modify' (removePending peerId)
      pure []

-- | Clean up per-peer state (fragment assemblers, migration cooldowns).
cleanupPeer :: PeerId -> NetPeer -> NetPeer
cleanupPeer peerId peer =
  peer
    { npFragmentAssemblers = Map.delete peerId (npFragmentAssemblers peer)
    }

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
decodePayloadHeader :: Word8 -> (ChannelId, Bool)
decodePayloadHeader b =
  let channel = ChannelId (b .&. payloadChannelMask)
      isFragment = (b .&. payloadFragmentFlag) /= 0
   in (channel, isFragment)
{-# INLINE decodePayloadHeader #-}

-- | Minimum size of a non-fragment payload: headerByte + 2 bytes channel sequence.
minPayloadSize :: Int
minPayloadSize = 3

-- | Handle payload packet (State version).
-- Routes messages through the channel system for proper ordering/dedup.
handlePayloadS :: PeerId -> Packet -> MonoTime -> State NetPeer [PeerEvent]
handlePayloadS peerId pkt now = do
  conns <- gets npConnections
  case Map.lookup peerId conns of
    Nothing -> pure []
    Just conn -> do
      let conn' = Conn.touchRecvTime now $ processIncomingHeader (pktHeader pkt) now conn
          payload = pktPayload pkt
      if BS.length payload < 1
        then do
          modify' $ \peer -> peer {npConnections = Map.insert peerId conn' (npConnections peer)}
          pure []
        else do
          let headerByte = BS.head payload
              (channel, isFragment) = decodePayloadHeader headerByte
              rest = BS.tail payload
          if isFragment
            then do
              modify' $ \peer -> peer {npConnections = Map.insert peerId conn' (npConnections peer)}
              handleFragmentS peerId channel rest now
            else
              if BS.length payload < minPayloadSize
                then do
                  modify' $ \peer -> peer {npConnections = Map.insert peerId conn' (npConnections peer)}
                  pure []
                else case decodeChannelSeq rest of
                  Nothing -> do
                    modify' $ \peer -> peer {npConnections = Map.insert peerId conn' (npConnections peer)}
                    pure []
                  Just (chSeq, msgData) -> do
                    let conn'' = receiveIncomingPayload channel chSeq msgData now conn'
                    modify' $ \peer -> peer {npConnections = Map.insert peerId conn'' (npConnections peer)}
                    pure []

-- | Handle a fragment, reassembling if complete (State version).
-- After reassembly, routes through the channel system for ordering/dedup.
handleFragmentS :: PeerId -> ChannelId -> BS.ByteString -> MonoTime -> State NetPeer [PeerEvent]
handleFragmentS peerId channel fragData now = do
  assemblers <- gets npFragmentAssemblers
  let assembler =
        Map.findWithDefault
          (newFragmentAssembler fragmentTimeoutMs fragmentMaxBufferSize)
          peerId
          assemblers
      (maybeComplete, assembler') = processFragment fragData now assembler
  modify' $ \peer -> peer {npFragmentAssemblers = Map.insert peerId assembler' assemblers}
  case maybeComplete of
    Nothing -> pure []
    Just completeData ->
      case decodeChannelSeq completeData of
        Nothing -> pure [] -- Malformed: missing channel sequence
        Just (chSeq, msgData) -> do
          modify' $ withConnection peerId (receiveIncomingPayload channel chSeq msgData now)
          pure []

-- | Try to migrate an existing connection to a new address, or ignore the packet (State version).
handleMigrationS :: PeerId -> Packet -> MonoTime -> State NetPeer [PeerEvent]
handleMigrationS newPeerId pkt now = do
  config <- gets npConfig
  if not (ncEnableConnectionMigration config)
    then pure []
    else do
      peer <- gets id
      case findMigrationCandidate pkt now peer of
        Nothing -> pure [] -- No matching connection
        Just (oldPeerId, conn, migrationToken) ->
          case Map.lookup migrationToken (npMigrationCooldowns peer) of
            Just lastMigration
              | elapsedMs lastMigration now < migrationCooldownMs ->
                  pure [] -- Still in cooldown
            _ -> do
              -- Perform migration
              modify' $ \p ->
                p
                  { npConnections =
                      Map.insert newPeerId conn $
                        Map.delete oldPeerId (npConnections p),
                    npMigrationCooldowns =
                      Map.insert migrationToken now (npMigrationCooldowns p),
                    -- Also migrate fragment assembler if exists
                    npFragmentAssemblers =
                      case Map.lookup oldPeerId (npFragmentAssemblers p) of
                        Nothing -> npFragmentAssemblers p
                        Just asm ->
                          Map.insert newPeerId asm $
                            Map.delete oldPeerId (npFragmentAssemblers p)
                  }
              let event = PeerMigrated oldPeerId newPeerId
              -- Now process the payload with the new peer ID
              payloadEvents <- handlePayloadS newPeerId pkt now
              pure (event : payloadEvents)

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

-- | Update all connections and collect messages/disconnects (State version).
updateConnectionsS :: MonoTime -> State NetPeer [PeerEvent]
updateConnectionsS now = do
  conns <- gets npConnections
  let (events, conns', disconnectedIds) = Map.foldrWithKey updateOne ([], Map.empty, []) conns
  modify' $ \peer ->
    foldl' (flip cleanupPeer) (peer {npConnections = conns'}) disconnectedIds
  pure events
  where
    updateOne peerId conn (evts, connsAcc, discs) =
      case Conn.updateTick now conn of
        Left _err ->
          -- Connection timed out
          (PeerDisconnected peerId ReasonTimeout : evts, connsAcc, peerId : discs)
        Right conn'
          | Conn.connectionState conn' == Conn.Disconnected ->
              -- Graceful disconnect complete
              (PeerDisconnected peerId ReasonRequested : evts, connsAcc, peerId : discs)
          | otherwise ->
              -- Collect messages from all channels
              let (msgs, conn'') = collectMessages peerId conn' []
               in (msgs ++ evts, Map.insert peerId conn'' connsAcc, discs)

    collectMessages peerId conn acc =
      let numChannels = Conn.channelCount conn
       in collectFromChannels peerId 0 numChannels conn acc

    collectFromChannels peerId ch maxCh conn acc
      | ch >= maxCh = (acc, conn)
      | otherwise =
          let chId = ChannelId ch
              (msgs, conn') = Conn.receiveMessage chId conn
              evts = map (PeerMessage peerId chId) msgs
           in collectFromChannels peerId (ch + 1) maxCh conn' (evts ++ acc)

-- | Retry pending outbound connections (pure).
retryPendingConnectionsPure :: MonoTime -> NetPeer -> NetPeer
retryPendingConnectionsPure now peer =
  let outbound = Map.toList $ Map.filter (\p -> pcDirection p == Outbound) (npPending peer)
   in foldl' (retryOne now) peer outbound
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

-- | Cleanup expired pending connections (State version).
cleanupPendingS :: MonoTime -> State NetPeer [PeerEvent]
cleanupPendingS now = do
  config <- gets npConfig
  pending <- gets npPending
  let timeout = ncConnectionRequestTimeoutMs config
      (expired, kept) = Map.partition (\p -> elapsedMs (pcCreatedAt p) now > timeout) pending
      events = map (\(pid, _) -> PeerDisconnected pid ReasonTimeout) (Map.toList expired)
  modify' $ \peer -> peer {npPending = kept}
  pure events

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

-- | FNV-1a hash seed.
fnvOffsetBasis :: Word64
fnvOffsetBasis = 14695981039346656037

-- | FNV-1a hash prime.
fnvPrime :: Word64
fnvPrime = 1099511628211

-- | Convert SockAddr to a key for rate limiting using FNV-1a hash.
sockAddrToKey :: SockAddr -> Word64
sockAddrToKey addr =
  let str = show addr
   in foldl' (\h c -> (h `xor` fromIntegral (fromEnum c)) * fnvPrime) fnvOffsetBasis str

-- | Send a message to a connected peer.
peerSend ::
  PeerId ->
  ChannelId ->
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
  ChannelId ->
  BS.ByteString ->
  Maybe PeerId ->
  MonoTime ->
  NetPeer ->
  NetPeer
peerBroadcast channel dat except now peer =
  let peerIds = filter (\p -> Just p /= except) $ Map.keys (npConnections peer)
      -- Queue message to each connection's channel
      peer' = foldl' (\p pid -> fromRight p (peerSend pid channel dat now p)) peer peerIds
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
