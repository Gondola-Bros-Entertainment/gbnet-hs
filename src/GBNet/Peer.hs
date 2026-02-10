{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

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

    -- * Pending connection (opaque)
    PendingConnection,

    -- * Queries
    peerCount,
    peerIsConnected,
    peerStats,
    peerLocalAddr,
    peerConnectedIds,
  )
where

import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import GBNet.Class (MonadNetwork (..), MonadTime (..), MonoTime)
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection
  ( ConnectionError (..),
    DisconnectReason (..),
    OutgoingPacket (..),
    connEncryptionKey,
    connRecvNonceMax,
    connSendNonce,
    processIncomingHeader,
    receiveIncomingPayload,
  )
import qualified GBNet.Connection as Conn
import GBNet.Crypto (NonceCounter (..), decrypt, encrypt)
import qualified GBNet.Crypto
import GBNet.Fragment (newFragmentAssembler, processFragment)
import GBNet.Packet
  ( Packet (..),
    PacketHeader (..),
    PacketType (..),
    deserializePacket,
    packetHeaderByteSize,
    serializePacket,
  )
import GBNet.Peer.Handshake
  ( handleConnectionAccepted,
    handleConnectionChallenge,
    handleConnectionRequest,
    handleConnectionResponse,
    handleDisconnect,
  )
import GBNet.Peer.Internal
import GBNet.Peer.Migration
  ( findMigrationCandidate,
    migrationCooldownMs,
  )
import GBNet.Peer.Protocol
  ( decodeDenyReason,
    decodePayloadHeader,
    denyToDisconnectReason,
    minPayloadSize,
  )
import qualified GBNet.Peer.Protocol as Proto
import GBNet.Reliability (elapsedMs)
import GBNet.Security (appendCrc32)
import GBNet.Socket
  ( SocketError (..),
    UdpSocket,
    newUdpSocket,
    socketLocalAddr,
  )
import GBNet.Stats (NetworkStats)
import GBNet.Types (ChannelId (..))
import GBNet.Util (nextRandom)
import Network.Socket (SockAddr)
import Optics ((%), (%~), (&), (.~), (?~))

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
              & #npPending
              %~ Map.insert peerId pending
              & #npRngState
              .~ rng'
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
--
-- See 'peerTick' for a convenience wrapper that combines receive, process, and send.
peerProcess :: MonoTime -> [IncomingPacket] -> NetPeer -> PeerResult
peerProcess now packets peer0 =
  let -- Process incoming packets
      internalPackets = map (\ip -> (ipFrom ip, ipData ip)) packets
      (events1, peer1) = processPacketsPure internalPackets now peer0
      -- Update all connections and collect messages
      (events2, peer2) = updateConnections now peer1
      -- Drain connection send queues into peer send queue
      peer3 = drainAllConnectionQueues now peer2
      -- Retry pending outbound connections
      peer4 = retryPendingConnectionsPure now peer3
      -- Cleanup expired pending connections
      (events3, peer5) = cleanupPending now peer4
      -- Drain the peer's send queue
      (outgoing, peer6) = drainPeerSendQueue peer5
   in PeerResult peer6 (events1 ++ events2 ++ events3) outgoing

-- | Drain send queues from all connections into the peer's send queue.
-- Single-pass over the connection map via foldlWithKey'.
-- Post-handshake packets (Payload, Keepalive, Disconnect) are encrypted
-- when the connection has an encryption key configured.
drainAllConnectionQueues :: MonoTime -> NetPeer -> NetPeer
drainAllConnectionQueues _now peer =
  Map.foldlWithKey' drainOne (peer & #npConnections .~ Map.empty) (npConnections peer)
  where
    protocolId = ncProtocolId (npConfig peer)

    drainOne p peerId conn =
      let (connPackets, conn') = Conn.drainSendQueue conn
          (rawPackets, conn'') = encryptOutgoing peerId protocolId conn' connPackets
          p' = p & #npConnections %~ Map.insert peerId conn''
       in foldl' (flip queueRawPacket) p' rawPackets

-- | Encrypt outgoing packets and update connection nonce state.
-- Handshake packets remain plaintext; post-handshake packets are encrypted
-- when the connection has a key.
encryptOutgoing ::
  PeerId ->
  Word32 ->
  Conn.Connection ->
  [OutgoingPacket] ->
  ([RawPacket], Conn.Connection)
encryptOutgoing peerId protocolId conn0 =
  foldl' encryptOne ([], conn0)
  where
    encryptOne (!acc, !conn) (OutgoingPacket hdr ptype payload) =
      let header = hdr {packetType = ptype}
          pkt = Packet {pktHeader = header, pktPayload = payload}
          serialized = serializePacket pkt
       in case (connEncryptionKey conn, isPostHandshake ptype) of
            (Just key, True) ->
              let nonce = connSendNonce conn
                  headerBytes = BS.take packetHeaderByteSize serialized
                  payloadBytes = BS.drop packetHeaderByteSize serialized
               in case encrypt key nonce protocolId payloadBytes of
                    Left _ ->
                      -- Encryption failed; send plaintext as fallback
                      let raw = appendCrc32 serialized
                       in (acc ++ [RawPacket peerId raw], conn)
                    Right encrypted ->
                      let raw = appendCrc32 (headerBytes <> encrypted)
                          conn' =
                            conn
                              & #connSendNonce
                              .~ NonceCounter (unNonceCounter nonce + 1)
                       in (acc ++ [RawPacket peerId raw], conn')
            _ ->
              -- Plaintext: no key or handshake packet
              let raw = appendCrc32 serialized
               in (acc ++ [RawPacket peerId raw], conn)

-- | Whether a packet type is post-handshake (should be encrypted).
isPostHandshake :: PacketType -> Bool
isPostHandshake Payload = True
isPostHandshake Keepalive = True
isPostHandshake Disconnect = True
isPostHandshake _ = False

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
-- Disconnects all connections through the state machine and closes the network.
peerShutdownM :: (MonadNetwork m) => NetPeer -> m ()
peerShutdownM peer = do
  now <- getMonoTime
  let peerIds = Map.keys (npConnections peer)
      -- Disconnect each connection through the proper state machine
      peer' = foldr (\pid p -> withConnection pid (Conn.disconnect ReasonRequested now) p) peer peerIds
      (outgoing, _) = drainPeerSendQueue (drainAllConnectionQueues now peer')
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

-- -----------------------------------------------------------------------------
-- Packet handling (pure)
-- -----------------------------------------------------------------------------

-- | Process received packets (pure).
processPacketsPure ::
  [(PeerId, BS.ByteString)] ->
  MonoTime ->
  NetPeer ->
  ([PeerEvent], NetPeer)
processPacketsPure packets now peer = foldl' go ([], peer) packets
  where
    go (evts, p) (pid, dat) =
      let (evts', p') = handlePacket pid dat now p
       in (evts ++ evts', p')

-- | Handle a single received packet (pure).
-- For post-handshake packets from connections with encryption keys,
-- the payload is decrypted before dispatch. Anti-replay is enforced
-- by checking the nonce counter.
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
       in case (isPostHandshake ptype, lookupConnectionKey peerId peer) of
            (True, Just (key, conn)) ->
              let protocolId = ncProtocolId (npConfig peer)
                  encPayload = pktPayload pkt
               in case decrypt key protocolId encPayload of
                    Left _ ->
                      -- Decryption failed: increment stat, drop packet
                      let conn' =
                            conn
                              & #connStats
                              % #nsDecryptionFailures
                              %~ (+ 1)
                          peer' = peer & #npConnections %~ Map.insert peerId conn'
                       in ([], peer')
                    Right (plaintext, NonceCounter recvNonce) ->
                      -- Anti-replay check
                      case connRecvNonceMax conn of
                        Just maxNonce
                          | recvNonce <= maxNonce ->
                              ([], peer) -- Nonce replay, drop
                        _ ->
                          let conn' =
                                conn
                                  & #connRecvNonceMax
                                  ?~ recvNonce
                              peer' = peer & #npConnections %~ Map.insert peerId conn'
                              decryptedPkt = pkt {pktPayload = plaintext}
                           in handlePacketByType peerId decryptedPkt now ptype peer'
            _ ->
              -- No encryption or handshake packet: process as plaintext
              handlePacketByType peerId pkt now ptype peer

-- | Look up a connection's encryption key and connection for a peer.
lookupConnectionKey ::
  PeerId ->
  NetPeer ->
  Maybe (GBNet.Crypto.EncryptionKey, Conn.Connection)
lookupConnectionKey peerId peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> Nothing
    Just conn -> case connEncryptionKey conn of
      Nothing -> Nothing
      Just key -> Just (key, conn)

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
  PacketType ->
  NetPeer ->
  ([PeerEvent], NetPeer)
handlePacketByType peerId pkt now ptype peer = case ptype of
  ConnectionRequest -> handleConnectionRequest peerId now peer
  ConnectionChallenge -> handleConnectionChallenge peerId pkt now peer
  ConnectionResponse -> handleConnectionResponse peerId pkt now peer
  ConnectionAccepted -> handleConnectionAccepted peerId now peer
  ConnectionDenied ->
    let reason = decodeDenyReason (pktPayload pkt)
        peer' = removePending peerId peer
     in ([PeerDisconnected peerId (denyToDisconnectReason reason)], peer')
  Disconnect -> handleDisconnect peerId peer
  Payload ->
    if Map.member peerId (npConnections peer)
      then handlePayload peerId pkt now peer
      else handleMigration peerId pkt now peer
  Keepalive ->
    let peer' =
          withConnection
            peerId
            (Conn.touchRecvTime now . processIncomingHeader (pktHeader pkt) now)
            peer
     in ([], peer')

-- | Handle payload packet (pure).
-- Routes messages through the channel system for proper ordering/dedup.
handlePayload :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handlePayload peerId pkt now peer =
  case Map.lookup peerId (npConnections peer) of
    Nothing -> ([], peer)
    Just conn ->
      let conn' = Conn.touchRecvTime now $ processIncomingHeader (pktHeader pkt) now conn
          payload = pktPayload pkt
       in case BS.uncons payload of
            Nothing ->
              ([], peer & #npConnections %~ Map.insert peerId conn')
            Just (headerByte, rest) ->
              let (channel, isFragment) = decodePayloadHeader headerByte
               in if isFragment
                    then
                      let peer' = peer & #npConnections %~ Map.insert peerId conn'
                       in handleFragment peerId channel rest now peer'
                    else
                      let finalConn
                            | BS.length payload < minPayloadSize = conn'
                            | otherwise = case Proto.decodeChannelSeq rest of
                                Nothing -> conn'
                                Just (chSeq, msgData) ->
                                  receiveIncomingPayload channel chSeq msgData now conn'
                       in ([], peer & #npConnections %~ Map.insert peerId finalConn)

-- | Handle a fragment, reassembling if complete (pure).
-- After reassembly, routes through the channel system for ordering/dedup.
handleFragment :: PeerId -> ChannelId -> BS.ByteString -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleFragment peerId channel fragData now peer =
  let assemblers = npFragmentAssemblers peer
      assembler =
        Map.findWithDefault
          (newFragmentAssembler fragmentTimeoutMs fragmentMaxBufferSize)
          peerId
          assemblers
      (maybeComplete, assembler') = processFragment fragData now assembler
      peer' = peer & #npFragmentAssemblers .~ Map.insert peerId assembler' assemblers
   in case maybeComplete of
        Nothing -> ([], peer')
        Just completeData ->
          case Proto.decodeChannelSeq completeData of
            Nothing -> ([], peer')
            Just (chSeq, msgData) ->
              ([], withConnection peerId (receiveIncomingPayload channel chSeq msgData now) peer')

-- | Try to migrate an existing connection to a new address, or ignore the packet (pure).
handleMigration :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleMigration newPeerId pkt now peer =
  if not (ncEnableConnectionMigration (npConfig peer))
    then ([], peer)
    else case findMigrationCandidate pkt now peer of
      Nothing -> ([], peer)
      Just (oldPeerId, conn, migrationToken) ->
        case Map.lookup migrationToken (npMigrationCooldowns peer) of
          Just lastMigration
            | elapsedMs lastMigration now < migrationCooldownMs ->
                ([], peer) -- Still in cooldown
          _ ->
            let peer' =
                  peer
                    & #npConnections
                    %~ (Map.insert newPeerId conn . Map.delete oldPeerId)
                    & #npMigrationCooldowns
                    %~ Map.insert migrationToken now
                    & #npFragmentAssemblers
                    %~ ( \fa -> case Map.lookup oldPeerId fa of
                           Nothing -> fa
                           Just asm -> Map.insert newPeerId asm $ Map.delete oldPeerId fa
                       )
                event = PeerMigrated oldPeerId newPeerId
                (payloadEvents, peer'') = handlePayload newPeerId pkt now peer'
             in (event : payloadEvents, peer'')

-- | Update all connections and collect messages/disconnects (pure).
-- Uses reverse accumulator to avoid O(n^2) list appending.
updateConnections :: MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
updateConnections now peer =
  let conns = npConnections peer
      (revEvents, conns', disconnectedIds) = Map.foldlWithKey' updateOne ([], Map.empty, []) conns
      peer' = foldl' (flip cleanupPeer) (peer & #npConnections .~ conns') disconnectedIds
   in (reverse revEvents, peer')
  where
    updateOne (revEvts, connsAcc, discs) peerId conn =
      case Conn.updateTick now conn of
        Left _err ->
          -- Connection timed out
          (PeerDisconnected peerId ReasonTimeout : revEvts, connsAcc, peerId : discs)
        Right conn'
          | Conn.connectionState conn' == Conn.Disconnected ->
              -- Graceful disconnect complete
              (PeerDisconnected peerId ReasonRequested : revEvts, connsAcc, peerId : discs)
          | otherwise ->
              -- Collect messages from all channels
              let (msgs, conn'') = collectMessages peerId conn'
               in (prependReversed msgs revEvts, Map.insert peerId conn'' connsAcc, discs)

    collectMessages peerId conn =
      let numChannels = Conn.channelCount conn
       in collectFromChannels peerId 0 numChannels conn []

    collectFromChannels peerId ch maxCh conn revAcc
      | ch >= maxCh = (reverse revAcc, conn)
      | otherwise =
          let chId = ChannelId ch
              (msgs, conn') = Conn.receiveMessage chId conn
              evts = map (PeerMessage peerId chId) msgs
           in collectFromChannels peerId (ch + 1) maxCh conn' (prependReversed evts revAcc)

-- | Prepend a list in reverse onto an accumulator. O(length xs).
-- Used for efficient reverse-accumulator pattern.
prependReversed :: [a] -> [a] -> [a]
prependReversed xs ys = foldl (flip (:)) ys xs

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
              let pending' = pending & #pcRetryCount %~ (+ 1) & #pcLastRetry .~ t
                  p' = p & #npPending %~ Map.insert peerId pending'
               in queueControlPacket ConnectionRequest BS.empty peerId p'
            else p

-- | Cleanup expired pending connections (pure).
cleanupPending :: MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
cleanupPending now peer =
  let timeout = ncConnectionRequestTimeoutMs (npConfig peer)
      (expired, kept) = Map.partition (\p -> elapsedMs (pcCreatedAt p) now > timeout) (npPending peer)
      events = map (\(pid, _) -> PeerDisconnected pid ReasonTimeout) (Map.toList expired)
   in (events, peer & #npPending .~ kept)

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
          Right (peer & #npConnections %~ Map.insert peerId conn')

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
      -- Best-effort: per-peer send failures (channel full, disconnected) are
      -- intentionally ignored so one failing peer doesn't block the broadcast.
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
peerLocalAddr :: NetPeer -> SockAddr
peerLocalAddr = npLocalAddr

-- | Get list of all connected peer IDs.
peerConnectedIds :: NetPeer -> [PeerId]
peerConnectedIds = Map.keys . npConnections
