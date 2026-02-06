{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Module      : GBNet.Peer.Handshake
-- Description : Connection handshake state machine
--
-- Handles connection request, challenge, response, accepted, and disconnect
-- packets during the handshake protocol.
module GBNet.Peer.Handshake
  ( handleConnectionRequest,
    handleNewConnectionRequest,
    handleConnectionChallenge,
    handleConnectionResponse,
    handleConnectionAccepted,
    handleDisconnect,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import GBNet.Class (MonoTime)
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection
  ( DisconnectReason (..),
    newConnection,
  )
import qualified GBNet.Connection as Conn
import GBNet.Packet (Packet (..), PacketType (..))
import GBNet.Peer.Internal
import GBNet.Peer.Protocol
  ( DenyReason (..),
    decodeSalt,
    encodeDenyReason,
    encodeSalt,
    sockAddrToKey,
  )
import GBNet.Security (rateLimiterAllow)
import GBNet.Util (nextRandom)
import Optics ((%~), (&), (.~))

-- | Handle incoming connection request (pure).
handleConnectionRequest :: PeerId -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionRequest peerId now peer =
  case (Map.member peerId (npConnections peer), Map.lookup peerId (npPending peer)) of
    (True, _) ->
      -- Already connected, resend accept
      ([], queueControlPacket ConnectionAccepted BS.empty peerId peer)
    (_, Just p) ->
      -- Already pending, resend challenge with stored salt
      let saltPayload = encodeSalt (pcServerSalt p)
       in ([], queueControlPacket ConnectionChallenge saltPayload peerId peer)
    (False, Nothing) ->
      handleNewConnectionRequest peerId now peer

-- | Handle a genuinely new connection request after checking existing state (pure).
handleNewConnectionRequest :: PeerId -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleNewConnectionRequest peerId now peer =
  let addrKey = sockAddrToKey (unPeerId peerId)
      (allowed, rl') = rateLimiterAllow addrKey now (npRateLimiter peer)
      peer1 = peer & #npRateLimiter .~ rl'
      pendingSize = Map.size (npPending peer1)
      connSize = Map.size (npConnections peer1)
      maxClients = ncMaxClients (npConfig peer1)
   in if
        | not allowed ->
            ([], peer1 & #npRateLimitDrops %~ (+ 1))
        | pendingSize >= maxClients ->
            ([], peer1 & #npRateLimitDrops %~ (+ 1))
        | connSize >= maxClients ->
            let reason = encodeDenyReason DenyServerFull
             in ([], queueControlPacket ConnectionDenied reason peerId peer1)
        | otherwise ->
            let (salt, rng') = nextRandom (npRngState peer1)
                newPend =
                  PendingConnection
                    { pcDirection = Inbound,
                      pcServerSalt = salt,
                      pcClientSalt = 0,
                      pcCreatedAt = now,
                      pcRetryCount = 0,
                      pcLastRetry = now
                    }
                peer2 =
                  peer1
                    & #npPending
                    %~ Map.insert peerId newPend
                    & #npRngState
                    .~ rng'
                saltPayload = encodeSalt salt
             in ([], queueControlPacket ConnectionChallenge saltPayload peerId peer2)

-- | Handle connection challenge (we're outbound, received their challenge) (pure).
handleConnectionChallenge :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionChallenge peerId pkt _now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> ([], peer)
    Just p
      | pcDirection p /= Outbound -> ([], peer)
      | otherwise ->
          case decodeSalt (pktPayload pkt) of
            Nothing -> ([], peer)
            Just serverSalt ->
              let p' = p & #pcServerSalt .~ serverSalt
                  peer' = peer & #npPending %~ Map.insert peerId p'
                  saltPayload = encodeSalt (pcClientSalt p')
               in ([], queueControlPacket ConnectionResponse saltPayload peerId peer')

-- | Handle connection response (we're inbound, received their response) (pure).
handleConnectionResponse :: PeerId -> Packet -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionResponse peerId pkt now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> ([], peer)
    Just p
      | pcDirection p /= Inbound -> ([], peer)
      | otherwise ->
          case decodeSalt (pktPayload pkt) of
            Nothing -> ([], peer)
            Just clientSalt
              | clientSalt == 0 || clientSalt == pcServerSalt p ->
                  let reason = encodeDenyReason DenyInvalidChallenge
                      peer' =
                        queueControlPacket ConnectionDenied reason peerId $
                          removePending peerId peer
                   in ([], peer')
              | otherwise ->
                  let conn =
                        Conn.markConnected now $
                          Conn.touchRecvTime now $
                            newConnection (npConfig peer) clientSalt now
                      peer' =
                        peer
                          & #npConnections
                          %~ Map.insert peerId conn
                          & #npPending
                          %~ Map.delete peerId
                   in ([PeerConnected peerId Inbound], queueControlPacket ConnectionAccepted BS.empty peerId peer')

-- | Handle connection accepted (we're outbound, they accepted) (pure).
handleConnectionAccepted :: PeerId -> MonoTime -> NetPeer -> ([PeerEvent], NetPeer)
handleConnectionAccepted peerId now peer =
  case Map.lookup peerId (npPending peer) of
    Nothing -> ([], peer)
    Just p
      | pcDirection p /= Outbound -> ([], peer)
      | otherwise ->
          let conn =
                Conn.markConnected now $
                  Conn.touchRecvTime now $
                    newConnection (npConfig peer) (pcClientSalt p) now
              peer' =
                peer
                  & #npConnections
                  %~ Map.insert peerId conn
                  & #npPending
                  %~ Map.delete peerId
           in ([PeerConnected peerId Outbound], peer')

-- | Handle disconnect packet (pure).
handleDisconnect :: PeerId -> NetPeer -> ([PeerEvent], NetPeer)
handleDisconnect peerId peer =
  if Map.member peerId (npConnections peer)
    then
      let peer' = cleanupPeer peerId (peer & #npConnections %~ Map.delete peerId)
       in ([PeerDisconnected peerId ReasonRequested], peer')
    else ([], removePending peerId peer)
