-- |
-- Module      : GBNet.Connection
-- Description : Connection state machine for reliable UDP
--
-- Manages handshake, channels, reliability tracking, and congestion control.
module GBNet.Connection
  ( -- * Connection state
    ConnectionState (..),
    DisconnectReason (..),
    disconnectReasonCode,
    parseDisconnectReason,

    -- * Errors
    ConnectionError (..),

    -- * Outgoing packet
    OutgoingPacket (..),

    -- * Connection
    Connection (..),
    newConnection,

    -- * State queries
    connectionState,
    isConnected,
    connectionStats,

    -- * Operations
    connect,
    disconnect,
    sendMessage,
    receiveMessage,

    -- * Tick update
    updateTick,

    -- * Packet handling
    drainSendQueue,
    createHeader,
    processIncomingHeader,

    -- * Time tracking
    touchRecvTime,
    touchSendTime,
    recordBytesSent,
    recordBytesReceived,

    -- * State transitions
    markConnected,

    -- * Reset
    resetConnection,

    -- * Channel info
    channelCount,
  )
where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Channel (Channel, ChannelError (..), ChannelMessage (..))
import qualified GBNet.Channel as Channel
import GBNet.Config (NetworkConfig (..))
import GBNet.Congestion
  ( BandwidthTracker,
    CongestionController,
    CongestionWindow,
    btBytesPerSecond,
    btRecord,
    ccCanSend,
    ccDeductBudget,
    ccRefillBudget,
    ccUpdate,
    cwCanSend,
    cwCanSendPaced,
    cwOnAck,
    cwOnSend,
    cwUpdatePacing,
    newBandwidthTracker,
    newCongestionController,
    newCongestionWindow,
  )
import GBNet.Packet (PacketHeader (..), PacketType (..))
import GBNet.Reliability (MonoTime, ReliableEndpoint, elapsedMs)
import qualified GBNet.Reliability as Rel
import GBNet.Stats
  ( NetworkStats (..),
    assessConnectionQuality,
    defaultNetworkStats,
  )
import GBNet.Util (sequenceGreaterThan)

-- | States of the connection state machine.
data ConnectionState
  = Disconnected
  | Connecting
  | ChallengeResponse
  | Connected
  | Disconnecting
  deriving (Eq, Show)

-- | Typed disconnect reason decoded from wire code.
data DisconnectReason
  = ReasonTimeout
  | ReasonRequested
  | ReasonKicked
  | ReasonServerFull
  | ReasonProtocolMismatch
  | ReasonUnknown Word8
  deriving (Eq, Show)

-- | Disconnect reason wire codes.
disconnectReasonCode :: DisconnectReason -> Word8
disconnectReasonCode ReasonTimeout = 0
disconnectReasonCode ReasonRequested = 1
disconnectReasonCode ReasonKicked = 2
disconnectReasonCode ReasonServerFull = 3
disconnectReasonCode ReasonProtocolMismatch = 4
disconnectReasonCode (ReasonUnknown code) = code

-- | Parse disconnect reason from wire code.
parseDisconnectReason :: Word8 -> DisconnectReason
parseDisconnectReason 0 = ReasonTimeout
parseDisconnectReason 1 = ReasonRequested
parseDisconnectReason 2 = ReasonKicked
parseDisconnectReason 3 = ReasonServerFull
parseDisconnectReason 4 = ReasonProtocolMismatch
parseDisconnectReason code = ReasonUnknown code

-- | Errors that can occur during connection operations.
data ConnectionError
  = ErrNotConnected
  | ErrAlreadyConnected
  | ErrConnectionDenied Word8
  | ErrTimeout
  | ErrProtocolMismatch
  | ErrInvalidPacket
  | ErrInvalidChannel Word8
  | ErrChannelError ChannelError
  | ErrMessageTooLarge
  deriving (Eq, Show)

-- | Packet waiting to be sent.
data OutgoingPacket = OutgoingPacket
  { opHeader :: !PacketHeader,
    opType :: !PacketType,
    opPayload :: !BS.ByteString
  }
  deriving (Show)

-- | A single peer connection managing handshake, channels, reliability, and congestion.
data Connection = Connection
  { connConfig :: !NetworkConfig,
    connState :: !ConnectionState,
    -- Salts for connection validation
    connClientSalt :: !Word64,
    connServerSalt :: !Word64,
    -- Timing
    connLastSendTime :: !MonoTime,
    connLastRecvTime :: !MonoTime,
    connStartTime :: !(Maybe MonoTime),
    connRequestTime :: !(Maybe MonoTime),
    connRetryCount :: !Int,
    -- Sequences
    connLocalSeq :: !Word16,
    connRemoteSeq :: !Word16,
    -- Subsystems
    connReliability :: !ReliableEndpoint,
    connChannels :: !(Vector Channel),
    connChannelPriority :: ![Int],
    connCongestion :: !CongestionController,
    connCwnd :: !(Maybe CongestionWindow),
    connBandwidthUp :: !BandwidthTracker,
    connBandwidthDown :: !BandwidthTracker,
    -- Queues
    connSendQueue :: !(Seq OutgoingPacket),
    -- Stats
    connStats :: !NetworkStats,
    -- Disconnect tracking
    connDisconnectTime :: !(Maybe MonoTime),
    connDisconnectRetries :: !Int,
    -- Flags
    connPendingAck :: !Bool,
    connDataSentThisTick :: !Bool
  }
  deriving (Show)

-- | Create a new connection.
newConnection :: NetworkConfig -> Word64 -> MonoTime -> Connection
newConnection config clientSalt now =
  let numChannels = ncMaxChannels config
      defaultCfg = ncDefaultChannelConfig config
      -- Pad custom configs with defaults, then take exactly numChannels
      configs = take numChannels $ ncChannelConfigs config ++ repeat defaultCfg
      channels = V.fromList $ zipWith Channel.newChannel [0 ..] configs
      -- Sort channel indices by priority (highest first)
      priorityOrder =
        sortBy (comparing (Down . Channel.ccPriority . (configs !!))) [0 .. numChannels - 1]
      congestion =
        newCongestionController
          (ncSendRate config)
          (ncCongestionBadLossThreshold config)
          (ncCongestionGoodRttThreshold config)
          (ncCongestionRecoveryTimeMs config)
      cwnd =
        if ncUseCwndCongestion config
          then Just (newCongestionWindow (ncMtu config))
          else Nothing
      bandwidthWindowMs = 1000.0
   in Connection
        { connConfig = config,
          connState = Disconnected,
          connClientSalt = clientSalt,
          connServerSalt = 0,
          connLastSendTime = now,
          connLastRecvTime = now,
          connStartTime = Nothing,
          connRequestTime = Nothing,
          connRetryCount = 0,
          connLocalSeq = 0,
          connRemoteSeq = 0,
          connReliability = Rel.newReliableEndpoint (ncPacketBufferSize config),
          connChannels = channels,
          connChannelPriority = priorityOrder,
          connCongestion = congestion,
          connCwnd = cwnd,
          connBandwidthUp = newBandwidthTracker bandwidthWindowMs,
          connBandwidthDown = newBandwidthTracker bandwidthWindowMs,
          connSendQueue = Seq.empty,
          connStats = defaultNetworkStats,
          connDisconnectTime = Nothing,
          connDisconnectRetries = 0,
          connPendingAck = False,
          connDataSentThisTick = False
        }

-- | Get connection state.
connectionState :: Connection -> ConnectionState
connectionState = connState

-- | Check if connected.
isConnected :: Connection -> Bool
isConnected conn = connState conn == Connected

-- | Get connection stats.
connectionStats :: Connection -> NetworkStats
connectionStats = connStats

-- | Get number of channels.
channelCount :: Connection -> Word8
channelCount conn = fromIntegral $ V.length (connChannels conn)

-- | Initiate connection.
connect :: MonoTime -> Connection -> Either ConnectionError Connection
connect now conn
  | connState conn /= Disconnected = Left ErrAlreadyConnected
  | otherwise =
      let conn' =
            conn
              { connState = Connecting,
                connRequestTime = Just now,
                connRetryCount = 0
              }
       in Right $ sendConnectionRequest conn'

-- | Initiate disconnect.
disconnect :: DisconnectReason -> MonoTime -> Connection -> Connection
disconnect reason now conn
  | connState conn == Disconnected = conn
  | otherwise =
      let header = createHeaderInternal conn
          pkt =
            OutgoingPacket
              { opHeader = header,
                opType = Disconnect,
                opPayload = BS.singleton (disconnectReasonCode reason)
              }
          conn' =
            conn
              { connState = Disconnecting,
                connDisconnectTime = Just now,
                connDisconnectRetries = 0,
                connSendQueue = connSendQueue conn Seq.|> pkt,
                connLocalSeq = connLocalSeq conn + 1
              }
       in conn'

-- | Send a message on a channel.
sendMessage :: Word8 -> BS.ByteString -> MonoTime -> Connection -> Either ConnectionError Connection
sendMessage channelId payload now conn
  | connState conn /= Connected = Left ErrNotConnected
  | idx >= V.length channels = Left (ErrInvalidChannel channelId)
  | otherwise =
      let channel = channels V.! idx
       in case Channel.channelSend payload now channel of
            Nothing -> Left (ErrChannelError ChannelBufferFull)
            Just (_msgSeq, channel') ->
              let channels' = channels V.// [(idx, channel')]
               in Right conn {connChannels = channels'}
  where
    channels = connChannels conn
    idx = fromIntegral channelId

-- | Receive a message from a channel.
receiveMessage :: Word8 -> Connection -> (Maybe BS.ByteString, Connection)
receiveMessage channelId conn
  | idx >= V.length channels = (Nothing, conn)
  | otherwise =
      let channel = channels V.! idx
          (msgs, channel') = Channel.channelReceive channel
          channels' = channels V.// [(idx, channel')]
       in case msgs of
            [] -> (Nothing, conn {connChannels = channels'})
            (m : _) -> (Just m, conn {connChannels = channels'})
  where
    channels = connChannels conn
    idx = fromIntegral channelId

-- | Create a packet header (increments local sequence).
createHeader :: Connection -> (PacketHeader, Connection)
createHeader conn =
  let header = createHeaderInternal conn
      conn' = conn {connLocalSeq = connLocalSeq conn + 1}
   in (header, conn')

-- | Internal header creation without state update.
createHeaderInternal :: Connection -> PacketHeader
createHeaderInternal conn =
  let (ackSeq, ackBits64) = Rel.getAckInfo (connReliability conn)
      ackBits32 = fromIntegral ackBits64 :: Word32 -- Truncate to 32-bit wire format
   in PacketHeader
        { packetType = Payload, -- Will be overwritten
          sequenceNum = connLocalSeq conn,
          ack = ackSeq,
          ackBitfield = ackBits32
        }

-- | Send connection request packet.
sendConnectionRequest :: Connection -> Connection
sendConnectionRequest conn =
  let header =
        PacketHeader
          { packetType = ConnectionRequest,
            sequenceNum = 0,
            ack = 0,
            ackBitfield = 0
          }
      pkt =
        OutgoingPacket
          { opHeader = header,
            opType = ConnectionRequest,
            opPayload = BS.empty
          }
   in conn {connSendQueue = connSendQueue conn Seq.|> pkt}

-- | Process incoming packet header for reliability.
processIncomingHeader :: PacketHeader -> MonoTime -> Connection -> Connection
processIncomingHeader header now conn =
  let rel = Rel.onPacketReceived (sequenceNum header) (connReliability conn)
      conn' = conn {connReliability = rel, connPendingAck = True}
      -- Update remote sequence if newer
      conn'' =
        if sequenceGreaterThan (sequenceNum header) (connRemoteSeq conn')
          then conn' {connRemoteSeq = sequenceNum header}
          else conn'
      -- Process ACKs (extend 32-bit wire format to 64-bit)
      ackBits64 = fromIntegral (ackBitfield header) :: Word64
      (ackResult, rel') = Rel.processAcks (ack header) ackBits64 now (connReliability conn'')
      ackedPairs = fst ackResult
      conn''' = conn'' {connReliability = rel'}
      -- Feed ack info to cwnd
      conn'''' = case connCwnd conn''' of
        Just cw ->
          let ackedBytes = length ackedPairs * ncMtu (connConfig conn''')
           in if ackedBytes > 0
                then conn''' {connCwnd = Just (cwOnAck ackedBytes cw)}
                else conn'''
        Nothing -> conn'''
      -- Acknowledge messages on channels
      conn''''' = foldl ackChannelMessage conn'''' ackedPairs
   in conn'''''
  where
    ackChannelMessage c (chId, chSeq) =
      let idx = fromIntegral chId
          channels = connChannels c
       in if idx < V.length channels
            then
              let channel = Channel.acknowledgeMessage chSeq (channels V.! idx)
                  channels' = channels V.// [(idx, channel)]
               in c {connChannels = channels'}
            else c

-- | Update connection state (called each tick).
updateTick :: MonoTime -> Connection -> Either ConnectionError Connection
updateTick now conn = case connState conn of
  Disconnected -> Right conn
  Connecting -> updateConnecting now conn
  ChallengeResponse -> Right conn -- Wait for accept
  Connected -> updateConnected now conn
  Disconnecting -> updateDisconnecting now conn

-- | Update while connecting.
updateConnecting :: MonoTime -> Connection -> Either ConnectionError Connection
updateConnecting now conn =
  case connRequestTime conn of
    Nothing -> Right conn
    Just reqTime ->
      let elapsed = elapsedMs reqTime now
          timeoutMs = ncConnectionRequestTimeoutMs (connConfig conn)
       in if elapsed > timeoutMs
            then
              let retries = connRetryCount conn + 1
                  maxRetries = ncConnectionRequestMaxRetries (connConfig conn)
               in if retries > maxRetries
                    then Left ErrTimeout
                    else
                      Right $
                        sendConnectionRequest
                          conn
                            { connRetryCount = retries,
                              connRequestTime = Just now
                            }
            else Right conn

-- | Update while connected.
updateConnected :: MonoTime -> Connection -> Either ConnectionError Connection
updateConnected now conn = do
  -- Check timeout
  let timeSinceRecv = elapsedMs (connLastRecvTime conn) now
      timeoutMs = ncConnectionTimeoutMs (connConfig conn)
  if timeSinceRecv > timeoutMs
    then Left ErrTimeout
    else do
      -- Update congestion
      let stats = connStats conn
          cong = ccUpdate (nsPacketLoss stats) (nsRtt stats) now (connCongestion conn)
          cong' = ccRefillBudget (ncMtu (connConfig conn)) cong
          conn' = conn {connCongestion = cong'}

      -- Update cwnd pacing
      let conn'' = case connCwnd conn' of
            Just cw ->
              let rto = Rel.rtoMs (connReliability conn')
               in conn' {connCwnd = Just (cwUpdatePacing rto cw)}
            Nothing -> conn'

      -- Send keepalive if needed
      let timeSinceSend = elapsedMs (connLastSendTime conn'') now
          keepaliveMs = ncKeepaliveIntervalMs (connConfig conn'')
          conn''' =
            if timeSinceSend > keepaliveMs
              then sendKeepalive conn''
              else conn''

      -- Process channel outgoing messages
      let conn'''' = processChannelOutput now conn'''

      -- Update channels
      let channels' = V.map (Channel.channelUpdate now) (connChannels conn'''')
          conn''''' = conn'''' {connChannels = channels'}

      -- Send AckOnly if needed
      let conn'''''' =
            if connPendingAck conn''''' && not (connDataSentThisTick conn''''')
              then sendAckOnly conn'''''
              else conn'''''

      -- Update stats
      let rel = connReliability conn''''''
          stats' =
            (connStats conn'''''')
              { nsRtt = realToFrac (Rel.srttMs rel),
                nsPacketLoss = Rel.packetLossPercent rel,
                nsBandwidthUp = realToFrac (btBytesPerSecond (connBandwidthUp conn'''''')),
                nsBandwidthDown = realToFrac (btBytesPerSecond (connBandwidthDown conn'''''')),
                nsConnectionQuality = assessConnectionQuality (nsRtt stats) (nsPacketLoss stats * 100)
              }
      Right
        conn''''''
          { connStats = stats',
            connPendingAck = False
          }

-- | Process outgoing messages from channels.
processChannelOutput :: MonoTime -> Connection -> Connection
processChannelOutput now conn =
  let conn' = conn {connDataSentThisTick = False}
   in foldl (processChannelIdx now) conn' (connChannelPriority conn')

processChannelIdx :: MonoTime -> Connection -> Int -> Connection
processChannelIdx now conn chIdx =
  if chIdx >= V.length (connChannels conn)
    then conn
    else processChannelMessages now conn chIdx

processChannelMessages :: MonoTime -> Connection -> Int -> Connection
processChannelMessages now conn chIdx =
  let mtu = ncMtu (connConfig conn)
      canSendCong = ccCanSend 0 mtu (connCongestion conn)
      canSendCwnd = case connCwnd conn of
        Just cw -> cwCanSend mtu cw && cwCanSendPaced now cw
        Nothing -> True
   in if not canSendCong || not canSendCwnd
        then conn
        else
          let channels = connChannels conn
              channel = channels V.! chIdx
           in case Channel.getOutgoingMessage channel of
                Nothing -> conn
                Just (msg, channel') ->
                  let msgData = cmData msg
                      msgSeq = cmSequence msg
                      -- Prepend payload header: channel (3 bits) + is_fragment (1 bit)
                      -- Format: [is_fragment:1][reserved:4][channel:3]
                      headerByte = fromIntegral chIdx .&. 0x07 -- Not a fragment
                      wireData = BS.cons headerByte msgData
                      channels' = channels V.// [(chIdx, channel')]
                      header = createHeaderInternal conn
                      pkt =
                        OutgoingPacket
                          { opHeader = header {packetType = Payload},
                            opType = Payload,
                            opPayload = wireData
                          }
                      cong' = ccDeductBudget (BS.length wireData) (connCongestion conn)
                      cwnd' = case connCwnd conn of
                        Just cw -> Just (cwOnSend (BS.length wireData) now cw)
                        Nothing -> Nothing
                      rel' =
                        if Channel.channelIsReliable channel
                          then
                            Rel.onPacketSent
                              (connLocalSeq conn)
                              now
                              (fromIntegral chIdx)
                              msgSeq
                              (BS.length wireData)
                              (connReliability conn)
                          else connReliability conn
                      conn' =
                        conn
                          { connChannels = channels',
                            connSendQueue = connSendQueue conn Seq.|> pkt,
                            connLocalSeq = connLocalSeq conn + 1,
                            connCongestion = cong',
                            connCwnd = cwnd',
                            connReliability = rel',
                            connDataSentThisTick = True
                          }
                   in processChannelMessages now conn' chIdx

-- | Send keepalive packet.
sendKeepalive :: Connection -> Connection
sendKeepalive conn =
  let header = createHeaderInternal conn
      pkt =
        OutgoingPacket
          { opHeader = header {packetType = Keepalive},
            opType = Keepalive,
            opPayload = BS.empty
          }
   in conn
        { connSendQueue = connSendQueue conn Seq.|> pkt,
          connLocalSeq = connLocalSeq conn + 1
        }

-- | Send AckOnly packet.
sendAckOnly :: Connection -> Connection
sendAckOnly conn =
  let header = createHeaderInternal conn
      pkt =
        OutgoingPacket
          { opHeader = header {packetType = Keepalive}, -- Use Keepalive as AckOnly
            opType = Keepalive,
            opPayload = BS.empty
          }
   in conn
        { connSendQueue = connSendQueue conn Seq.|> pkt,
          connLocalSeq = connLocalSeq conn + 1
        }

-- | Update while disconnecting.
updateDisconnecting :: MonoTime -> Connection -> Either ConnectionError Connection
updateDisconnecting now conn =
  case connDisconnectTime conn of
    Nothing -> Right conn
    Just discTime ->
      let elapsed = elapsedMs discTime now
          timeoutMs = ncDisconnectRetryTimeoutMs (connConfig conn)
       in if elapsed > timeoutMs
            then
              let retries = connDisconnectRetries conn
                  maxRetries = ncDisconnectRetries (connConfig conn)
               in if retries >= maxRetries
                    then Right $ resetConnection conn {connState = Disconnected}
                    else
                      let header = createHeaderInternal conn
                          pkt =
                            OutgoingPacket
                              { opHeader = header,
                                opType = Disconnect,
                                opPayload = BS.singleton (disconnectReasonCode ReasonRequested)
                              }
                       in Right
                            conn
                              { connDisconnectRetries = retries + 1,
                                connDisconnectTime = Just now,
                                connSendQueue = connSendQueue conn Seq.|> pkt,
                                connLocalSeq = connLocalSeq conn + 1
                              }
            else Right conn

-- | Reset connection state.
resetConnection :: Connection -> Connection
resetConnection conn =
  let config = connConfig conn
   in conn
        { connStartTime = Nothing,
          connRequestTime = Nothing,
          connLocalSeq = 0,
          connRemoteSeq = 0,
          connSendQueue = Seq.empty,
          connDisconnectTime = Nothing,
          connDisconnectRetries = 0,
          connPendingAck = False,
          connDataSentThisTick = False,
          connChannels = V.map Channel.resetChannel (connChannels conn),
          connCongestion =
            newCongestionController
              (ncSendRate config)
              (ncCongestionBadLossThreshold config)
              (ncCongestionGoodRttThreshold config)
              (ncCongestionRecoveryTimeMs config),
          connBandwidthUp = newBandwidthTracker 1000.0,
          connBandwidthDown = newBandwidthTracker 1000.0
        }

-- | Drain send queue.
drainSendQueue :: Connection -> ([OutgoingPacket], Connection)
drainSendQueue conn =
  let packets = foldr (:) [] (connSendQueue conn)
   in (packets, conn {connSendQueue = Seq.empty})

-- | Update last receive time.
touchRecvTime :: MonoTime -> Connection -> Connection
touchRecvTime now conn = conn {connLastRecvTime = now}

-- | Update last send time.
touchSendTime :: MonoTime -> Connection -> Connection
touchSendTime now conn = conn {connLastSendTime = now}

-- | Mark connection as established (Connected state).
-- Used after handshake completes.
markConnected :: MonoTime -> Connection -> Connection
markConnected now conn =
  conn
    { connState = Connected,
      connStartTime = Just now,
      connLocalSeq = 0,
      connRemoteSeq = 0
    }

-- | Record bytes sent for bandwidth tracking.
recordBytesSent :: Int -> MonoTime -> Connection -> Connection
recordBytesSent bytes now conn =
  let bw = btRecord bytes now (connBandwidthUp conn)
      stats = connStats conn
      stats' =
        stats
          { nsPacketsSent = nsPacketsSent stats + 1,
            nsBytesSent = nsBytesSent stats + fromIntegral bytes
          }
   in conn
        { connBandwidthUp = bw,
          connStats = stats',
          connLastSendTime = now
        }

-- | Record bytes received for bandwidth tracking.
recordBytesReceived :: Int -> MonoTime -> Connection -> Connection
recordBytesReceived bytes now conn =
  let bw = btRecord bytes now (connBandwidthDown conn)
      stats = connStats conn
      stats' =
        stats
          { nsPacketsReceived = nsPacketsReceived stats + 1,
            nsBytesReceived = nsBytesReceived stats + fromIntegral bytes
          }
   in conn
        { connBandwidthDown = bw,
          connStats = stats'
        }
