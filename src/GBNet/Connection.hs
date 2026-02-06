{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- * Connection (opaque)
    Connection,
    newConnection,

    -- * State queries
    connectionState,
    isConnected,
    connectionStats,
    connRemoteSeq,
    connLocalSeq,
    connClientSalt,

    -- * Operations
    connect,
    disconnect,
    sendMessage,
    receiveMessage,
    receiveIncomingPayload,

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

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', sortBy)
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64, Word8)
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
    ccCongestionLevel,
    ccDeductBudget,
    ccRefillBudget,
    ccUpdate,
    cwCanSend,
    cwCanSendPaced,
    cwCongestionLevel,
    cwOnAck,
    cwOnLoss,
    cwOnSend,
    cwSlowStartRestart,
    cwUpdatePacing,
    newBandwidthTracker,
    newCongestionController,
    newCongestionWindow,
  )
import GBNet.Packet (PacketHeader (..), PacketType (..))
import GBNet.Reliability (MonoTime, ReliableEndpoint, elapsedMs)
import qualified GBNet.Reliability as Rel
import GBNet.Stats
  ( CongestionLevel (..),
    NetworkStats (..),
    assessConnectionQuality,
    defaultNetworkStats,
  )
import GBNet.Types (ChannelId (..), SequenceNum (..), channelIdToInt)
import Optics ((%), (%~), (&), (.~), (?~))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Bandwidth tracking window duration in milliseconds.
bandwidthWindowMs :: Double
bandwidthWindowMs = 1000.0

-- | States of the connection state machine.
data ConnectionState
  = Disconnected
  | Connecting
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
  | ErrInvalidChannel ChannelId
  | ErrChannelError ChannelError
  | ErrMessageTooLarge
  deriving (Eq, Show)

instance NFData ConnectionError where rnf = rwhnf

-- | Packet waiting to be sent.
data OutgoingPacket = OutgoingPacket
  { opHeader :: !PacketHeader,
    opType :: !PacketType,
    opPayload :: !BS.ByteString
  }
  deriving (Show)

instance NFData OutgoingPacket where rnf = rwhnf

makeFieldLabelsNoPrefix ''OutgoingPacket

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
    connLocalSeq :: !SequenceNum,
    -- Subsystems
    connReliability :: !ReliableEndpoint,
    connChannels :: !(IntMap Channel),
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

instance NFData Connection where rnf = rwhnf

makeFieldLabelsNoPrefix ''Connection

-- | Modify a single channel by index, leaving others unchanged.
modifyChannel :: Int -> (Channel -> Channel) -> Connection -> Connection
modifyChannel idx f conn = conn & #connChannels %~ IntMap.adjust f idx
{-# INLINE modifyChannel #-}

-- | Create a new connection.
newConnection :: NetworkConfig -> Word64 -> MonoTime -> Connection
newConnection config clientSalt now =
  let numChannels = ncMaxChannels config
      defaultCfg = ncDefaultChannelConfig config
      -- Pad custom configs with defaults, then take exactly numChannels
      configs = take numChannels $ ncChannelConfigs config ++ repeat defaultCfg
      channels = IntMap.fromList $ zip [0 ..] $ zipWith Channel.newChannel (map ChannelId [0 ..]) configs
      -- Sort channel indices by priority (highest first)
      -- Zip indices with configs, sort by priority, extract indices
      priorityOrder =
        map fst $ sortBy (comparing (Down . Channel.ccPriority . snd)) $ zip [0 ..] configs
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
{-# INLINE connectionState #-}

-- | Check if connected.
isConnected :: Connection -> Bool
isConnected conn = connState conn == Connected
{-# INLINE isConnected #-}

-- | Get connection stats.
connectionStats :: Connection -> NetworkStats
connectionStats = connStats

-- | Get remote sequence number (delegates to reliability layer).
connRemoteSeq :: Connection -> SequenceNum
connRemoteSeq = Rel.reRemoteSequence . connReliability
{-# INLINE connRemoteSeq #-}

-- | Get number of channels.
channelCount :: Connection -> Word8
channelCount = fromIntegral . IntMap.size . connChannels

-- | Initiate connection.
connect :: MonoTime -> Connection -> Either ConnectionError Connection
connect now conn
  | connState conn /= Disconnected = Left ErrAlreadyConnected
  | otherwise =
      Right $
        sendConnectionRequest $
          conn
            & #connState
            .~ Connecting
            & #connRequestTime
            ?~ now
            & #connRetryCount
            .~ 0

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
       in conn
            & #connState
            .~ Disconnecting
            & #connDisconnectTime
            ?~ now
            & #connDisconnectRetries
            .~ 0
            & #connSendQueue
            %~ (Seq.|> pkt)
            & #connLocalSeq
            %~ (+ 1)

-- | Send a message on a channel.
sendMessage :: ChannelId -> BS.ByteString -> MonoTime -> Connection -> Either ConnectionError Connection
sendMessage channelId payload now conn
  | connState conn /= Connected = Left ErrNotConnected
  | otherwise =
      case IntMap.lookup idx (connChannels conn) of
        Nothing -> Left (ErrInvalidChannel channelId)
        Just channel ->
          case Channel.channelSend payload now channel of
            Left chErr -> Left (ErrChannelError chErr)
            Right (_msgSeq, channel') ->
              Right $ modifyChannel idx (const channel') conn
  where
    idx = channelIdToInt channelId

-- | Receive all pending messages from a channel.
receiveMessage :: ChannelId -> Connection -> ([BS.ByteString], Connection)
receiveMessage channelId conn =
  case IntMap.lookup idx (connChannels conn) of
    Nothing -> ([], conn)
    Just channel ->
      let (msgs, channel') = Channel.channelReceive channel
       in (msgs, modifyChannel idx (const channel') conn)
  where
    idx = channelIdToInt channelId

-- | Route an incoming payload through the appropriate channel.
receiveIncomingPayload :: ChannelId -> SequenceNum -> BS.ByteString -> MonoTime -> Connection -> Connection
receiveIncomingPayload channelId chSeq payload now conn =
  case IntMap.lookup idx (connChannels conn) of
    Nothing -> conn
    Just _ -> modifyChannel idx (Channel.onMessageReceived chSeq payload now) conn
  where
    idx = channelIdToInt channelId

-- | Create a packet header (increments local sequence).
createHeader :: Connection -> (PacketHeader, Connection)
createHeader conn =
  let header = createHeaderInternal conn
   in (header, conn & #connLocalSeq %~ (+ 1))

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
   in conn & #connSendQueue %~ (Seq.|> pkt)

-- | Process incoming packet header for reliability.
processIncomingHeader :: PacketHeader -> MonoTime -> Connection -> Connection
processIncomingHeader header now conn0 =
  let -- Record received packet for ACK generation
      conn1 =
        conn0
          & #connReliability %~ Rel.onPacketsReceived [sequenceNum header]
          & #connPendingAck .~ True
      -- Process ACKs (extend 32-bit wire format to 64-bit)
      ackBits64 = fromIntegral (ackBitfield header) :: Word64
      (ackResult, rel') = Rel.processAcks (ack header) ackBits64 now (connReliability conn1)
      ackedPairs = Rel.arAcked ackResult
      fastRetransmits = Rel.arFastRetransmit ackResult
      conn2 = conn1 & #connReliability .~ rel'
      -- Feed ack/loss info to cwnd
      conn3 = case connCwnd conn2 of
        Just cwVal ->
          let hasLoss = not (null fastRetransmits)
              ackedBytes = length ackedPairs * ncMtu (connConfig conn2)
              cwVal'
                | hasLoss = cwOnLoss cwVal
                | ackedBytes > 0 = cwOnAck ackedBytes cwVal
                | otherwise = cwVal
           in conn2 & #connCwnd .~ Just cwVal'
        Nothing -> conn2
      -- Acknowledge messages on channels
      conn4 = foldl' (\c (chId, chSeq) -> modifyChannel (channelIdToInt chId) (Channel.acknowledgeMessage chSeq) c) conn3 ackedPairs
   in conn4

-- | Update connection state (called each tick).
updateTick :: MonoTime -> Connection -> Either ConnectionError Connection
updateTick now conn = case connState conn of
  Disconnected -> Right conn
  Connecting -> updateConnecting now conn
  Connected -> updateConnected now conn
  Disconnecting -> updateDisconnecting now conn

-- | Update while connecting.
updateConnecting :: MonoTime -> Connection -> Either ConnectionError Connection
updateConnecting now conn =
  case connRequestTime conn of
    Nothing -> Right conn
    Just reqTime
      | elapsed <= timeoutMs -> Right conn
      | retries > maxRetries -> Left ErrTimeout
      | otherwise ->
          Right $
            sendConnectionRequest $
              conn
                & #connRetryCount
                .~ retries
                & #connRequestTime
                ?~ now
      where
        elapsed = elapsedMs reqTime now
        timeoutMs = ncConnectionRequestTimeoutMs (connConfig conn)
        retries = connRetryCount conn + 1
        maxRetries = ncConnectionRequestMaxRetries (connConfig conn)

-- | Update while connected.
updateConnected :: MonoTime -> Connection -> Either ConnectionError Connection
updateConnected now conn
  | timeSinceRecv > timeoutMs = Left ErrTimeout
  | otherwise = Right $ updateConnectedPure now conn
  where
    timeSinceRecv = elapsedMs (connLastRecvTime conn) now
    timeoutMs = ncConnectionTimeoutMs (connConfig conn)

-- | Pure connected update (after timeout check passes).
updateConnectedPure :: MonoTime -> Connection -> Connection
updateConnectedPure now conn0 =
  let -- Update congestion
      cfg = connConfig conn0
      cong' = ccRefillBudget (ncMtu cfg) $ ccUpdate (nsPacketLoss (connStats conn0)) (nsRtt (connStats conn0)) now (connCongestion conn0)
      conn1 = conn0 & #connCongestion .~ cong'
      -- Update cwnd pacing and check for slow start restart
      conn2 = case connCwnd conn1 of
        Just cwVal ->
          let rto = Rel.rtoMs (connReliability conn1)
              cwPaced = cwUpdatePacing rto $ cwSlowStartRestart rto now cwVal
           in conn1 & #connCwnd .~ Just cwPaced
        Nothing -> conn1
      -- Send keepalive if needed
      timeSinceSend = elapsedMs (connLastSendTime conn2) now
      keepaliveMs = ncKeepaliveIntervalMs cfg
      conn3
        | timeSinceSend > keepaliveMs = sendKeepalive conn2
        | otherwise = conn2
      -- Process channel outgoing messages
      conn4 = processChannelOutput now conn3
      -- Update channels
      conn5 = conn4 & #connChannels %~ IntMap.map (Channel.channelUpdate now)
      -- Send AckOnly if needed
      conn6
        | connPendingAck conn5 && not (connDataSentThisTick conn5) = sendAckOnly conn5
        | otherwise = conn5
      -- Compute congestion level (worst of binary and window-based)
      binaryLevel = ccCongestionLevel (connCongestion conn6)
      windowLevel = maybe CongestionNone cwCongestionLevel (connCwnd conn6)
      congLevel = max binaryLevel windowLevel
      -- Update stats
      rel' = connReliability conn6
      conn7 =
        conn6
          & #connStats % #nsRtt .~ Rel.srttMs rel'
          & #connStats % #nsPacketLoss .~ Rel.packetLossPercent rel'
          & #connStats % #nsBandwidthUp .~ btBytesPerSecond (connBandwidthUp conn6)
          & #connStats % #nsBandwidthDown .~ btBytesPerSecond (connBandwidthDown conn6)
          & #connStats % #nsConnectionQuality .~ assessConnectionQuality (Rel.srttMs rel') (Rel.packetLossPercent rel' * 100)
          & #connStats % #nsCongestionLevel .~ congLevel
          & #connPendingAck .~ False
   in conn7

-- | Process outgoing messages from channels.
processChannelOutput :: MonoTime -> Connection -> Connection
processChannelOutput now conn =
  let conn' = conn & #connDataSentThisTick .~ False
   in foldl' (processChannelIdx now) conn' (connChannelPriority conn')

processChannelIdx :: MonoTime -> Connection -> Int -> Connection
processChannelIdx now conn chIdx =
  if IntMap.member chIdx (connChannels conn)
    then processChannelMessages now conn chIdx
    else conn

processChannelMessages :: MonoTime -> Connection -> Int -> Connection
processChannelMessages now conn chIdx =
  let mtu = ncMtu (connConfig conn)
      canSendCong = ccCanSend 0 mtu (connCongestion conn)
      canSendCwnd = case connCwnd conn of
        Just cw -> cwCanSend mtu cw && cwCanSendPaced now cw
        Nothing -> True
   in if not canSendCong || not canSendCwnd
        then conn
        else case IntMap.lookup chIdx (connChannels conn) of
          Nothing -> conn
          Just channel ->
            case Channel.getOutgoingMessage channel of
              Nothing -> conn
              Just (msg, channel') ->
                let msgData = cmData msg
                    msgSeqRaw = unSequenceNum (cmSequence msg)
                    -- Prepend payload header: channel (3 bits) + is_fragment (1 bit)
                    -- Format: [headerByte:1][channelSeqHi:1][channelSeqLo:1][msgData:N]
                    headerByte = fromIntegral chIdx .&. 0x07 -- Not a fragment
                    seqHi = fromIntegral (msgSeqRaw `shiftR` 8) :: Word8
                    seqLo = fromIntegral (msgSeqRaw .&. 0xFF) :: Word8
                    wireData = BS.cons headerByte $ BS.cons seqHi $ BS.cons seqLo msgData
                    header = createHeaderInternal conn
                    pkt =
                      OutgoingPacket
                        { opHeader = header {packetType = Payload},
                          opType = Payload,
                          opPayload = wireData
                        }
                    cong' = ccDeductBudget (BS.length wireData) (connCongestion conn)
                    cwnd' = fmap (cwOnSend (BS.length wireData) now) (connCwnd conn)
                    rel' =
                      if Channel.channelIsReliable channel
                        then
                          Rel.onPacketSent
                            (connLocalSeq conn)
                            now
                            (ChannelId (fromIntegral chIdx))
                            (cmSequence msg)
                            (BS.length wireData)
                            (connReliability conn)
                        else connReliability conn
                    conn' =
                      conn
                        & #connChannels
                        %~ IntMap.insert chIdx channel'
                        & #connSendQueue
                        %~ (Seq.|> pkt)
                        & #connLocalSeq
                        %~ (+ 1)
                        & #connCongestion
                        .~ cong'
                        & #connCwnd
                        .~ cwnd'
                        & #connReliability
                        .~ rel'
                        & #connDataSentThisTick
                        .~ True
                 in processChannelMessages now conn' chIdx

-- | Enqueue an empty keepalive/ack-only packet.
-- Both keepalive and ack-only use the same wire format (Keepalive with empty payload).
sendKeepalive, sendAckOnly :: Connection -> Connection
sendKeepalive = enqueueEmptyPacket
sendAckOnly = enqueueEmptyPacket

-- | Shared implementation for keepalive and ack-only packets.
enqueueEmptyPacket :: Connection -> Connection
enqueueEmptyPacket conn =
  let header = createHeaderInternal conn
      pkt =
        OutgoingPacket
          { opHeader = header {packetType = Keepalive},
            opType = Keepalive,
            opPayload = BS.empty
          }
   in conn
        & #connSendQueue
        %~ (Seq.|> pkt)
        & #connLocalSeq
        %~ (+ 1)

-- | Update while disconnecting.
updateDisconnecting :: MonoTime -> Connection -> Either ConnectionError Connection
updateDisconnecting now conn =
  case connDisconnectTime conn of
    Nothing -> Right conn
    Just discTime
      | elapsed <= timeoutMs -> Right conn
      | retries >= maxRetries -> Right $ resetConnection $ conn & #connState .~ Disconnected
      | otherwise ->
          let header = createHeaderInternal conn
              pkt =
                OutgoingPacket
                  { opHeader = header,
                    opType = Disconnect,
                    opPayload = BS.singleton (disconnectReasonCode ReasonRequested)
                  }
           in Right $
                conn
                  & #connDisconnectRetries
                  .~ (retries + 1)
                  & #connDisconnectTime
                  ?~ now
                  & #connSendQueue
                  %~ (Seq.|> pkt)
                  & #connLocalSeq
                  %~ (+ 1)
      where
        elapsed = elapsedMs discTime now
        timeoutMs = ncDisconnectRetryTimeoutMs (connConfig conn)
        retries = connDisconnectRetries conn
        maxRetries = ncDisconnectRetries (connConfig conn)

-- | Reset connection state.
resetConnection :: Connection -> Connection
resetConnection conn =
  let config = connConfig conn
   in conn
        & #connStartTime .~ Nothing
        & #connRequestTime .~ Nothing
        & #connLocalSeq .~ 0
        & #connSendQueue .~ Seq.empty
        & #connDisconnectTime .~ Nothing
        & #connDisconnectRetries .~ 0
        & #connPendingAck .~ False
        & #connDataSentThisTick .~ False
        & #connChannels .~ IntMap.map Channel.resetChannel (connChannels conn)
        & #connCongestion
          .~ newCongestionController
            (ncSendRate config)
            (ncCongestionBadLossThreshold config)
            (ncCongestionGoodRttThreshold config)
            (ncCongestionRecoveryTimeMs config)
        & #connBandwidthUp .~ newBandwidthTracker bandwidthWindowMs
        & #connBandwidthDown .~ newBandwidthTracker bandwidthWindowMs

-- | Drain send queue.
drainSendQueue :: Connection -> ([OutgoingPacket], Connection)
drainSendQueue conn =
  (toList (connSendQueue conn), conn & #connSendQueue .~ Seq.empty)

-- | Update last receive time.
touchRecvTime :: MonoTime -> Connection -> Connection
touchRecvTime now conn = conn & #connLastRecvTime .~ now
{-# INLINE touchRecvTime #-}

-- | Update last send time.
touchSendTime :: MonoTime -> Connection -> Connection
touchSendTime now conn = conn & #connLastSendTime .~ now
{-# INLINE touchSendTime #-}

-- | Mark connection as established (Connected state).
-- Used after handshake completes.
markConnected :: MonoTime -> Connection -> Connection
markConnected now conn =
  conn
    & #connState
    .~ Connected
    & #connStartTime
    ?~ now
    & #connLocalSeq
    .~ 0

-- | Record bytes sent for bandwidth tracking.
recordBytesSent :: Int -> MonoTime -> Connection -> Connection
recordBytesSent bytes now conn =
  let bw = btRecord bytes now (connBandwidthUp conn)
   in conn
        & #connBandwidthUp
        .~ bw
        & #connStats
        % #nsPacketsSent
        %~ (+ 1)
        & #connStats
        % #nsBytesSent
        %~ (+ fromIntegral bytes)
        & #connLastSendTime
        .~ now

-- | Record bytes received for bandwidth tracking.
recordBytesReceived :: Int -> MonoTime -> Connection -> Connection
recordBytesReceived bytes now conn =
  let bw = btRecord bytes now (connBandwidthDown conn)
   in conn
        & #connBandwidthDown
        .~ bw
        & #connStats
        % #nsPacketsReceived
        %~ (+ 1)
        & #connStats
        % #nsBytesReceived
        %~ (+ fromIntegral bytes)
