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

import Control.Monad.State.Strict (State, execState, gets, modify')
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', sortBy)
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

-- | Modify a single channel by index, leaving others unchanged.
modifyChannel :: Int -> (Channel -> Channel) -> Connection -> Connection
modifyChannel idx f conn = conn {connChannels = IntMap.adjust f idx (connChannels conn)}
{-# INLINE modifyChannel #-}

-- | Create a new connection.
newConnection :: NetworkConfig -> Word64 -> MonoTime -> Connection
newConnection config clientSalt now =
  let numChannels = ncMaxChannels config
      defaultCfg = ncDefaultChannelConfig config
      -- Pad custom configs with defaults, then take exactly numChannels
      configs = take numChannels $ ncChannelConfigs config ++ repeat defaultCfg
      channels = IntMap.fromList $ zip [0 ..] $ zipWith Channel.newChannel [0 ..] configs
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
{-# INLINE connectionState #-}

-- | Check if connected.
isConnected :: Connection -> Bool
isConnected conn = connState conn == Connected
{-# INLINE isConnected #-}

-- | Get connection stats.
connectionStats :: Connection -> NetworkStats
connectionStats = connStats

-- | Get number of channels.
channelCount :: Connection -> Word8
channelCount = fromIntegral . IntMap.size . connChannels

-- | Initiate connection.
connect :: MonoTime -> Connection -> Either ConnectionError Connection
connect now conn
  | connState conn /= Disconnected = Left ErrAlreadyConnected
  | otherwise =
      Right $
        sendConnectionRequest
          conn
            { connState = Connecting,
              connRequestTime = Just now,
              connRetryCount = 0
            }

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
            { connState = Disconnecting,
              connDisconnectTime = Just now,
              connDisconnectRetries = 0,
              connSendQueue = connSendQueue conn Seq.|> pkt,
              connLocalSeq = connLocalSeq conn + 1
            }

-- | Send a message on a channel.
sendMessage :: Word8 -> BS.ByteString -> MonoTime -> Connection -> Either ConnectionError Connection
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
    idx = fromIntegral channelId

-- | Receive all pending messages from a channel.
receiveMessage :: Word8 -> Connection -> ([BS.ByteString], Connection)
receiveMessage channelId conn =
  case IntMap.lookup idx (connChannels conn) of
    Nothing -> ([], conn)
    Just channel ->
      let (msgs, channel') = Channel.channelReceive channel
       in (msgs, modifyChannel idx (const channel') conn)
  where
    idx = fromIntegral channelId

-- | Route an incoming payload through the appropriate channel.
receiveIncomingPayload :: Word8 -> Word16 -> BS.ByteString -> MonoTime -> Connection -> Connection
receiveIncomingPayload channelId chSeq payload now conn =
  case IntMap.lookup idx (connChannels conn) of
    Nothing -> conn
    Just _ -> modifyChannel idx (Channel.onMessageReceived chSeq payload now) conn
  where
    idx = fromIntegral channelId

-- | Create a packet header (increments local sequence).
createHeader :: Connection -> (PacketHeader, Connection)
createHeader conn =
  let header = createHeaderInternal conn
   in (header, conn {connLocalSeq = connLocalSeq conn + 1})

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
processIncomingHeader header now = execState $ do
  -- Record received packet for ACK generation
  modify' $ \c ->
    c
      { connReliability = Rel.onPacketReceived (sequenceNum header) (connReliability c),
        connPendingAck = True
      }

  -- Update remote sequence if newer
  remoteSeq <- gets connRemoteSeq
  if sequenceGreaterThan (sequenceNum header) remoteSeq
    then modify' $ \c -> c {connRemoteSeq = sequenceNum header}
    else pure ()

  -- Process ACKs (extend 32-bit wire format to 64-bit)
  rel <- gets connReliability
  let ackBits64 = fromIntegral (ackBitfield header) :: Word64
      (ackResult, rel') = Rel.processAcks (ack header) ackBits64 now rel
      (ackedPairs, fastRetransmits) = ackResult
  modify' $ \c -> c {connReliability = rel'}

  -- Feed ack/loss info to cwnd
  cw <- gets connCwnd
  cfg <- gets connConfig
  case cw of
    Just cwVal -> do
      let hasLoss = not (null fastRetransmits)
          ackedBytes = length ackedPairs * ncMtu cfg
          cwVal'
            | hasLoss = cwOnLoss cwVal
            | ackedBytes > 0 = cwOnAck ackedBytes cwVal
            | otherwise = cwVal
      modify' $ \c -> c {connCwnd = Just cwVal'}
    Nothing -> pure ()

  -- Acknowledge messages on channels
  mapM_ (\(chId, chSeq) -> modify' $ modifyChannel (fromIntegral chId) (Channel.acknowledgeMessage chSeq)) ackedPairs

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
    else
      Right $ execState (updateConnectedS now) conn

-- | State-based connected update (after timeout check passes).
updateConnectedS :: MonoTime -> State Connection ()
updateConnectedS now = do
  -- Update congestion
  stats <- gets connStats
  cfg <- gets connConfig
  cong <- gets connCongestion
  let cong' = ccRefillBudget (ncMtu cfg) $ ccUpdate (nsPacketLoss stats) (nsRtt stats) now cong
  modify' $ \c -> c {connCongestion = cong'}

  -- Update cwnd pacing and check for slow start restart
  cw <- gets connCwnd
  rel <- gets connReliability
  case cw of
    Just cwVal -> do
      let rto = Rel.rtoMs rel
          cwPaced = cwUpdatePacing rto $ cwSlowStartRestart rto now cwVal
      modify' $ \c -> c {connCwnd = Just cwPaced}
    Nothing -> pure ()

  -- Send keepalive if needed
  lastSend <- gets connLastSendTime
  let timeSinceSend = elapsedMs lastSend now
      keepaliveMs = ncKeepaliveIntervalMs cfg
  if timeSinceSend > keepaliveMs
    then modify' sendKeepalive
    else pure ()

  -- Process channel outgoing messages
  modify' (processChannelOutput now)

  -- Update channels
  modify' $ \c -> c {connChannels = IntMap.map (Channel.channelUpdate now) (connChannels c)}

  -- Send AckOnly if needed
  pending <- gets connPendingAck
  dataSent <- gets connDataSentThisTick
  if pending && not dataSent
    then modify' sendAckOnly
    else pure ()

  -- Compute congestion level (worst of binary and window-based)
  cong'' <- gets connCongestion
  cw' <- gets connCwnd
  let binaryLevel = ccCongestionLevel cong''
      windowLevel = maybe CongestionNone cwCongestionLevel cw'
      congLevel = max binaryLevel windowLevel

  -- Update stats
  rel' <- gets connReliability
  bwUp <- gets connBandwidthUp
  bwDown <- gets connBandwidthDown
  modify' $ \c ->
    c
      { connStats =
          (connStats c)
            { nsRtt = realToFrac (Rel.srttMs rel'),
              nsPacketLoss = Rel.packetLossPercent rel',
              nsBandwidthUp = realToFrac (btBytesPerSecond bwUp),
              nsBandwidthDown = realToFrac (btBytesPerSecond bwDown),
              nsConnectionQuality = assessConnectionQuality (realToFrac (Rel.srttMs rel')) (Rel.packetLossPercent rel' * 100),
              nsCongestionLevel = congLevel
            },
        connPendingAck = False
      }

-- | Process outgoing messages from channels.
processChannelOutput :: MonoTime -> Connection -> Connection
processChannelOutput now conn =
  let conn' = conn {connDataSentThisTick = False}
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
                    msgSeq = cmSequence msg
                    -- Prepend payload header: channel (3 bits) + is_fragment (1 bit)
                    -- Format: [headerByte:1][channelSeqHi:1][channelSeqLo:1][msgData:N]
                    headerByte = fromIntegral chIdx .&. 0x07 -- Not a fragment
                    seqHi = fromIntegral (msgSeq `shiftR` 8) :: Word8
                    seqLo = fromIntegral (msgSeq .&. 0xFF) :: Word8
                    wireData = BS.cons headerByte $ BS.cons seqHi $ BS.cons seqLo msgData
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
                        { connChannels = IntMap.insert chIdx channel' (connChannels conn),
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
          connChannels = IntMap.map Channel.resetChannel (connChannels conn),
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
{-# INLINE touchRecvTime #-}

-- | Update last send time.
touchSendTime :: MonoTime -> Connection -> Connection
touchSendTime now conn = conn {connLastSendTime = now}
{-# INLINE touchSendTime #-}

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
