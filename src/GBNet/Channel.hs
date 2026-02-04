-- |
-- Module      : GBNet.Channel
-- Description : Channel-based message delivery with multiple reliability modes
--
-- Implements LiteNetLib-style delivery modes: Unreliable, UnreliableSequenced,
-- ReliableUnordered, ReliableOrdered, and ReliableSequenced. Each channel
-- manages its own sequence numbers, buffering, and retransmission.
module GBNet.Channel
  ( -- * Delivery modes
    DeliveryMode (..),
    isReliable,
    isSequenced,
    isOrdered,

    -- * Configuration
    ChannelConfig (..),
    defaultChannelConfig,
    unreliableConfig,
    reliableOrderedConfig,
    reliableSequencedConfig,

    -- * Channel message
    ChannelMessage (..),

    -- * Channel
    Channel (..),
    newChannel,
    resetChannel,
    channelIsReliable,
    channelSend,
    getOutgoingMessage,
    getRetransmitMessages,
    onMessageReceived,
    acknowledgeMessage,
    channelReceive,
    takePendingAcks,
    channelUpdate,

    -- * Errors
    ChannelError (..),
  )
where

import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word64, Word8)
import GBNet.Reliability (MonoTime, elapsedMs)
import GBNet.Types (ChannelId (..), SequenceNum (..))
import GBNet.Util (sequenceGreaterThan)

-- | Delivery mode for channel messages.
data DeliveryMode
  = -- | Fire and forget, no guarantees
    Unreliable
  | -- | Unreliable but drops out-of-order
    UnreliableSequenced
  | -- | Guaranteed delivery, any order
    ReliableUnordered
  | -- | Guaranteed delivery, strict order
    ReliableOrdered
  | -- | Guaranteed delivery, drops out-of-order
    ReliableSequenced
  deriving (Eq, Show, Enum, Bounded)

-- | Check if a delivery mode guarantees delivery.
isReliable :: DeliveryMode -> Bool
isReliable Unreliable = False
isReliable UnreliableSequenced = False
isReliable _ = True

-- | Check if a delivery mode drops out-of-order messages.
isSequenced :: DeliveryMode -> Bool
isSequenced UnreliableSequenced = True
isSequenced ReliableSequenced = True
isSequenced _ = False

-- | Check if a delivery mode delivers in strict order.
isOrdered :: DeliveryMode -> Bool
isOrdered ReliableOrdered = True
isOrdered _ = False

-- | Channel configuration.
data ChannelConfig = ChannelConfig
  { ccDeliveryMode :: !DeliveryMode,
    ccMaxMessageSize :: !Int,
    ccMessageBufferSize :: !Int,
    ccBlockOnFull :: !Bool,
    -- | Milliseconds
    ccOrderedBufferTimeout :: !Double,
    ccMaxOrderedBufferSize :: !Int,
    ccMaxReliableRetries :: !Int,
    ccPriority :: !Word8
  }
  deriving (Show)

-- | Default channel configuration (ReliableOrdered).
defaultChannelConfig :: ChannelConfig
defaultChannelConfig =
  ChannelConfig
    { ccDeliveryMode = ReliableOrdered,
      ccMaxMessageSize = 1024,
      ccMessageBufferSize = 256,
      ccBlockOnFull = False,
      ccOrderedBufferTimeout = 5000.0,
      ccMaxOrderedBufferSize = 64,
      ccMaxReliableRetries = 10,
      ccPriority = 0
    }

-- | Unreliable channel configuration.
unreliableConfig :: ChannelConfig
unreliableConfig =
  defaultChannelConfig
    { ccDeliveryMode = Unreliable,
      ccBlockOnFull = False
    }

-- | Reliable ordered channel configuration.
reliableOrderedConfig :: ChannelConfig
reliableOrderedConfig =
  defaultChannelConfig
    { ccDeliveryMode = ReliableOrdered
    }

-- | Reliable sequenced channel configuration.
reliableSequencedConfig :: ChannelConfig
reliableSequencedConfig =
  defaultChannelConfig
    { ccDeliveryMode = ReliableSequenced
    }

-- | A message in the channel system.
data ChannelMessage = ChannelMessage
  { cmSequence :: !SequenceNum,
    cmData :: !BS.ByteString,
    cmSendTime :: !MonoTime,
    cmAcked :: !Bool,
    cmRetryCount :: !Int,
    cmReliable :: !Bool
  }
  deriving (Show)

-- | Channel state for message delivery.
data Channel = Channel
  { chConfig :: !ChannelConfig,
    chChannelId :: !ChannelId,
    chLocalSequence :: !SequenceNum,
    chRemoteSequence :: !SequenceNum,
    chSendBuffer :: !(Map SequenceNum ChannelMessage),
    chReceiveBuffer :: !(Seq BS.ByteString), -- O(1) append via |>
    chPendingAck :: ![SequenceNum],
    chOrderedReceiveBuffer :: !(Map SequenceNum (BS.ByteString, MonoTime)),
    chOrderedExpected :: !SequenceNum,
    chTotalSent :: !Word64,
    chTotalReceived :: !Word64,
    chTotalDropped :: !Word64,
    chTotalRetransmits :: !Word64
  }
  deriving (Show)

-- | Create a new channel with the given configuration.
newChannel :: ChannelId -> ChannelConfig -> Channel
newChannel channelId config =
  Channel
    { chConfig = config,
      chChannelId = channelId,
      chLocalSequence = 0,
      chRemoteSequence = 0,
      chSendBuffer = Map.empty,
      chReceiveBuffer = Seq.empty,
      chPendingAck = [],
      chOrderedReceiveBuffer = Map.empty,
      chOrderedExpected = 0,
      chTotalSent = 0,
      chTotalReceived = 0,
      chTotalDropped = 0,
      chTotalRetransmits = 0
    }

-- | Queue a message for sending.
-- Returns 'Left ChannelBufferFull' if the buffer is full and blocking is enabled.
-- Returns 'Left ChannelMessageTooLarge' if the message exceeds the max size.
channelSend :: BS.ByteString -> MonoTime -> Channel -> Either ChannelError (SequenceNum, Channel)
channelSend payload now ch
  | BS.length payload > ccMaxMessageSize (chConfig ch) = Left ChannelMessageTooLarge
  | bufferFull && ccBlockOnFull (chConfig ch) = Left ChannelBufferFull
  | otherwise = Right (seqNum, ch')
  where
    bufferFull = Map.size (chSendBuffer ch) >= ccMessageBufferSize (chConfig ch)
    seqNum = chLocalSequence ch
    reliable = isReliable (ccDeliveryMode (chConfig ch))
    msg =
      ChannelMessage
        { cmSequence = seqNum,
          cmData = payload,
          cmSendTime = now,
          cmAcked = False,
          cmRetryCount = 0,
          cmReliable = reliable
        }
    sendBuf =
      if bufferFull
        then Map.deleteMin (chSendBuffer ch)
        else chSendBuffer ch
    ch' =
      ch
        { chLocalSequence = seqNum + 1,
          chSendBuffer = Map.insert seqNum msg sendBuf,
          chTotalSent = chTotalSent ch + 1
        }

-- | Get the next outgoing message that hasn't been sent yet.
getOutgoingMessage :: Channel -> Maybe (ChannelMessage, Channel)
getOutgoingMessage ch =
  case Map.lookupMin (chSendBuffer ch) of
    Nothing -> Nothing
    Just (seqNum, msg)
      | cmAcked msg ->
          -- Already acked, remove and try next
          getOutgoingMessage (ch {chSendBuffer = Map.delete seqNum (chSendBuffer ch)})
      | cmRetryCount msg == 0 ->
          -- First send
          if cmReliable msg
            then
              -- Reliable: keep in buffer for retransmit
              let msg' = msg {cmRetryCount = 1}
                  ch' = ch {chSendBuffer = Map.insert seqNum msg' (chSendBuffer ch)}
               in Just (msg, ch')
            else
              -- Unreliable: remove from buffer immediately (fire and forget)
              let ch' = ch {chSendBuffer = Map.delete seqNum (chSendBuffer ch)}
               in Just (msg, ch')
      | otherwise -> Nothing -- Already sent, waiting for ack or retransmit

-- | Get messages that need retransmission based on RTO.
getRetransmitMessages :: MonoTime -> Double -> Channel -> ([ChannelMessage], Channel)
getRetransmitMessages now rtoMs ch
  | not (isReliable (ccDeliveryMode (chConfig ch))) = ([], ch)
  | otherwise = Map.foldrWithKey checkRetransmit ([], ch) (chSendBuffer ch)
  where
    maxRetries = ccMaxReliableRetries (chConfig ch)

    checkRetransmit seqNum msg (acc, c)
      | cmAcked msg = (acc, c)
      | cmRetryCount msg == 0 = (acc, c) -- Not sent yet
      | cmRetryCount msg > maxRetries =
          -- Give up on this message
          ( acc,
            c
              { chSendBuffer = Map.delete seqNum (chSendBuffer c),
                chTotalDropped = chTotalDropped c + 1
              }
          )
      | elapsedMs (cmSendTime msg) now >= rtoMs =
          -- Needs retransmit
          let msg' = msg {cmSendTime = now, cmRetryCount = cmRetryCount msg + 1}
              c' =
                c
                  { chSendBuffer = Map.insert seqNum msg' (chSendBuffer c),
                    chTotalRetransmits = chTotalRetransmits c + 1
                  }
           in (msg : acc, c')
      | otherwise = (acc, c)

-- | Process a received message. Returns updated channel.
onMessageReceived :: SequenceNum -> BS.ByteString -> MonoTime -> Channel -> Channel
onMessageReceived seqNum payload now ch =
  case ccDeliveryMode (chConfig ch) of
    Unreliable ->
      ch
        { chReceiveBuffer = chReceiveBuffer ch |> payload,
          chTotalReceived = chTotalReceived ch + 1
        }
    UnreliableSequenced ->
      if sequenceGreaterThan seqNum (chRemoteSequence ch)
        then
          ch
            { chReceiveBuffer = chReceiveBuffer ch |> payload,
              chRemoteSequence = seqNum,
              chTotalReceived = chTotalReceived ch + 1
            }
        else ch {chTotalDropped = chTotalDropped ch + 1}
    ReliableUnordered ->
      ch
        { chReceiveBuffer = chReceiveBuffer ch |> payload,
          chPendingAck = seqNum : chPendingAck ch,
          chTotalReceived = chTotalReceived ch + 1
        }
    ReliableOrdered ->
      let ch' = ch {chPendingAck = seqNum : chPendingAck ch}
       in if seqNum == chOrderedExpected ch'
            then deliverOrdered payload ch'
            else bufferOrdered seqNum payload now ch'
    ReliableSequenced ->
      if sequenceGreaterThan seqNum (chRemoteSequence ch)
        then
          ch
            { chReceiveBuffer = chReceiveBuffer ch |> payload,
              chPendingAck = seqNum : chPendingAck ch,
              chRemoteSequence = max seqNum (chRemoteSequence ch),
              chTotalReceived = chTotalReceived ch + 1
            }
        else
          ch
            { chPendingAck = seqNum : chPendingAck ch,
              chTotalDropped = chTotalDropped ch + 1
            }

-- | Deliver message and flush any buffered consecutive messages.
deliverOrdered :: BS.ByteString -> Channel -> Channel
deliverOrdered payload ch =
  let ch' =
        ch
          { chReceiveBuffer = chReceiveBuffer ch |> payload,
            chOrderedExpected = chOrderedExpected ch + 1,
            chTotalReceived = chTotalReceived ch + 1
          }
   in flushOrderedBuffer ch'

-- | Buffer an out-of-order message for later delivery.
bufferOrdered :: SequenceNum -> BS.ByteString -> MonoTime -> Channel -> Channel
bufferOrdered seqNum payload now ch
  | Map.size (chOrderedReceiveBuffer ch) >= ccMaxOrderedBufferSize (chConfig ch) =
      ch {chTotalDropped = chTotalDropped ch + 1}
  | otherwise =
      ch {chOrderedReceiveBuffer = Map.insert seqNum (payload, now) (chOrderedReceiveBuffer ch)}

-- | Flush consecutive messages from the ordered buffer.
flushOrderedBuffer :: Channel -> Channel
flushOrderedBuffer ch =
  case Map.lookup (chOrderedExpected ch) (chOrderedReceiveBuffer ch) of
    Nothing -> ch
    Just (payload, _) ->
      let ch' =
            ch
              { chOrderedReceiveBuffer = Map.delete (chOrderedExpected ch) (chOrderedReceiveBuffer ch),
                chReceiveBuffer = chReceiveBuffer ch |> payload,
                chOrderedExpected = chOrderedExpected ch + 1,
                chTotalReceived = chTotalReceived ch + 1
              }
       in flushOrderedBuffer ch'

-- | Acknowledge a message by sequence number.
acknowledgeMessage :: SequenceNum -> Channel -> Channel
acknowledgeMessage seqNum ch =
  ch {chSendBuffer = Map.adjust (\msg -> msg {cmAcked = True}) seqNum (chSendBuffer ch)}

-- | Take all received messages from the channel.
channelReceive :: Channel -> ([BS.ByteString], Channel)
channelReceive ch = (toList (chReceiveBuffer ch), ch {chReceiveBuffer = Seq.empty})

-- | Take pending ack sequence numbers.
takePendingAcks :: Channel -> ([SequenceNum], Channel)
takePendingAcks ch = (chPendingAck ch, ch {chPendingAck = []})

-- | Update channel state (flush old ordered messages, clean up acked).
channelUpdate :: MonoTime -> Channel -> Channel
channelUpdate now = flushTimedOutOrdered now . cleanupAcked

-- | Remove acked messages from send buffer.
cleanupAcked :: Channel -> Channel
cleanupAcked ch =
  ch {chSendBuffer = Map.filter (not . cmAcked) (chSendBuffer ch)}

-- | Flush ordered messages that have timed out waiting.
flushTimedOutOrdered :: MonoTime -> Channel -> Channel
flushTimedOutOrdered now ch
  | not (isOrdered (ccDeliveryMode (chConfig ch))) = ch
  | Map.null (chOrderedReceiveBuffer ch) = ch
  | otherwise =
      let timeout = ccOrderedBufferTimeout (chConfig ch)
          (toFlush, toKeep) = Map.partition (timedOut timeout) (chOrderedReceiveBuffer ch)
       in if Map.null toFlush
            then ch
            else
              let payloads = map fst (Map.elems toFlush)
                  newExpected = case Map.lookupMax toFlush of
                    Nothing -> chOrderedExpected ch
                    Just (maxSeq, _) -> maxSeq + 1
               in ch
                    { chOrderedReceiveBuffer = toKeep,
                      chReceiveBuffer = chReceiveBuffer ch <> Seq.fromList payloads,
                      chOrderedExpected = newExpected,
                      chTotalReceived = chTotalReceived ch + fromIntegral (length payloads)
                    }
  where
    timedOut timeout (_, arriveTime) = elapsedMs arriveTime now >= timeout

-- | Reset channel to initial state.
resetChannel :: Channel -> Channel
resetChannel ch =
  ch
    { chLocalSequence = 0,
      chRemoteSequence = 0,
      chSendBuffer = Map.empty,
      chReceiveBuffer = Seq.empty,
      chPendingAck = [],
      chOrderedReceiveBuffer = Map.empty,
      chOrderedExpected = 0,
      chTotalSent = 0,
      chTotalReceived = 0,
      chTotalDropped = 0,
      chTotalRetransmits = 0
    }

-- | Check if channel uses reliable delivery.
channelIsReliable :: Channel -> Bool
channelIsReliable ch = isReliable (ccDeliveryMode (chConfig ch))
{-# INLINE channelIsReliable #-}

-- | Channel errors.
data ChannelError
  = ChannelBufferFull
  | ChannelMessageTooLarge
  deriving (Eq, Show)
