{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : GBNet.Types
-- Description : Domain-specific newtypes for type safety
--
-- Wraps raw primitives in newtypes so the compiler catches type mix-ups
-- at compile time. Zero runtime cost via newtype erasure.
module GBNet.Types
  ( -- * Channel identifier
    ChannelId (..),
    channelIdToInt,

    -- * Sequence number
    SequenceNum (..),

    -- * Message identifier
    MessageId (..),
  )
where

import Data.Word (Word16, Word32, Word8)
import GBNet.Serialize.BitBuffer (readBitsM, writeBits)
import GBNet.Serialize.Class
  ( BitDeserialize (..),
    BitSerialize (..),
    runDeserialize,
    word16BitWidth,
    word32BitWidth,
    word8BitWidth,
  )

-- | Channel identifier (3-bit wire format, stored as Word8).
-- Not a quantity — no 'Num' instance. Construct via @ChannelId 0@.
newtype ChannelId = ChannelId {unChannelId :: Word8}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Bounded)

-- | Convert a 'ChannelId' to 'Int' for IntMap indexing.
channelIdToInt :: ChannelId -> Int
channelIdToInt = fromIntegral . unChannelId
{-# INLINE channelIdToInt #-}

instance BitSerialize ChannelId where
  bitSerialize (ChannelId val) = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize ChannelId where
  bitDeserialize = runDeserialize $ ChannelId . fromIntegral <$> readBitsM word8BitWidth

-- | Sequence number with Word16 wraparound arithmetic.
-- Derives 'Num' because @seq + 1@ is the canonical operation.
newtype SequenceNum = SequenceNum {unSequenceNum :: Word16}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Bounded, Enum, Real, Integral)

instance BitSerialize SequenceNum where
  bitSerialize (SequenceNum val) = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize SequenceNum where
  bitDeserialize = runDeserialize $ SequenceNum . fromIntegral <$> readBitsM word16BitWidth

-- | Fragment message identifier.
-- Derives 'Num' for increment patterns in fragmentation.
newtype MessageId = MessageId {unMessageId :: Word32}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Bounded)

instance BitSerialize MessageId where
  bitSerialize (MessageId val) = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize MessageId where
  bitDeserialize = runDeserialize $ MessageId . fromIntegral <$> readBitsM word32BitWidth
