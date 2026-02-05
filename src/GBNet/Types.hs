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
import Foreign.Storable (Storable (..))

-- | Channel identifier (stored as Word8).
-- Not a quantity — no 'Num' instance. Construct via @ChannelId 0@.
newtype ChannelId = ChannelId {unChannelId :: Word8}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Bounded, Storable)

-- | Convert a 'ChannelId' to 'Int' for IntMap indexing.
channelIdToInt :: ChannelId -> Int
channelIdToInt = fromIntegral . unChannelId
{-# INLINE channelIdToInt #-}

-- | Sequence number with Word16 wraparound arithmetic.
-- Derives 'Num' because @seq + 1@ is the canonical operation.
newtype SequenceNum = SequenceNum {unSequenceNum :: Word16}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Bounded, Enum, Real, Integral, Storable)

-- | Fragment message identifier.
-- Derives 'Num' for increment patterns in fragmentation.
newtype MessageId = MessageId {unMessageId :: Word32}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Bounded, Storable)
