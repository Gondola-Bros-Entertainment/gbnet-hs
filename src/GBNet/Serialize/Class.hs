-- |
-- Module      : GBNet.Serialize.Class
-- Description : Typeclasses for bitpacked serialization
--
-- Defines 'BitSerialize' and 'BitDeserialize' — the Haskell equivalents
-- of the @BitSerialize@ / @BitDeserialize@ traits in the Rust library.

module GBNet.Serialize.Class
  ( BitSerialize(..)
  , BitDeserialize(..)
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import GBNet.Serialize.BitBuffer

-- | Types that can be serialized into a bit buffer.
class BitSerialize a where
  bitSerialize :: a -> BitBuffer -> BitBuffer

-- | Types that can be deserialized from a bit buffer.
class BitDeserialize a where
  bitDeserialize :: BitBuffer -> Either String (ReadResult a)

-- --------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------

-- | Bool: 1 bit.
instance BitSerialize Bool where
  bitSerialize val = writeBit val

instance BitDeserialize Bool where
  bitDeserialize = readBit

-- | Word8: 8 bits.
instance BitSerialize Word8 where
  bitSerialize val = writeBits (fromIntegral val) 8

instance BitDeserialize Word8 where
  bitDeserialize buf =
    case readBits 8 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word16: 16 bits.
instance BitSerialize Word16 where
  bitSerialize val = writeBits (fromIntegral val) 16

instance BitDeserialize Word16 where
  bitDeserialize buf =
    case readBits 16 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word32: 32 bits.
instance BitSerialize Word32 where
  bitSerialize val = writeBits (fromIntegral val) 32

instance BitDeserialize Word32 where
  bitDeserialize buf =
    case readBits 32 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word64: 64 bits.
instance BitSerialize Word64 where
  bitSerialize val = writeBits val 64

instance BitDeserialize Word64 where
  bitDeserialize = readBits 64
