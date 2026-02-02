-- |
-- Module      : GBNet.Serialize.Class
-- Description : Typeclasses for bitpacked serialization
--
-- == What's a typeclass?
--
-- A typeclass is Haskell's version of a Rust trait. It defines a set of
-- functions that a type must implement.
--
-- Rust:
-- @
-- pub trait BitSerialize {
--     fn bit_serialize<W: BitWrite>(&self, writer: &mut W) -> io::Result<()>;
-- }
--
-- impl BitSerialize for u8 {
--     fn bit_serialize<W: BitWrite>(&self, writer: &mut W) -> io::Result<()> {
--         writer.write_bits(*self as u64, 8)
--     }
-- }
-- @
--
-- Haskell:
-- @
-- class BitSerialize a where
--     bitSerialize :: a -> BitBuffer -> BitBuffer
--
-- instance BitSerialize Word8 where
--     bitSerialize val buf = writeBits (fromIntegral val) 8 buf
-- @
--
-- Key differences:
-- 1. No 'self' — the value is an explicit parameter
-- 2. No mutation — we return a new BitBuffer
-- 3. No generics on the method — the buffer type is concrete
--    (we could make it generic with more typeclasses, but KISS for now)

module GBNet.Serialize.Class
  ( BitSerialize(..)
  , BitDeserialize(..)
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import GBNet.Serialize.BitBuffer

-- | Typeclass for types that can be serialized to bits.
--
-- This is your Rust trait: `pub trait BitSerialize`.
-- 'class' in Haskell = 'trait' in Rust. (Confusingly, not OOP 'class'.)
class BitSerialize a where
  -- | Serialize a value into the bit buffer, returning the updated buffer.
  --
  -- Note: no io::Result here. Writes to our growable buffer can't fail.
  -- In Rust you used Result for writer generality; here we're concrete.
  bitSerialize :: a -> BitBuffer -> BitBuffer

-- | Typeclass for types that can be deserialized from bits.
--
-- This is your Rust trait: `pub trait BitDeserialize`.
-- 'Either String a' = Result<a, String> in Rust.
class BitDeserialize a where
  -- | Deserialize a value from the bit buffer.
  -- Returns either an error message or the value + updated buffer.
  bitDeserialize :: BitBuffer -> Either String (ReadResult a)

-- ────────────────────────────────────────────────────────────────────
-- Instances for primitive types
-- ────────────────────────────────────────────────────────────────────
--
-- 'instance' in Haskell = 'impl Trait for Type' in Rust.
-- Each instance says "here's how this specific type implements the typeclass."

-- | Bool: 1 bit. Exactly like your Rust impl.
instance BitSerialize Bool where
  bitSerialize val = writeBit val
    -- Point-free: we're saying bitSerialize val IS writeBit val.
    -- The BitBuffer parameter is implicitly passed through.
    -- Equivalent to: bitSerialize val buf = writeBit val buf

instance BitDeserialize Bool where
  bitDeserialize = readBit
    -- Even more point-free: bitDeserialize IS readBit.
    -- They have the same signature!

-- | Word8 (u8): 8 bits.
instance BitSerialize Word8 where
  bitSerialize val = writeBits (fromIntegral val) 8
    -- 'fromIntegral' converts between numeric types.
    -- Like 'as u64' in Rust. Haskell is stricter about implicit conversions.

instance BitDeserialize Word8 where
  bitDeserialize buf =
    case readBits 8 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word16 (u16): 16 bits.
instance BitSerialize Word16 where
  bitSerialize val = writeBits (fromIntegral val) 16

instance BitDeserialize Word16 where
  bitDeserialize buf =
    case readBits 16 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word32 (u32): 32 bits.
instance BitSerialize Word32 where
  bitSerialize val = writeBits (fromIntegral val) 32

instance BitDeserialize Word32 where
  bitDeserialize buf =
    case readBits 32 buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word64 (u64): 64 bits.
instance BitSerialize Word64 where
  bitSerialize val = writeBits val 64

instance BitDeserialize Word64 where
  bitDeserialize = readBits 64
