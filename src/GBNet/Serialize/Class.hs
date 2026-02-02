-- |
-- Module      : GBNet.Serialize.Class
-- Description : Typeclasses for bitpacked serialization
--
-- Defines 'BitSerialize' and 'BitDeserialize' — the Haskell equivalents
-- of the @BitSerialize@ / @BitDeserialize@ traits in the Rust library.
--
-- == Typeclasses vs Rust Traits
--
-- Haskell's typeclasses and Rust's traits serve the same purpose: ad-hoc
-- polymorphism. Both let you define a shared interface that multiple types
-- can implement independently:
--
-- @
--   -- Haskell                        // Rust
--   class BitSerialize a where        trait BitSerialize {
--     bitSerialize :: a -> ...            fn bit_serialize(&self, ...) -> ...
--                                     }
--   instance BitSerialize Word8 ...   impl BitSerialize for u8 { ... }
-- @
--
-- Key differences:
--   * Haskell instances are resolved at compile time via dictionary passing
--     (the compiler threads a "vtable" argument implicitly). Rust uses
--     monomorphization (generates a separate copy for each concrete type)
--     or dynamic dispatch via @dyn Trait@.
--   * Haskell typeclasses can be "orphan" instances (defined outside the
--     type's or class's module). Rust's coherence rules forbid this.
--
-- == 'fromIntegral' Conversions
--
-- Throughout this module you'll see 'fromIntegral' — Haskell's generic
-- numeric conversion function. It converts between any two integer types
-- (e.g., @Word8 -> Word64@, @Word64 -> Word16@). This is safe when
-- widening (smaller to larger) and truncates when narrowing (larger to
-- smaller), matching Rust's @as@ casts.
--
-- == The 'ReadResult' Pattern
--
-- Every 'bitDeserialize' returns @Either String (ReadResult a)@ because
-- reading can fail (buffer underflow) and must return the updated buffer.
-- The 'ReadResult' bundles the value with the new buffer state, since
-- Haskell is pure and can't mutate the buffer in place. See
-- "GBNet.Serialize.Reader" for a monad that automates this threading.

module GBNet.Serialize.Class
  ( BitSerialize(..)
  , BitDeserialize(..)
    -- * Bit width constants
  , word8BitWidth
  , word16BitWidth
  , word32BitWidth
  , word64BitWidth
  , packetTypeBitWidth
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)

-- 'castWord32ToFloat' and friends let us reinterpret the bits of a
-- 'Word32' as a 'Float' (IEEE 754) without changing the underlying
-- representation. This is the Haskell equivalent of Rust's
-- @f32::from_bits()@ / @f32::to_bits()@.
import GHC.Float (castWord32ToFloat, castFloatToWord32,
                  castWord64ToDouble, castDoubleToWord64)

import GBNet.Serialize.BitBuffer

-- ====================================================================
-- Bit width constants
-- ====================================================================

-- | Haskell doesn't have a @const@ keyword — every top-level binding is
-- already immutable (there's no @let mut@). So a named binding /is/ a
-- constant. GHC inlines these at compile time, so there's zero runtime
-- overhead compared to writing the literal @8@ everywhere.
--
-- We define these to avoid "magic numbers" — unnamed numeric literals
-- scattered through the code whose meaning isn't obvious at a glance.
-- For example, seeing @writeBits val 16@ doesn't tell you /why/ 16;
-- @writeBits val word16BitWidth@ makes the intent clear.

-- | Bit width of a 'Word8' / 'Int8' (8 bits).
word8BitWidth :: Int
word8BitWidth = 8

-- | Bit width of a 'Word16' / 'Int16' (16 bits).
word16BitWidth :: Int
word16BitWidth = 16

-- | Bit width of a 'Word32' / 'Int32' / 'Float' (32 bits).
word32BitWidth :: Int
word32BitWidth = 32

-- | Bit width of a 'Word64' / 'Int64' / 'Double' (64 bits).
word64BitWidth :: Int
word64BitWidth = 64

-- | Bit width of the 'PacketType' tag in the wire format (4 bits).
-- Supports up to 16 distinct packet types (we currently use 6).
packetTypeBitWidth :: Int
packetTypeBitWidth = 4

-- | Types that can be serialized into a bit buffer.
--
-- The signature @a -> BitBuffer -> BitBuffer@ takes a value and an
-- existing buffer, and returns a new buffer with the value appended.
-- This "builder" style composes nicely:
--
-- @
--   bitSerialize (99 :: Word8) $ bitSerialize True $ empty
-- @
--
-- Note the @$@ operator: it's just function application with low
-- precedence, letting us avoid parentheses. The above is equivalent to:
-- @bitSerialize (99 :: Word8) (bitSerialize True empty)@
class BitSerialize a where
  bitSerialize :: a -> BitBuffer -> BitBuffer

-- | Types that can be deserialized from a bit buffer.
--
-- Returns @Either String (ReadResult a)@ where:
--   * @Left msg@ indicates a read error (like Rust's @Err@)
--   * @Right (ReadResult value buffer')@ gives the value and the
--     buffer with its read cursor advanced (like Rust's @Ok@)
class BitDeserialize a where
  bitDeserialize :: BitBuffer -> Either String (ReadResult a)

-- ====================================================================
-- Unsigned integer instances
-- ====================================================================

-- | Bool: serialized as a single bit.
--
-- 'writeBit' and 'readBit' are defined in "GBNet.Serialize.BitBuffer".
-- The instance declarations here are minimal because the buffer
-- primitives already have the right signatures.
instance BitSerialize Bool where
  bitSerialize val = writeBit val
    -- ^ Point-free style: @bitSerialize val@ returns a function
    -- @BitBuffer -> BitBuffer@, which is exactly what 'writeBit val' is.

instance BitDeserialize Bool where
  bitDeserialize = readBit
    -- ^ Fully point-free: the types match exactly, so we can just
    -- equate the two functions. 'readBit :: BitBuffer -> Either String
    -- (ReadResult Bool)' is already the right signature.

-- | Word8: 8-bit unsigned integer.
--
-- 'fromIntegral' widens @Word8@ to @Word64@ for the 'writeBits' call.
-- This is a zero-cost conversion at the machine level — both fit in a
-- register. The @8@ is the bit width.
instance BitSerialize Word8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize Word8 where
  bitDeserialize buf =
    case readBits word8BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        -- 'fromIntegral' narrows @Word64@ back to @Word8@, keeping
        -- only the low 8 bits. Since we only read 8 bits, this is safe.
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word16: 16-bit unsigned integer.
instance BitSerialize Word16 where
  bitSerialize val = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize Word16 where
  bitDeserialize buf =
    case readBits word16BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word32: 32-bit unsigned integer.
instance BitSerialize Word32 where
  bitSerialize val = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize Word32 where
  bitDeserialize buf =
    case readBits word32BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

-- | Word64: 64-bit unsigned integer.
--
-- No 'fromIntegral' needed here — the value is already a 'Word64'.
instance BitSerialize Word64 where
  bitSerialize val = writeBits val word64BitWidth

instance BitDeserialize Word64 where
  bitDeserialize = readBits word64BitWidth
    -- ^ Point-free: @readBits 64@ already has the type
    -- @BitBuffer -> Either String (ReadResult Word64)@.

-- ====================================================================
-- Signed integer instances
-- ====================================================================

-- Signed integers are serialized using two's complement representation,
-- which is the universal standard for signed integers in modern CPUs.
-- The key insight is that 'fromIntegral' between signed and unsigned
-- types of the same width preserves the bit pattern in Haskell (and in
-- Rust via @as@), so we can reuse the unsigned write/read machinery.
--
-- For example, @(-1 :: Int8)@ has the bit pattern @0xFF@. Converting
-- this to @Word8@ via 'fromIntegral' gives @255@, which has the same
-- bit pattern. On deserialization, converting @255 :: Word8@ back to
-- @Int8@ gives @-1@. The bit pattern round-trips perfectly.

-- | Int8: 8-bit signed integer (two's complement).
instance BitSerialize Int8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth
    -- ^ @fromIntegral@ converts @Int8 -> Word64@, preserving the low
    -- 8 bits in two's complement. For @-1 :: Int8@, this gives
    -- @0x00000000000000FF :: Word64@.

instance BitDeserialize Int8 where
  bitDeserialize buf =
    case readBits word8BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        -- Convert @Word64 -> Word8 -> Int8@. The intermediate @Word8@
        -- step ensures we only keep 8 bits before sign-extending to
        -- @Int8@. Going directly @Word64 -> Int8@ would also work
        -- since 'fromIntegral' truncates, but the two-step conversion
        -- makes the intent clearer.
        Right $ ReadResult
          { readValue  = fromIntegral (fromIntegral val :: Word8)
          , readBuffer = buf'
          }

-- | Int16: 16-bit signed integer (two's complement).
instance BitSerialize Int16 where
  bitSerialize val = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize Int16 where
  bitDeserialize buf =
    case readBits word16BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = fromIntegral (fromIntegral val :: Word16)
          , readBuffer = buf'
          }

-- | Int32: 32-bit signed integer (two's complement).
instance BitSerialize Int32 where
  bitSerialize val = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize Int32 where
  bitDeserialize buf =
    case readBits word32BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = fromIntegral (fromIntegral val :: Word32)
          , readBuffer = buf'
          }

-- | Int64: 64-bit signed integer (two's complement).
instance BitSerialize Int64 where
  bitSerialize val = writeBits (fromIntegral val) word64BitWidth

instance BitDeserialize Int64 where
  bitDeserialize buf =
    case readBits word64BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = fromIntegral val
          , readBuffer = buf'
          }

-- ====================================================================
-- Floating-point instances
-- ====================================================================

-- IEEE 754 floating-point values are serialized by reinterpreting their
-- bit representation as an unsigned integer of the same width, then
-- serializing that integer. This is exactly what Rust's @f32::to_bits()@
-- and @f32::from_bits()@ do.
--
-- GHC provides 'castFloatToWord32' and 'castWord32ToFloat' (and the
-- 64-bit equivalents) for this purpose. These are true bitwise casts —
-- no rounding, no conversion, just reinterpreting the same 32/64 bits.
--
-- A 'Float' (32-bit IEEE 754) has: 1 sign bit, 8 exponent bits, 23
-- mantissa bits = 32 bits total. A 'Double' (64-bit) has: 1 sign bit,
-- 11 exponent bits, 52 mantissa bits = 64 bits total.

-- | Float: 32-bit IEEE 754 single-precision floating-point.
instance BitSerialize Float where
  bitSerialize val = writeBits (fromIntegral (castFloatToWord32 val)) word32BitWidth
    -- ^ Convert Float -> Word32 (bit cast) -> Word64 (widen) -> write 32 bits.

instance BitDeserialize Float where
  bitDeserialize buf =
    case readBits word32BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        -- Convert Word64 -> Word32 (truncate) -> Float (bit cast).
        Right $ ReadResult
          { readValue  = castWord32ToFloat (fromIntegral val)
          , readBuffer = buf'
          }

-- | Double: 64-bit IEEE 754 double-precision floating-point.
instance BitSerialize Double where
  bitSerialize val = writeBits (castDoubleToWord64 val) word64BitWidth
    -- ^ 'castDoubleToWord64' gives us the raw bits directly as a
    -- 'Word64', no intermediate conversion needed.

instance BitDeserialize Double where
  bitDeserialize buf =
    case readBits word64BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = castWord64ToDouble val
          , readBuffer = buf'
          }
