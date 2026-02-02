-- |
-- Module      : GBNet.Serialize.BitBuffer
-- Description : Bit-level read/write buffer for sub-byte serialization
--
-- This is the Haskell equivalent of gbnet::serialize::bit_io::BitBuffer.
-- Instead of a struct with methods, we have a data type with pure functions
-- that return new states — because Haskell is immutable by default.
--
-- == Rust vs Haskell mental model
--
-- In Rust you wrote:
--
-- @
-- let mut buf = BitBuffer::new();
-- buf.write_bits(42, 8)?;  // mutates buf in place
-- buf.write_bits(1, 1)?;   // mutates again
-- let bytes = buf.into_bytes(true)?;
-- @
--
-- In Haskell, there's no mutation. Each operation returns a NEW buffer:
--
-- @
-- let buf0 = empty
-- let buf1 = writeBits 42 8 buf0   -- new buffer, buf0 unchanged
-- let buf2 = writeBits 1  1 buf1   -- new buffer, buf1 unchanged
-- let bytes = toBytes buf2
-- @
--
-- This looks wasteful but GHC (the Haskell compiler) optimizes this
-- heavily — often to the same machine code as mutation.

module GBNet.Serialize.BitBuffer
  ( -- * Types
    BitBuffer       -- The buffer type (exported without constructors — opaque)
  , ReadResult(..)  -- Result of a read operation

    -- * Construction
  , empty           -- Like BitBuffer::new()
  , fromBytes       -- Like BitBuffer::from_bytes()

    -- * Writing
  , writeBit        -- Write a single bit
  , writeBits       -- Write N bits from a value

    -- * Reading
  , readBit         -- Read a single bit
  , readBits        -- Read N bits into a value

    -- * Conversion
  , toBytes         -- Like into_bytes()
  , bitPosition     -- Current write position
  , readPosition    -- Current read position
  ) where

-- IMPORTS
--
-- In Haskell, imports are explicit. No glob imports by convention.
-- This is like `use` in Rust but you list exactly what you're pulling in.

import Data.Word (Word8, Word64)
  -- Word8  = u8 in Rust
  -- Word64 = u64 in Rust
  -- Haskell calls unsigned integers "Word" (historical, from machine words)

import Data.Bits ((.|.), shiftL, testBit, setBit, clearBit)
  -- Bitwise operators. In Rust these are built-in syntax (& | << >>).
  -- In Haskell they're functions from Data.Bits:
  --   (.&.)    = &   (bitwise AND)
  --   (.|.)    = |   (bitwise OR)
  --   shiftL   = <<  (left shift)
  --   shiftR   = >>  (right shift)
  --   testBit  = like (val >> n) & 1
  --   setBit   = like val | (1 << n)
  --   clearBit = like val & !(1 << n)

import qualified Data.ByteString as BS
  -- ByteString is Haskell's Vec<u8>. It's a contiguous byte array.
  -- 'qualified' means we must prefix: BS.pack, BS.index, etc.
  -- This avoids name collisions (like Rust's `use std::collections::HashMap;`)

-- | The bit buffer. Equivalent to your Rust BitBuffer struct.
--
-- In Rust you had:
-- @
-- pub struct BitBuffer {
--     buffer: Vec<u8>,
--     bit_pos: usize,
--     read_pos: usize,
-- }
-- @
--
-- In Haskell, 'data' defines a new type. Each field is a function that
-- extracts that field from the struct (called "record syntax").
--
-- The '!' before each type means "strict" — evaluate immediately.
-- Without it, Haskell would lazily defer computation (bad for perf).
data BitBuffer = BitBuffer
  { bufferBytes :: ![Word8]   -- The raw bytes (like Vec<u8>)
  , bitPos      :: !Int       -- Write position in bits
  , readPos     :: !Int       -- Read position in bits
  }
  -- 'deriving Show' auto-generates a Debug-like printer.
  -- Like #[derive(Debug)] in Rust.
  deriving (Show)

-- | Result of a read operation: the value read AND the updated buffer.
--
-- This is the key Haskell pattern for "mutation". Instead of modifying
-- the buffer in place, we return a new buffer alongside the value.
--
-- In Rust:  buf.read_bits(8)?     returns u64, mutates buf
-- In Haskell: readBits 8 buf      returns (Word64, BitBuffer)
--
-- 'Either String a' is like Result<T, String> in Rust:
--   Left "error msg"  = Err("error msg")
--   Right value        = Ok(value)
data ReadResult a = ReadResult
  { readValue  :: !a
  , readBuffer :: !BitBuffer
  }
  deriving (Show)

-- ────────────────────────────────────────────────────────────────────
-- Construction
-- ────────────────────────────────────────────────────────────────────

-- | Create an empty buffer. Like BitBuffer::new().
--
-- Note: no 'mut', no allocation. It's just a value.
empty :: BitBuffer
empty = BitBuffer
  { bufferBytes = []    -- Empty list. Lists in Haskell are linked lists.
  , bitPos      = 0     -- We'll use a list for simplicity; optimize later.
  , readPos     = 0
  }

-- | Create a buffer from existing bytes. Like BitBuffer::from_bytes().
fromBytes :: BS.ByteString -> BitBuffer
fromBytes bs = BitBuffer
  { bufferBytes = BS.unpack bs   -- ByteString -> [Word8] (unpack to list)
  , bitPos      = 0
  , readPos     = 0
  }

-- ────────────────────────────────────────────────────────────────────
-- Writing
-- ────────────────────────────────────────────────────────────────────

-- | Write a single bit to the buffer.
--
-- This function is PURE — it takes a buffer and returns a new one.
-- The original buffer is untouched (immutability!).
--
-- Compare to Rust:
-- @
-- fn write_bit(&mut self, bit: bool) -> io::Result<()>
-- @
--
-- Haskell signature reads: "takes a Bool and a BitBuffer, returns a BitBuffer"
writeBit :: Bool -> BitBuffer -> BitBuffer
writeBit bit buf =
  let
    -- 'let ... in' is like 'let' in Rust but for binding intermediate values.
    byteIndex = bitPos buf `div` 8        -- Which byte we're writing to
    bitOffset = bitPos buf `mod` 8        -- Which bit within that byte

    -- Ensure buffer is large enough (grow if needed)
    -- This is where immutability shows: we build a new list, not mutate.
    currentBytes = padToLength (byteIndex + 1) (bufferBytes buf)

    -- Get the current byte at this position
    currentByte = currentBytes !! byteIndex
      -- '!!' is list index. Like array[i] in Rust.
      -- Yes, it's O(n) on linked lists. We'll optimize later.

    -- Set or clear the bit. Bits are MSB-first (bit 7 = leftmost).
    -- This matches your Rust: self.buffer[byte_pos] |= 1 << (7 - bit_offset)
    newByte = if bit
              then setBit   currentByte (7 - bitOffset)
              else clearBit currentByte (7 - bitOffset)

    -- Replace the byte at byteIndex with newByte
    newBytes = replaceAt byteIndex newByte currentBytes
  in
    -- Return a NEW buffer with updated bytes and position.
    -- The old 'buf' is unchanged. This is the core Haskell idea.
    buf { bufferBytes = newBytes
        , bitPos      = bitPos buf + 1
        }

-- | Write multiple bits from a Word64 value.
--
-- This is equivalent to your Rust write_bits(value: u64, bits: usize).
--
-- We write MSB-first (most significant bit first), matching your Rust impl.
writeBits :: Word64 -> Int -> BitBuffer -> BitBuffer
writeBits _value 0 buf = buf    -- Base case: 0 bits = do nothing
writeBits value numBits buf
  -- Guards (|) are like match arms with conditions in Rust.
  -- 'error' is like panic!() — unrecoverable.
  | numBits < 0  = error "writeBits: negative bit count"
  | numBits > 64 = error "writeBits: exceeds 64 bits"
  | otherwise     =
      -- Write bits one at a time, MSB first.
      -- 'foldl' is like .fold() in Rust iterators.
      -- It processes a list left-to-right, threading an accumulator.
      --
      -- foldl f init [a, b, c] = f (f (f init a) b) c
      --
      -- Here: for each bit index from (numBits-1) down to 0,
      -- write that bit to the buffer.
      foldl writeSingleBit buf [numBits - 1, numBits - 2 .. 0]
  where
    -- 'where' is like 'let' but at the bottom. Haskell style preference.
    -- It defines helper functions scoped to this function.
    writeSingleBit :: BitBuffer -> Int -> BitBuffer
    writeSingleBit b i =
      let bitValue = testBit value i   -- Is bit at position i set?
      in writeBit bitValue b

-- ────────────────────────────────────────────────────────────────────
-- Reading
-- ────────────────────────────────────────────────────────────────────

-- | Read a single bit from the buffer.
--
-- Returns Either (like Result in Rust):
--   Left "error"           = Err (buffer underflow)
--   Right (ReadResult bool buf') = Ok (the bit, and the advanced buffer)
readBit :: BitBuffer -> Either String (ReadResult Bool)
readBit buf =
  let
    byteIndex = readPos buf `div` 8
    bitOffset = readPos buf `mod` 8
    bytes     = bufferBytes buf
  in
    -- Check bounds (like your Rust: if byte_pos >= self.buffer.len())
    if byteIndex >= length bytes
      then Left "readBit: buffer underflow"
      else
        let
          byte = bytes !! byteIndex
          bit  = testBit byte (7 - bitOffset)  -- MSB-first
        in
          Right $ ReadResult
            { readValue  = bit
            , readBuffer = buf { readPos = readPos buf + 1 }
            }

-- | Read multiple bits into a Word64.
--
-- Equivalent to your Rust read_bits(bits: usize) -> io::Result<u64>.
readBits :: Int -> BitBuffer -> Either String (ReadResult Word64)
readBits 0 buf = Right $ ReadResult { readValue = 0, readBuffer = buf }
readBits numBits buf
  | numBits < 0  = Left "readBits: negative bit count"
  | numBits > 64 = Left "readBits: exceeds 64 bits"
  | otherwise     = readBitsLoop numBits 0 buf
  where
    -- Recursive helper. Haskell doesn't have for-loops — we use recursion.
    -- GHC optimizes tail-recursive loops into actual machine loops.
    --
    -- This is equivalent to:
    --   let mut value = 0u64;
    --   for _ in 0..numBits {
    --       let bit = self.read_bit()?;
    --       value = (value << 1) | (bit as u64);
    --   }
    readBitsLoop :: Int -> Word64 -> BitBuffer -> Either String (ReadResult Word64)
    readBitsLoop 0 accum b = Right $ ReadResult { readValue = accum, readBuffer = b }
    readBitsLoop n accum b =
      -- Pattern match on the result of readBit.
      -- 'case ... of' is like 'match' in Rust.
      case readBit b of
        Left err -> Left err              -- Propagate error (like ? in Rust)
        Right (ReadResult bitVal b') ->
          let accum' = (accum `shiftL` 1)
                       .|. (if bitVal then 1 else 0)
          in readBitsLoop (n - 1) accum' b'

-- ────────────────────────────────────────────────────────────────────
-- Conversion
-- ────────────────────────────────────────────────────────────────────

-- | Convert buffer to a ByteString, padding to byte boundary.
-- Like into_bytes(pad_to_byte: true).
toBytes :: BitBuffer -> BS.ByteString
toBytes buf =
  let
    -- Pad the internal byte list to cover all written bits
    totalBytes = (bitPos buf + 7) `div` 8
    padded     = padToLength totalBytes (bufferBytes buf)
  in
    BS.pack padded   -- [Word8] -> ByteString

-- | Current write position in bits.
bitPosition :: BitBuffer -> Int
bitPosition = bitPos
  -- This is "point-free style" — we're saying bitPosition IS bitPos.
  -- Equivalent to: bitPosition buf = bitPos buf

-- | Current read position in bits.
readPosition :: BitBuffer -> Int
readPosition = readPos

-- ────────────────────────────────────────────────────────────────────
-- Internal helpers (not exported — like pub(crate) in Rust)
-- ────────────────────────────────────────────────────────────────────

-- | Ensure a list has at least 'n' elements, padding with 0.
-- Like Vec::resize() in Rust.
padToLength :: Int -> [Word8] -> [Word8]
padToLength n xs
  | length xs >= n = xs
  | otherwise      = xs ++ replicate (n - length xs) 0
    -- 'replicate n x' creates a list of n copies of x. Like vec![0; n].
    -- '++' concatenates lists. Like .extend() in Rust.

-- | Replace element at index. Returns a new list.
-- In Rust you'd just do array[i] = val. In Haskell, immutability means
-- we build a new list. (This is O(n) — we'll optimize with Vector later.)
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs =
  -- splitAt splits a list: splitAt 2 [a,b,c,d] = ([a,b], [c,d])
  -- We pattern match on the second half to drop the old element.
  -- '_' means "ignore this value" — same as Rust.
  case splitAt idx xs of
    (before, _:after) -> before ++ [val] ++ after
    (before, [])      -> before  -- Index out of bounds; return unchanged
