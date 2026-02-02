-- |
-- Module      : GBNet.Serialize.BitBuffer
-- Description : Bit-level read/write buffer for sub-byte serialization
--
-- Haskell equivalent of @gbnet::serialize::bit_io::BitBuffer@.
-- Pure functional API — each operation returns a new buffer.
--
-- == Design Overview
--
-- Game networking demands sub-byte serialization: a boolean is 1 bit, a health
-- value might be 7 bits, and a coordinate 10 bits. Packing these tightly
-- reduces bandwidth, which directly impacts player experience.
--
-- This module provides a 'BitBuffer' that tracks independent write and read
-- cursors measured in /bits/. Internally, data is stored in a strict
-- 'ByteString' — a contiguous array of bytes — giving us O(1) indexing and
-- cache-friendly memory layout, plus direct interop with network sockets
-- (which speak 'ByteString' natively).
--
-- == MSB-First Bit Ordering
--
-- Bits within each byte are numbered from the most-significant bit (MSB)
-- downward:
--
-- @
--   Bit index within byte:   0  1  2  3  4  5  6  7
--   Corresponding power:    2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0
-- @
--
-- This matches network byte order (big-endian) and is the convention used by
-- the Rust gbnet library. The formula @7 - bitOffset@ converts our
-- logical bit index to the 'Data.Bits' bit index (which counts from LSB).
--
-- == Mapping Bit Positions to Byte + Offset
--
-- Given a bit position @p@:
--
--   * @p \`div\` 8@ gives the byte index (which byte we're in)
--   * @p \`mod\` 8@ gives the bit offset within that byte (0 = MSB, 7 = LSB)
--
-- This is identical to the Rust version's @bit_position / 8@ and
-- @bit_position % 8@.
--
-- == Pure Functional Style
--
-- Unlike the Rust version which uses @&mut self@, every operation here returns
-- a /new/ 'BitBuffer'. Haskell's persistent data structures and lazy
-- evaluation make this efficient — the compiler can often reuse unchanged
-- portions of the structure. The strict (@!@) annotations on each field
-- ensure we don't build up unevaluated thunks (deferred computations) that
-- would waste memory.
--
-- == Error Handling with 'Either'
--
-- Read operations return @Either String a@ rather than using exceptions.
-- 'Left' carries an error message (e.g., buffer underflow); 'Right' carries
-- the successful result. This is the idiomatic Haskell equivalent of Rust's
-- @Result<T, E>@ — errors are explicit in the type and must be handled by
-- the caller.

module GBNet.Serialize.BitBuffer
  ( -- * Types
    BitBuffer
  , ReadResult(..)

    -- * Construction
  , empty
  , fromBytes

    -- * Writing
  , writeBit
  , writeBits

    -- * Reading
  , readBit
  , readBits

    -- * Conversion
  , toBytes
  , bitPosition
  , readPosition
  ) where

-- 'Word64' is an unsigned 64-bit integer (like Rust's u64).
-- We use it as the intermediate type for all bit read/write operations
-- because it can hold any value up to 64 bits wide.
import Data.Word (Word64)

-- Bitwise operations: '.|.' is bitwise OR, 'shiftL' shifts left,
-- 'testBit' checks if a specific bit is set, 'setBit'/'clearBit'
-- turn individual bits on or off.
import Data.Bits ((.|.), shiftL, testBit, setBit, clearBit)

-- ByteString is a packed, immutable array of bytes — the standard Haskell
-- type for binary data. Unlike '[Word8]' (a linked list where each byte
-- costs ~5 words of heap overhead), ByteString stores bytes contiguously
-- in memory, giving us:
--   * O(1) indexing via 'BS.index' (vs O(n) for list's '!!')
--   * Cache locality — sequential bytes are adjacent in memory
--   * Direct interop with C libraries and network sockets
--   * Much lower memory overhead (1 byte per byte, not ~40 bytes per byte)
import qualified Data.ByteString as BS

-- ====================================================================
-- Constants
-- ====================================================================

-- | Number of bits in a single byte. Used throughout to convert between
-- bit positions and byte indices. Haskell doesn't have a @const@ keyword
-- like Rust — instead, any top-level binding is automatically immutable
-- (there is no @let mut@ in Haskell). GHC inlines small constants like
-- this at compile time, so there's zero runtime cost.
bitsPerByte :: Int
bitsPerByte = 8

-- | The index of the most-significant bit within a byte, using
-- Data.Bits' LSB-first numbering. We store bits MSB-first (network
-- order), but Data.Bits numbers from the LSB (bit 0 = least significant).
-- So our "bit 0" (MSB) is Data.Bits' bit 7.
msbIndex :: Int
msbIndex = 7

-- | Maximum number of bits that can be read or written in a single
-- 'writeBits' / 'readBits' call. Bounded by the width of 'Word64'.
maxBitsPerOp :: Int
maxBitsPerOp = 64

-- | Bit-level read/write buffer. Stores raw bytes with independent
-- write and read cursors, both tracking position in bits.
--
-- The '!' (bang) annotations make each field /strict/: when a 'BitBuffer'
-- is constructed, all three fields are evaluated immediately. Without these,
-- Haskell's laziness would store unevaluated thunks (closures) instead of
-- values, potentially causing space leaks in a long-running network loop.
--
-- In the Rust gbnet, this corresponds to @BitBuffer { bytes, bit_position,
-- read_position }@, except Rust is strict by default so no annotations
-- are needed.
data BitBuffer = BitBuffer
  { bufferBytes :: !BS.ByteString
    -- ^ The raw byte storage. Uses 'ByteString' for O(1) indexing and
    -- contiguous memory layout — critical for a networking library where
    -- we'll eventually hand these bytes to a socket. The Rust equivalent
    -- is @Vec<u8>@.
  , bitPos      :: !Int
    -- ^ Write cursor: the next bit position to be written. Monotonically
    -- increases as data is serialized into the buffer.
  , readPos     :: !Int
    -- ^ Read cursor: the next bit position to be read. Monotonically
    -- increases as data is deserialized from the buffer. Independent of
    -- 'bitPos' so we can write a full packet, then read it back.
  }
  deriving (Show)
  -- ^ 'deriving (Show)' auto-generates a textual representation, useful
  -- for debugging. Haskell's 'Show' is roughly analogous to Rust's
  -- @#[derive(Debug)]@.

-- | Result of a read operation: the value plus the updated buffer.
--
-- Since Haskell is pure (no mutation), a read must return both the value
-- /and/ the new buffer state (with the read cursor advanced). In Rust,
-- the buffer is mutated in-place via @&mut self@.
--
-- The 'ReadResult' type makes this explicit. It's used throughout the
-- deserialization pipeline: @'Either' 'String' ('ReadResult' a)@ means
-- "either an error message, or a value paired with the updated buffer."
data ReadResult a = ReadResult
  { readValue  :: !a
    -- ^ The deserialized value.
  , readBuffer :: !BitBuffer
    -- ^ The buffer with its read cursor advanced past the consumed bits.
  }
  deriving (Show)

-- --------------------------------------------------------------------
-- Construction
-- --------------------------------------------------------------------

-- | Create an empty buffer with no bytes and both cursors at position 0.
--
-- This is the starting point for serialization: write values into an
-- empty buffer, then call 'toBytes' to get the wire format.
empty :: BitBuffer
empty = BitBuffer
  { bufferBytes = BS.empty
    -- ^ Start with an empty 'ByteString'. Bytes are appended as needed
    -- during writes via 'BS.snoc'.
  , bitPos      = 0
  , readPos     = 0
  }

-- | Create a buffer from existing bytes (e.g., received from a socket).
--
-- The read cursor starts at 0, ready to deserialize from the beginning.
-- The write cursor also starts at 0 — if you write into a 'fromBytes'
-- buffer, you'll overwrite from the start (matching the Rust behavior).
--
-- With 'ByteString', this is trivially cheap: we just store the
-- 'ByteString' directly with no copying or conversion. Compare to the
-- old @[Word8]@ version which required 'BS.unpack' — an O(n) traversal
-- that allocated a linked-list node for every single byte.
fromBytes :: BS.ByteString -> BitBuffer
fromBytes bs = BitBuffer
  { bufferBytes = bs
  , bitPos      = 0
  , readPos     = 0
  }

-- --------------------------------------------------------------------
-- Writing
-- --------------------------------------------------------------------

-- | Write a single bit to the buffer (MSB-first within each byte).
--
-- == Algorithm
--
--  1. Compute which byte and which bit within that byte we're targeting,
--     using @div@ and @mod@ by 8.
--  2. If the buffer is too short, extend it by appending a zero byte
--     with 'BS.snoc'.
--  3. Read the current byte at that index, set or clear the target bit.
--  4. Build a new 'ByteString' with the modified byte in place.
--  5. Advance the write cursor by 1.
--
-- == Performance Note
--
-- The byte-replacement step rebuilds the 'ByteString' in O(n) time.
-- For a networking library, this is acceptable because:
--   * Packets are small (typically < 1500 bytes / MTU)
--   * The Rust version also does an indexed write, but O(1) due to
--     mutable Vec. A future Haskell optimization could use a mutable
--     'STUArray' under the hood if profiling shows this is hot.
--   * In practice, 'writeBits' (below) calls this in a tight loop,
--     and GHC often fuses the intermediate ByteStrings away.
writeBit :: Bool -> BitBuffer -> BitBuffer
writeBit bit buf =
  let
    -- Which byte does this bit land in?
    byteIndex    = bitPos buf `div` bitsPerByte
    -- Which bit within that byte? 0 = MSB, 7 = LSB.
    bitOffset    = bitPos buf `mod` bitsPerByte

    -- Ensure the buffer has enough bytes. If byteIndex is beyond the
    -- current length, append a zero byte. 'BS.snoc' appends a single
    -- byte to the end of a ByteString.
    currentBytes = if byteIndex >= BS.length (bufferBytes buf)
                   then BS.snoc (bufferBytes buf) 0
                   else bufferBytes buf

    -- Read the existing byte at the target index. 'BS.index' is O(1)
    -- on ByteString (vs O(n) for list's '!!').
    currentByte  = BS.index currentBytes byteIndex

    -- Set or clear the target bit. @msbIndex - bitOffset@ converts from
    -- our MSB-first numbering to Data.Bits' LSB-first numbering.
    -- For example, bitOffset=0 (MSB) maps to Data.Bits index 7 (msbIndex).
    newByte      = if bit
                   then setBit   currentByte (msbIndex - bitOffset)
                   else clearBit currentByte (msbIndex - bitOffset)

    -- Build a new ByteString with the modified byte. We split at the
    -- target index, drop the old byte, and concatenate with the new one.
    -- This is O(n) but packets are small.
    (before, after) = BS.splitAt byteIndex currentBytes
    newBytes        = before `BS.append` BS.cons newByte (BS.drop 1 after)
  in
    buf { bufferBytes = newBytes
        , bitPos      = bitPos buf + 1
        }

-- | Write N bits from a 'Word64' value (MSB-first).
--
-- Iterates from the highest bit down to bit 0, extracting each bit
-- with 'testBit' and writing it via 'writeBit'. The MSB-first order
-- ensures the value is stored in network byte order.
--
-- The @foldl@ (fold-left) processes bits left-to-right:
-- @[numBits-1, numBits-2, .. 0]@, accumulating the buffer state
-- through each write. This matches a @for@ loop in Rust that iterates
-- @(0..num_bits).rev()@.
writeBits :: Word64 -> Int -> BitBuffer -> BitBuffer
writeBits _value 0 buf = buf
  -- ^ Base case: writing 0 bits is a no-op. The underscore prefix on
  -- @_value@ tells GHC we intentionally ignore the value argument.
writeBits value numBits buf
  | numBits < 0  = error "writeBits: negative bit count"
  | numBits > maxBitsPerOp = error "writeBits: exceeds 64 bits"
  | otherwise     = foldl writeSingleBit buf [numBits - 1, numBits - 2 .. 0]
    -- ^ 'foldl' is Haskell's left fold — like Rust's @.fold()@ on an
    -- iterator. Starting with @buf@ as the accumulator, it applies
    -- 'writeSingleBit' for each bit index from high to low.
  where
    writeSingleBit :: BitBuffer -> Int -> BitBuffer
    writeSingleBit b i =
      let bitValue = testBit value i
        -- ^ 'testBit' checks if bit @i@ is set in @value@.
        -- e.g., @testBit 5 0 == True@ (binary 101, bit 0 is set).
      in writeBit bitValue b

-- --------------------------------------------------------------------
-- Reading
-- --------------------------------------------------------------------

-- | Read a single bit from the buffer.
--
-- Returns 'Left' with an error message if the read cursor is past the
-- end of the buffer (underflow). Otherwise returns 'Right' with the
-- bit value and the updated buffer (read cursor advanced by 1).
--
-- The 'Either' return type is Haskell's equivalent of Rust's @Result@:
--   * @Left err@ ~ @Err(err)@ — an error occurred
--   * @Right val@ ~ @Ok(val)@ — success with a value
readBit :: BitBuffer -> Either String (ReadResult Bool)
readBit buf =
  let
    byteIndex = readPos buf `div` bitsPerByte
    bitOffset = readPos buf `mod` bitsPerByte
    bytes     = bufferBytes buf
  in
    -- Bounds check: ensure we haven't read past the buffer.
    -- 'BS.length' is O(1) on ByteString (it's stored in the header).
    if byteIndex >= BS.length bytes
      then Left "readBit: buffer underflow"
      else
        let
          -- O(1) byte access via 'BS.index'.
          byte = BS.index bytes byteIndex
          -- Convert MSB-first offset to Data.Bits index.
          bit  = testBit byte (msbIndex - bitOffset)
        in
          Right $ ReadResult
            { readValue  = bit
            , readBuffer = buf { readPos = readPos buf + 1 }
              -- ^ Return a new buffer with the read cursor advanced.
              -- Record update syntax @buf { field = newVal }@ creates
              -- a copy of @buf@ with just that field changed. This is
              -- Haskell's equivalent of Rust's struct update: @BitBuffer
              -- { read_position: self.read_position + 1, ..self }@.
            }

-- | Read N bits into a 'Word64'.
--
-- Accumulates bits one at a time via 'readBit', shifting the accumulator
-- left and OR-ing in each new bit. This builds the value MSB-first,
-- matching the write order.
--
-- The recursive helper @readBitsLoop@ is a common Haskell pattern for
-- stateful iteration. Each recursive call carries the updated buffer,
-- remaining count, and accumulated value — similar to a @while@ loop
-- in Rust with mutable state.
readBits :: Int -> BitBuffer -> Either String (ReadResult Word64)
readBits 0 buf = Right $ ReadResult { readValue = 0, readBuffer = buf }
readBits numBits buf
  | numBits < 0  = Left "readBits: negative bit count"
  | numBits > maxBitsPerOp = Left "readBits: exceeds 64 bits"
  | otherwise     = readBitsLoop numBits 0 buf
  where
    readBitsLoop :: Int -> Word64 -> BitBuffer -> Either String (ReadResult Word64)
    readBitsLoop 0 accum b = Right $ ReadResult { readValue = accum, readBuffer = b }
    readBitsLoop n accum b =
      case readBit b of
        Left err -> Left err
          -- ^ Propagate errors upward. In Rust, the @?@ operator does
          -- this automatically; in Haskell, we pattern-match on 'Left'.
          -- (The 'BitReader' monad in Reader.hs automates this.)
        Right (ReadResult bitVal b') ->
          let accum' = (accum `shiftL` 1)
                       .|. (if bitVal then 1 else 0)
            -- ^ Shift the accumulator left by 1 and OR in the new bit.
            -- After reading all N bits, the first bit read is in the
            -- MSB position of the result — matching network byte order.
          in readBitsLoop (n - 1) accum' b'

-- --------------------------------------------------------------------
-- Conversion
-- --------------------------------------------------------------------

-- | Convert buffer to a 'ByteString', including only the bytes that
-- have been written to.
--
-- The formula @(bitPos + 7) \`div\` 8@ rounds up to the next byte
-- boundary. For example, if 10 bits were written, we need 2 bytes.
--
-- With 'ByteString', this is a simple 'BS.take' — an O(1) slice that
-- shares the underlying memory with the original. No copying occurs.
-- Compare to the old list version which required traversing and
-- rebuilding the list.
toBytes :: BitBuffer -> BS.ByteString
toBytes buf =
  let
    totalBytes = (bitPos buf + msbIndex) `div` bitsPerByte
  in
    BS.take totalBytes (bufferBytes buf)

-- | Current write position in bits.
--
-- Useful for verifying that a known structure serializes to the expected
-- number of bits (e.g., a packet header should always be exactly 64 bits).
bitPosition :: BitBuffer -> Int
bitPosition = bitPos
  -- ^ Point-free style: @bitPosition = bitPos@ means @bitPosition buf =
  -- bitPos buf@. The function is just a field accessor — no additional
  -- logic needed.

-- | Current read position in bits.
readPosition :: BitBuffer -> Int
readPosition = readPos
