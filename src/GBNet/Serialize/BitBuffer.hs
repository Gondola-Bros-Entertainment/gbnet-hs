-- |
-- Module      : GBNet.Serialize.BitBuffer
-- Description : Bit-level read/write buffer for sub-byte serialization
--
-- Haskell equivalent of @gbnet::serialize::bit_io::BitBuffer@.
-- Pure functional API — each operation returns a new buffer.

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

import Data.Word (Word8, Word64)
import Data.Bits ((.|.), shiftL, testBit, setBit, clearBit)
import qualified Data.ByteString as BS

-- | Bit-level read/write buffer. Stores raw bytes with independent
-- write and read cursors, both tracking position in bits.
data BitBuffer = BitBuffer
  { bufferBytes :: ![Word8]
  , bitPos      :: !Int
  , readPos     :: !Int
  }
  deriving (Show)

-- | Result of a read operation: the value plus the updated buffer.
data ReadResult a = ReadResult
  { readValue  :: !a
  , readBuffer :: !BitBuffer
  }
  deriving (Show)

-- --------------------------------------------------------------------
-- Construction
-- --------------------------------------------------------------------

-- | Create an empty buffer.
empty :: BitBuffer
empty = BitBuffer
  { bufferBytes = []
  , bitPos      = 0
  , readPos     = 0
  }

-- | Create a buffer from existing bytes.
fromBytes :: BS.ByteString -> BitBuffer
fromBytes bs = BitBuffer
  { bufferBytes = BS.unpack bs
  , bitPos      = 0
  , readPos     = 0
  }

-- --------------------------------------------------------------------
-- Writing
-- --------------------------------------------------------------------

-- | Write a single bit to the buffer (MSB-first within each byte).
writeBit :: Bool -> BitBuffer -> BitBuffer
writeBit bit buf =
  let
    byteIndex    = bitPos buf `div` 8
    bitOffset    = bitPos buf `mod` 8
    currentBytes = padToLength (byteIndex + 1) (bufferBytes buf)
    currentByte  = currentBytes !! byteIndex
    newByte      = if bit
                   then setBit   currentByte (7 - bitOffset)
                   else clearBit currentByte (7 - bitOffset)
    newBytes     = replaceAt byteIndex newByte currentBytes
  in
    buf { bufferBytes = newBytes
        , bitPos      = bitPos buf + 1
        }

-- | Write N bits from a Word64 value (MSB-first).
writeBits :: Word64 -> Int -> BitBuffer -> BitBuffer
writeBits _value 0 buf = buf
writeBits value numBits buf
  | numBits < 0  = error "writeBits: negative bit count"
  | numBits > 64 = error "writeBits: exceeds 64 bits"
  | otherwise     = foldl writeSingleBit buf [numBits - 1, numBits - 2 .. 0]
  where
    writeSingleBit :: BitBuffer -> Int -> BitBuffer
    writeSingleBit b i =
      let bitValue = testBit value i
      in writeBit bitValue b

-- --------------------------------------------------------------------
-- Reading
-- --------------------------------------------------------------------

-- | Read a single bit from the buffer.
readBit :: BitBuffer -> Either String (ReadResult Bool)
readBit buf =
  let
    byteIndex = readPos buf `div` 8
    bitOffset = readPos buf `mod` 8
    bytes     = bufferBytes buf
  in
    if byteIndex >= length bytes
      then Left "readBit: buffer underflow"
      else
        let
          byte = bytes !! byteIndex
          bit  = testBit byte (7 - bitOffset)
        in
          Right $ ReadResult
            { readValue  = bit
            , readBuffer = buf { readPos = readPos buf + 1 }
            }

-- | Read N bits into a Word64.
readBits :: Int -> BitBuffer -> Either String (ReadResult Word64)
readBits 0 buf = Right $ ReadResult { readValue = 0, readBuffer = buf }
readBits numBits buf
  | numBits < 0  = Left "readBits: negative bit count"
  | numBits > 64 = Left "readBits: exceeds 64 bits"
  | otherwise     = readBitsLoop numBits 0 buf
  where
    readBitsLoop :: Int -> Word64 -> BitBuffer -> Either String (ReadResult Word64)
    readBitsLoop 0 accum b = Right $ ReadResult { readValue = accum, readBuffer = b }
    readBitsLoop n accum b =
      case readBit b of
        Left err -> Left err
        Right (ReadResult bitVal b') ->
          let accum' = (accum `shiftL` 1)
                       .|. (if bitVal then 1 else 0)
          in readBitsLoop (n - 1) accum' b'

-- --------------------------------------------------------------------
-- Conversion
-- --------------------------------------------------------------------

-- | Convert buffer to a ByteString, padding to byte boundary.
toBytes :: BitBuffer -> BS.ByteString
toBytes buf =
  let
    totalBytes = (bitPos buf + 7) `div` 8
    padded     = padToLength totalBytes (bufferBytes buf)
  in
    BS.pack padded

-- | Current write position in bits.
bitPosition :: BitBuffer -> Int
bitPosition = bitPos

-- | Current read position in bits.
readPosition :: BitBuffer -> Int
readPosition = readPos

-- --------------------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------------------

-- | Ensure a list has at least @n@ elements, padding with 0.
padToLength :: Int -> [Word8] -> [Word8]
padToLength n xs
  | length xs >= n = xs
  | otherwise      = xs ++ replicate (n - length xs) 0

-- | Replace element at index. Returns a new list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs =
  case splitAt idx xs of
    (before, _:after) -> before ++ [val] ++ after
    (before, [])      -> before
