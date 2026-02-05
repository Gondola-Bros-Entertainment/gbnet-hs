{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : GBNet.Serialize.BitBuffer
-- Description : Bit-level read/write buffer for sub-byte serialization
--
-- Pure functional bit buffer with MSB-first encoding, matching the wire
-- format of the Rust gbnet library.
module GBNet.Serialize.BitBuffer
  ( -- * Types
    BitBuffer,
    ReadResult (..),

    -- * Construction
    empty,
    fromBytes,

    -- * Writing
    writeBit,
    writeBits,

    -- * Reading
    readBit,
    readBits,

    -- * Conversion
    toBytes,
    bitPosition,
    readPosition,

    -- * Measurement
    serializedSizeBits,
    serializedSizeBytes,

    -- * Debug
    toBitString,

    -- * BitReader monad
    BitReader (..),
    readBitM,
    readBitsM,
  )
where

import Data.Bits (complement, shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word16, Word64, Word8)

bitsPerByte :: Int
bitsPerByte = 8

msbIndex :: Int
msbIndex = 7

maxBitsPerOp :: Int
maxBitsPerOp = 64

-- | Bit-level read/write buffer with independent write and read cursors.
data BitBuffer = BitBuffer
  { -- | Raw byte storage
    bufferBytes :: !BS.ByteString,
    -- | Write cursor (bits)
    bitPos :: !Int,
    -- | Read cursor (bits)
    readPos :: !Int
  }
  deriving (Show)

-- | Result of a read operation: value plus updated buffer.
data ReadResult a = ReadResult
  { -- | The deserialized value
    readValue :: !a,
    -- | Buffer with advanced read cursor
    readBuffer :: !BitBuffer
  }
  deriving (Show)

-- | Create an empty buffer.
empty :: BitBuffer
empty =
  BitBuffer
    { bufferBytes = BS.empty,
      bitPos = 0,
      readPos = 0
    }

-- | Create a buffer from existing bytes.
fromBytes :: BS.ByteString -> BitBuffer
fromBytes bs =
  BitBuffer
    { bufferBytes = bs,
      bitPos = 0,
      readPos = 0
    }
{-# INLINE fromBytes #-}

-- | Write a single bit (MSB-first within each byte).
writeBit :: Bool -> BitBuffer -> BitBuffer
writeBit bit = writeBits (if bit then 1 else 0) 1
{-# INLINE writeBit #-}

-- | Write N bits from a Word64 (MSB-first).
writeBits :: Word64 -> Int -> BitBuffer -> BitBuffer
writeBits _value 0 buf = buf
writeBits value numBits buf
  | numBits < 0 = error "writeBits: negative bit count"
  | numBits > maxBitsPerOp = error "writeBits: exceeds 64 bits"
  | bitPos buf `mod` bitsPerByte == 0 && numBits `mod` bitsPerByte == 0 =
      -- Fast path: byte-aligned write
      let numBytes = numBits `div` bitsPerByte
          extractBytes 0 _ = []
          extractBytes n v =
            let shift = (n - 1) * bitsPerByte
             in fromIntegral ((v `shiftR` shift) .&. 0xFF) : extractBytes (n - 1) v
          newBytes = BS.pack (extractBytes numBytes value)
          byteIndex = bitPos buf `div` bitsPerByte
          existing = bufferBytes buf
          before = BS.take byteIndex existing
          after = BS.drop (byteIndex + numBytes) existing
          padding = BS.replicate (max 0 (byteIndex - BS.length existing)) 0
       in buf
            { bufferBytes = before `BS.append` padding `BS.append` newBytes `BS.append` after,
              bitPos = bitPos buf + numBits
            }
  | otherwise =
      let !startBit = bitPos buf
          !endBit = startBit + numBits - 1
          !startByte = startBit `div` bitsPerByte
          !endByte = endBit `div` bitsPerByte
          !neededLen = endByte + 1
          !existing = bufferBytes buf
          !curLen = BS.length existing
          !padded =
            if neededLen > curLen
              then existing <> BS.replicate (neededLen - curLen) 0
              else existing
          !newSlice =
            BS.pack
              [ computeByte value numBits startBit i (BS.index padded i)
              | i <- [startByte .. endByte]
              ]
          !before = BS.take startByte padded
          !after = BS.drop (endByte + 1) padded
       in buf
            { bufferBytes = before <> newSlice <> after,
              bitPos = startBit + numBits
            }
{-# INLINE writeBits #-}

-- | Compute a single byte of the buffer after merging in written bits.
-- For each affected byte, extracts the relevant bits from the value via
-- pure arithmetic (mask/shift/OR) — no ByteString operations.
computeByte :: Word64 -> Int -> Int -> Int -> Word8 -> Word8
computeByte value numBits startBitPos byteIdx oldByte =
  let !writeEnd = startBitPos + numBits - 1
      !byteStart = byteIdx * bitsPerByte
      !lo = max byteStart startBitPos
      !hi = min (byteStart + msbIndex) writeEnd
      !count = hi - lo + 1
      !localLoBit = msbIndex - (hi - byteStart)
      !valueBitLo = writeEnd - hi
      !extracted =
        fromIntegral
          ( (value `shiftR` valueBitLo)
              .&. ((1 `shiftL` count) - 1)
          ) ::
          Word8
      !shifted = extracted `shiftL` localLoBit
      !mask16 = (((1 :: Word16) `shiftL` count) - 1) `shiftL` localLoBit
      !mask = fromIntegral mask16 :: Word8
   in (oldByte .&. complement mask) .|. shifted
{-# INLINE computeByte #-}

-- | Extract N bits from a range of bytes, inverse of 'computeByte'.
-- For each affected byte, extracts the relevant bits via pure arithmetic.
extractBits :: BS.ByteString -> Int -> Int -> Int -> Int -> Word64
extractBits bytes numBits startBitPos startByte endByte = go startByte 0
  where
    !readEnd = startBitPos + numBits - 1
    go !byteIdx !accum
      | byteIdx > endByte = accum
      | otherwise =
          let !byteStart = byteIdx * bitsPerByte
              !lo = max byteStart startBitPos
              !hi = min (byteStart + msbIndex) readEnd
              !count = hi - lo + 1
              -- Which bits within this byte to extract (MSB-first)
              !localLoBit = msbIndex - (hi - byteStart)
              !byte = BS.index bytes byteIdx
              !extracted =
                fromIntegral
                  ((byte `shiftR` localLoBit) .&. ((1 `shiftL` count) - 1)) ::
                  Word64
              !accum' = (accum `shiftL` count) .|. extracted
           in go (byteIdx + 1) accum'
{-# INLINE extractBits #-}

-- | Read a single bit from the buffer.
readBit :: BitBuffer -> Either String (ReadResult Bool)
readBit buf =
  let byteIndex = readPos buf `div` bitsPerByte
      bitOffset = readPos buf `mod` bitsPerByte
      bytes = bufferBytes buf
   in if byteIndex >= BS.length bytes
        then Left "readBit: buffer underflow"
        else
          let byte = BS.index bytes byteIndex
              bit = testBit byte (msbIndex - bitOffset)
           in Right $
                ReadResult
                  { readValue = bit,
                    readBuffer = buf {readPos = readPos buf + 1}
                  }
{-# INLINE readBit #-}

-- | Read N bits into a Word64.
readBits :: Int -> BitBuffer -> Either String (ReadResult Word64)
readBits 0 buf = Right $ ReadResult {readValue = 0, readBuffer = buf}
readBits numBits buf
  | numBits < 0 = Left "readBits: negative bit count"
  | numBits > maxBitsPerOp = Left "readBits: exceeds 64 bits"
  | isAligned = readAligned
  | otherwise = readUnaligned
  where
    isAligned = readPos buf `mod` bitsPerByte == 0 && numBits `mod` bitsPerByte == 0
    bytes = bufferBytes buf

    readAligned =
      let startByte = readPos buf `div` bitsPerByte
          numBytes = numBits `div` bitsPerByte
       in guardBounds (startByte + numBytes - 1) $
            let slice = BS.take numBytes (BS.drop startByte bytes)
                val = BS.foldl' (\acc w -> (acc `shiftL` bitsPerByte) .|. fromIntegral w) 0 slice
             in readResult val numBits

    readUnaligned =
      let !startBit = readPos buf
          !endBit = startBit + numBits - 1
          !startByte = startBit `div` bitsPerByte
          !endByte = endBit `div` bitsPerByte
       in guardBounds endByte $
            let !val = extractBits bytes numBits startBit startByte endByte
             in readResult val numBits

    guardBounds lastByte ok
      | lastByte >= BS.length bytes = Left "readBits: buffer underflow"
      | otherwise = ok

    readResult val n = Right $ ReadResult {readValue = val, readBuffer = buf {readPos = readPos buf + n}}

-- | Convert buffer to ByteString (only written bytes).
toBytes :: BitBuffer -> BS.ByteString
toBytes buf =
  let totalBytes = (bitPos buf + msbIndex) `div` bitsPerByte
   in BS.take totalBytes (bufferBytes buf)
{-# INLINE toBytes #-}

-- | Current write position in bits.
bitPosition :: BitBuffer -> Int
bitPosition = bitPos

-- | Current read position in bits.
readPosition :: BitBuffer -> Int
readPosition = readPos

-- | Measure serialized size in bits.
serializedSizeBits :: (BitBuffer -> BitBuffer) -> Int
serializedSizeBits serialize = bitPos (serialize empty)

-- | Measure serialized size in bytes (rounded up).
serializedSizeBytes :: (BitBuffer -> BitBuffer) -> Int
serializedSizeBytes serialize =
  let bits = serializedSizeBits serialize
   in (bits + msbIndex) `div` bitsPerByte

-- | Convert buffer to human-readable bit string for debugging.
toBitString :: BitBuffer -> String
toBitString buf
  | bitPos buf == 0 = ""
  | otherwise =
      let totalBits = bitPos buf
          totalBytes = (totalBits + msbIndex) `div` bitsPerByte
          bytes = bufferBytes buf
          showByte :: Int -> Word8 -> Int -> String
          showByte byteIdx w bitsToShow =
            [ if testBit w (msbIndex - i) then '1' else '0'
            | i <- [0 .. bitsToShow - 1]
            ]
              ++ if byteIdx < totalBytes - 1 then " " else ""
          lastByteBits = case totalBits `mod` bitsPerByte of
            0 -> bitsPerByte
            r -> r
       in concatMap
            ( \i ->
                let w = BS.index bytes i
                    bitsInThisByte =
                      if i == totalBytes - 1
                        then lastByteBits
                        else bitsPerByte
                 in showByte i w bitsInThisByte
            )
            [0 .. totalBytes - 1]

-- | Monad for threading buffer state through sequential reads.
newtype BitReader a = BitReader
  {runBitReader :: BitBuffer -> Either String (a, BitBuffer)}

instance Functor BitReader where
  fmap f (BitReader run) = BitReader $ \buf ->
    case run buf of
      Left err -> Left err
      Right (val, buf') -> Right (f val, buf')

instance Applicative BitReader where
  pure val = BitReader $ \buf -> Right (val, buf)
  (BitReader runF) <*> (BitReader runA) = BitReader $ \buf ->
    case runF buf of
      Left err -> Left err
      Right (f, buf') ->
        case runA buf' of
          Left err -> Left err
          Right (val, buf'') -> Right (f val, buf'')

instance Monad BitReader where
  (BitReader run) >>= f = BitReader $ \buf ->
    case run buf of
      Left err -> Left err
      Right (val, buf') ->
        let (BitReader run') = f val
         in run' buf'

-- | Read a single bit in the monad.
readBitM :: BitReader Bool
readBitM = BitReader $ \buf ->
  case readBit buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Read N bits as Word64 in the monad.
readBitsM :: Int -> BitReader Word64
readBitsM n = BitReader $ \buf ->
  case readBits n buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')
