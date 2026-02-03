{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- |
-- Module      : GBNet.Serialize.Class
-- Description : Typeclasses for bitpacked serialization
--
-- Defines 'BitSerialize' and 'BitDeserialize' typeclasses with instances
-- for all primitive types, collections, and custom bit widths.

module GBNet.Serialize.Class
  ( BitSerialize(..)
  , BitDeserialize(..)
    -- * Custom bit widths
  , BitWidth(..)
    -- * Bit width constants
  , word8BitWidth
  , word16BitWidth
  , word32BitWidth
  , word64BitWidth
  , packetTypeBitWidth
    -- * Collection constants
  , defaultMaxLength
  , lengthPrefixBitWidth
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import GHC.Float (castWord32ToFloat, castFloatToWord32,
                  castWord64ToDouble, castDoubleToWord64)

import GBNet.Serialize.BitBuffer

-- Bit width constants

word8BitWidth :: Int
word8BitWidth = 8

word16BitWidth :: Int
word16BitWidth = 16

word32BitWidth :: Int
word32BitWidth = 32

word64BitWidth :: Int
word64BitWidth = 64

-- | Bit width of PacketType tag (4 bits, supports 16 packet types).
packetTypeBitWidth :: Int
packetTypeBitWidth = 4

-- | Types that can be serialized into a bit buffer.
class BitSerialize a where
  bitSerialize :: a -> BitBuffer -> BitBuffer

-- | Types that can be deserialized from a bit buffer.
class BitDeserialize a where
  bitDeserialize :: BitBuffer -> Either String (ReadResult a)

-- Unsigned integers

instance BitSerialize Bool where
  bitSerialize = writeBit

instance BitDeserialize Bool where
  bitDeserialize = readBit

instance BitSerialize Word8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize Word8 where
  bitDeserialize buf =
    case readBits word8BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

instance BitSerialize Word16 where
  bitSerialize val = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize Word16 where
  bitDeserialize buf =
    case readBits word16BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

instance BitSerialize Word32 where
  bitSerialize val = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize Word32 where
  bitDeserialize buf =
    case readBits word32BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult { readValue = fromIntegral val, readBuffer = buf' }

instance BitSerialize Word64 where
  bitSerialize val = writeBits val word64BitWidth

instance BitDeserialize Word64 where
  bitDeserialize = readBits word64BitWidth

-- Signed integers (two's complement)

instance BitSerialize Int8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize Int8 where
  bitDeserialize buf =
    case readBits word8BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = fromIntegral (fromIntegral val :: Word8)
          , readBuffer = buf'
          }

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

-- Floating-point (IEEE 754 bit cast)

instance BitSerialize Float where
  bitSerialize val = writeBits (fromIntegral (castFloatToWord32 val)) word32BitWidth

instance BitDeserialize Float where
  bitDeserialize buf =
    case readBits word32BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = castWord32ToFloat (fromIntegral val)
          , readBuffer = buf'
          }

instance BitSerialize Double where
  bitSerialize val = writeBits (castDoubleToWord64 val) word64BitWidth

instance BitDeserialize Double where
  bitDeserialize buf =
    case readBits word64BitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        Right $ ReadResult
          { readValue  = castWord64ToDouble val
          , readBuffer = buf'
          }

-- Collection constants

-- | Maximum length for lists/strings (65535).
defaultMaxLength :: Int
defaultMaxLength = 65535

-- | Bit width of length prefix (16 bits).
lengthPrefixBitWidth :: Int
lengthPrefixBitWidth = 16

-- Maybe

-- | 1-bit presence flag + payload.
instance (BitSerialize a) => BitSerialize (Maybe a) where
  bitSerialize Nothing  = writeBit False
  bitSerialize (Just x) = bitSerialize x . writeBit True

instance (BitDeserialize a) => BitDeserialize (Maybe a) where
  bitDeserialize buf =
    case readBit buf of
      Left err -> Left err
      Right (ReadResult present buf') ->
        if present
          then case bitDeserialize buf' of
            Left err -> Left err
            Right (ReadResult val buf'') ->
              Right $ ReadResult { readValue = Just val, readBuffer = buf'' }
          else Right $ ReadResult { readValue = Nothing, readBuffer = buf' }

-- List

-- | 16-bit length prefix + elements.
instance (BitSerialize a) => BitSerialize [a] where
  bitSerialize xs buf =
    let len = min (length xs) defaultMaxLength
        buf' = writeBits (fromIntegral len) lengthPrefixBitWidth buf
    in foldl (flip bitSerialize) buf' xs

instance (BitDeserialize a) => BitDeserialize [a] where
  bitDeserialize buf =
    case readBits lengthPrefixBitWidth buf of
      Left err -> Left err
      Right (ReadResult lenW64 buf') ->
        let len = fromIntegral lenW64 :: Int
        in if len > defaultMaxLength
           then Left $ "list deserialize: length " ++ show len ++ " exceeds max " ++ show defaultMaxLength
           else readNElements len [] buf'
    where
      readNElements 0 acc b =
        Right $ ReadResult { readValue = reverse acc, readBuffer = b }
      readNElements n acc b =
        case bitDeserialize b of
          Left err -> Left err
          Right (ReadResult val b') -> readNElements (n - 1) (val : acc) b'

-- Text

-- | UTF-8 encoded with 16-bit byte length prefix.
instance BitSerialize T.Text where
  bitSerialize t buf =
    let bytes = TE.encodeUtf8 t
        len   = min (BS.length bytes) defaultMaxLength
        buf'  = writeBits (fromIntegral len) lengthPrefixBitWidth buf
    in BS.foldl' (flip bitSerialize) buf' (BS.take len bytes)

instance BitDeserialize T.Text where
  bitDeserialize buf =
    case readBits lengthPrefixBitWidth buf of
      Left err -> Left err
      Right (ReadResult lenW64 buf') ->
        let len = fromIntegral lenW64 :: Int
        in if len > defaultMaxLength
           then Left $ "Text deserialize: length " ++ show len ++ " exceeds max " ++ show defaultMaxLength
           else readNBytes len [] buf'
    where
      readNBytes :: Int -> [Word8] -> BitBuffer -> Either String (ReadResult T.Text)
      readNBytes 0 acc b =
        case TE.decodeUtf8' (BS.pack (reverse acc)) of
          Left err -> Left $ "Text: invalid UTF-8: " ++ show err
          Right t  -> Right $ ReadResult { readValue = t, readBuffer = b }
      readNBytes n acc b =
        case readBits word8BitWidth b of
          Left err -> Left err
          Right (ReadResult val b') ->
            readNBytes (n - 1) (fromIntegral val : acc) b'

-- Tuples

instance (BitSerialize a, BitSerialize b) => BitSerialize (a, b) where
  bitSerialize (a, b) = bitSerialize b . bitSerialize a

instance (BitDeserialize a, BitDeserialize b) => BitDeserialize (a, b) where
  bitDeserialize buf =
    case bitDeserialize buf of
      Left err -> Left err
      Right (ReadResult a buf') ->
        case bitDeserialize buf' of
          Left err -> Left err
          Right (ReadResult b buf'') ->
            Right $ ReadResult { readValue = (a, b), readBuffer = buf'' }

instance (BitSerialize a, BitSerialize b, BitSerialize c) =>
         BitSerialize (a, b, c) where
  bitSerialize (a, b, c) = bitSerialize c . bitSerialize b . bitSerialize a

instance (BitDeserialize a, BitDeserialize b, BitDeserialize c) =>
         BitDeserialize (a, b, c) where
  bitDeserialize buf =
    case bitDeserialize buf of
      Left err -> Left err
      Right (ReadResult a buf') ->
        case bitDeserialize buf' of
          Left err -> Left err
          Right (ReadResult b buf'') ->
            case bitDeserialize buf'' of
              Left err -> Left err
              Right (ReadResult c buf''') ->
                Right $ ReadResult { readValue = (a, b, c), readBuffer = buf''' }

instance (BitSerialize a, BitSerialize b, BitSerialize c, BitSerialize d) =>
         BitSerialize (a, b, c, d) where
  bitSerialize (a, b, c, d) =
    bitSerialize d . bitSerialize c . bitSerialize b . bitSerialize a

instance (BitDeserialize a, BitDeserialize b, BitDeserialize c, BitDeserialize d) =>
         BitDeserialize (a, b, c, d) where
  bitDeserialize buf =
    case bitDeserialize buf of
      Left err -> Left err
      Right (ReadResult a buf1) ->
        case bitDeserialize buf1 of
          Left err -> Left err
          Right (ReadResult b buf2) ->
            case bitDeserialize buf2 of
              Left err -> Left err
              Right (ReadResult c buf3) ->
                case bitDeserialize buf3 of
                  Left err -> Left err
                  Right (ReadResult d buf4) ->
                    Right $ ReadResult { readValue = (a, b, c, d), readBuffer = buf4 }

-- BitWidth: custom bit-width serialization

-- | Newtype tagging a value with a type-level bit width.
newtype BitWidth (n :: Nat) a = BitWidth { unBitWidth :: a }
  deriving (Eq, Show)

instance (KnownNat n, Integral a) => BitSerialize (BitWidth n a) where
  bitSerialize (BitWidth val) =
    let n = fromIntegral (natVal (Proxy :: Proxy n))
    in writeBits (fromIntegral val) n

instance (KnownNat n, Integral a) => BitDeserialize (BitWidth n a) where
  bitDeserialize buf =
    let n = fromIntegral (natVal (Proxy :: Proxy n))
    in case readBits n buf of
         Left err -> Left err
         Right (ReadResult val buf') ->
           Right $ ReadResult { readValue = BitWidth (fromIntegral val), readBuffer = buf' }
