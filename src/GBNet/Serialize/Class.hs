{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GBNet.Serialize.Class
-- Description : Typeclasses for bitpacked serialization
--
-- Defines 'BitSerialize' and 'BitDeserialize' typeclasses with instances
-- for all primitive types, collections, and custom bit widths.
module GBNet.Serialize.Class
  ( BitSerialize (..),
    BitDeserialize (..),

    -- * Monadic deserialization
    deserializeM,
    runDeserialize,

    -- * Custom bit widths
    BitWidth (..),

    -- * Bit width constants
    word8BitWidth,
    word16BitWidth,
    word32BitWidth,
    word64BitWidth,
    packetTypeBitWidth,

    -- * Collection constants
    defaultMaxLength,
    lengthPrefixBitWidth,
  )
where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Serialize.BitBuffer
import GHC.Float
  ( castDoubleToWord64,
    castFloatToWord32,
    castWord32ToFloat,
    castWord64ToDouble,
  )
import GHC.TypeLits (KnownNat, Nat, natVal)

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

-- | Deserialize in the BitReader monad.
deserializeM :: (BitDeserialize a) => BitReader a
deserializeM = BitReader $ \buf ->
  case bitDeserialize buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Run BitReader and wrap result in ReadResult.
runDeserialize :: BitReader a -> BitBuffer -> Either String (ReadResult a)
runDeserialize reader buf =
  case runBitReader reader buf of
    Left err -> Left err
    Right (val, buf') -> Right $ ReadResult val buf'

-- Unsigned integers

instance BitSerialize Bool where
  bitSerialize = writeBit

instance BitDeserialize Bool where
  bitDeserialize = readBit

instance BitSerialize Word8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize Word8 where
  bitDeserialize = runDeserialize $ fromIntegral <$> readBitsM word8BitWidth

instance BitSerialize Word16 where
  bitSerialize val = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize Word16 where
  bitDeserialize = runDeserialize $ fromIntegral <$> readBitsM word16BitWidth

instance BitSerialize Word32 where
  bitSerialize val = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize Word32 where
  bitDeserialize = runDeserialize $ fromIntegral <$> readBitsM word32BitWidth

instance BitSerialize Word64 where
  bitSerialize val = writeBits val word64BitWidth

instance BitDeserialize Word64 where
  bitDeserialize = runDeserialize $ readBitsM word64BitWidth

-- Signed integers (two's complement)

instance BitSerialize Int8 where
  bitSerialize val = writeBits (fromIntegral val) word8BitWidth

instance BitDeserialize Int8 where
  bitDeserialize = runDeserialize $ do
    val <- readBitsM word8BitWidth
    pure $ fromIntegral (fromIntegral val :: Word8)

instance BitSerialize Int16 where
  bitSerialize val = writeBits (fromIntegral val) word16BitWidth

instance BitDeserialize Int16 where
  bitDeserialize = runDeserialize $ do
    val <- readBitsM word16BitWidth
    pure $ fromIntegral (fromIntegral val :: Word16)

instance BitSerialize Int32 where
  bitSerialize val = writeBits (fromIntegral val) word32BitWidth

instance BitDeserialize Int32 where
  bitDeserialize = runDeserialize $ do
    val <- readBitsM word32BitWidth
    pure $ fromIntegral (fromIntegral val :: Word32)

instance BitSerialize Int64 where
  bitSerialize val = writeBits (fromIntegral val) word64BitWidth

instance BitDeserialize Int64 where
  bitDeserialize = runDeserialize $ fromIntegral <$> readBitsM word64BitWidth

-- Floating-point (IEEE 754 bit cast)

instance BitSerialize Float where
  bitSerialize val = writeBits (fromIntegral (castFloatToWord32 val)) word32BitWidth

instance BitDeserialize Float where
  bitDeserialize = runDeserialize $ do
    val <- readBitsM word32BitWidth
    pure $ castWord32ToFloat (fromIntegral val)

instance BitSerialize Double where
  bitSerialize val = writeBits (castDoubleToWord64 val) word64BitWidth

instance BitDeserialize Double where
  bitDeserialize = runDeserialize $ castWord64ToDouble <$> readBitsM word64BitWidth

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
  bitSerialize Nothing = writeBit False
  bitSerialize (Just x) = bitSerialize x . writeBit True

instance (BitDeserialize a) => BitDeserialize (Maybe a) where
  bitDeserialize = runDeserialize $ do
    present <- readBitM
    if present
      then Just <$> deserializeM
      else pure Nothing

-- List

-- | 16-bit length prefix + elements.
instance (BitSerialize a) => BitSerialize [a] where
  bitSerialize xs buf =
    let len = min (length xs) defaultMaxLength
        buf' = writeBits (fromIntegral len) lengthPrefixBitWidth buf
     in foldl (flip bitSerialize) buf' xs

instance (BitDeserialize a) => BitDeserialize [a] where
  bitDeserialize = runDeserialize $ do
    lenW64 <- readBitsM lengthPrefixBitWidth
    let len = fromIntegral lenW64 :: Int
    if len > defaultMaxLength
      then BitReader $ \_ -> Left $ "list: length " ++ show len ++ " exceeds max"
      else readN len []
    where
      readN 0 acc = pure (reverse acc)
      readN n acc = do
        val <- deserializeM
        readN (n - 1) (val : acc)

-- Text

-- | UTF-8 encoded with 16-bit byte length prefix.
instance BitSerialize T.Text where
  bitSerialize t buf =
    let bytes = TE.encodeUtf8 t
        len = min (BS.length bytes) defaultMaxLength
        buf' = writeBits (fromIntegral len) lengthPrefixBitWidth buf
     in BS.foldl' (flip bitSerialize) buf' (BS.take len bytes)

instance BitDeserialize T.Text where
  bitDeserialize = runDeserialize $ do
    lenW64 <- readBitsM lengthPrefixBitWidth
    let len = fromIntegral lenW64 :: Int
    if len > defaultMaxLength
      then BitReader $ \_ -> Left $ "Text: length " ++ show len ++ " exceeds max"
      else do
        bytes <- readNBytes len []
        case TE.decodeUtf8' (BS.pack (reverse bytes)) of
          Left err -> BitReader $ \_ -> Left $ "Text: invalid UTF-8: " ++ show err
          Right t -> pure t
    where
      readNBytes 0 acc = pure acc
      readNBytes n acc = do
        val <- readBitsM word8BitWidth
        readNBytes (n - 1) (fromIntegral val : acc)

-- Tuples

instance (BitSerialize a, BitSerialize b) => BitSerialize (a, b) where
  bitSerialize (a, b) = bitSerialize b . bitSerialize a

instance (BitDeserialize a, BitDeserialize b) => BitDeserialize (a, b) where
  bitDeserialize = runDeserialize $ do
    a <- deserializeM
    b <- deserializeM
    pure (a, b)

instance
  (BitSerialize a, BitSerialize b, BitSerialize c) =>
  BitSerialize (a, b, c)
  where
  bitSerialize (a, b, c) = bitSerialize c . bitSerialize b . bitSerialize a

instance
  (BitDeserialize a, BitDeserialize b, BitDeserialize c) =>
  BitDeserialize (a, b, c)
  where
  bitDeserialize = runDeserialize $ do
    a <- deserializeM
    b <- deserializeM
    c <- deserializeM
    pure (a, b, c)

instance
  (BitSerialize a, BitSerialize b, BitSerialize c, BitSerialize d) =>
  BitSerialize (a, b, c, d)
  where
  bitSerialize (a, b, c, d) =
    bitSerialize d . bitSerialize c . bitSerialize b . bitSerialize a

instance
  (BitDeserialize a, BitDeserialize b, BitDeserialize c, BitDeserialize d) =>
  BitDeserialize (a, b, c, d)
  where
  bitDeserialize = runDeserialize $ do
    a <- deserializeM
    b <- deserializeM
    c <- deserializeM
    d <- deserializeM
    pure (a, b, c, d)

-- BitWidth: custom bit-width serialization

-- | Newtype tagging a value with a type-level bit width.
newtype BitWidth (n :: Nat) a = BitWidth {unBitWidth :: a}
  deriving (Eq, Show)

instance (KnownNat n, Integral a) => BitSerialize (BitWidth n a) where
  bitSerialize (BitWidth val) =
    let n = fromIntegral (natVal (Proxy :: Proxy n))
     in writeBits (fromIntegral val) n

instance (KnownNat n, Integral a) => BitDeserialize (BitWidth n a) where
  bitDeserialize buf =
    let n = fromIntegral (natVal (Proxy :: Proxy n))
     in runDeserialize (BitWidth . fromIntegral <$> readBitsM n) buf
