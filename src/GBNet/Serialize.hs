{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GBNet.Serialize
-- Description : Zero-allocation serialization for Storable types
--
-- Provides fast serialization for any type with a 'Storable' instance.
-- Use @deriveStorable@ from "GBNet.Serialize.TH" to generate instances,
-- then use 'serialize' and 'deserialize' for zero-copy conversion.
--
-- Wire format is always little-endian for cross-platform compatibility.
-- On LE platforms (x86, ARM), the LE helpers compile to native operations
-- with zero overhead — GHC eliminates the dead branch at compile time.
--
-- @
-- data Vec3 = Vec3 !Float !Float !Float
-- deriveStorable ''Vec3
--
-- -- Pure, zero-allocation serialization:
-- let bytes = serialize (Vec3 1.0 2.0 3.0)
-- let Right v = deserialize bytes :: Either String Vec3
-- @
module GBNet.Serialize
  ( -- * Serialization (pure, zero-copy)
    serialize,
    deserialize,

    -- * Re-exports for deriveStorable
    ByteString,
    Storable (..),
    plusPtr,
    castPtr,

    -- * Little-endian helpers (used by generated Storable instances)
    pokeWord16LE,
    pokeWord32LE,
    pokeWord64LE,
    pokeInt16LE,
    pokeInt32LE,
    pokeInt64LE,
    pokeFloatLE,
    pokeDoubleLE,
    peekWord16LE,
    peekWord32LE,
    peekWord64LE,
    peekInt16LE,
    peekInt32LE,
    peekInt64LE,
    peekFloatLE,
    peekDoubleLE,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr, unsafeCreate)
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Serialize any Storable type to a ByteString.
-- Pure interface, zero allocation beyond the result ByteString.
serialize :: (Storable a) => a -> ByteString
serialize !x = unsafeCreate (sizeOf x) $ \ptr ->
  poke (castPtr ptr) x
{-# INLINE serialize #-}

-- | Deserialize a ByteString to any Storable type.
-- Returns Left with error message if buffer is too short.
deserialize :: forall a. (Storable a) => ByteString -> Either String a
deserialize !bs
  | BS.length bs < needed = Left "deserialize: buffer too short"
  | otherwise = unsafeDupablePerformIO $ do
      let (fptr, off, _) = toForeignPtr bs
      val <- withForeignPtr fptr $ \ptr ->
        peek (castPtr (ptr `plusPtr` off))
      return $ Right val
  where
    needed = sizeOf (undefined :: a)
{-# INLINE deserialize #-}

-- ---------------------------------------------------------------------------
-- Little-endian poke helpers
-- On LE: compiles to native pokeByteOff (zero overhead).
-- On BE: byte-swaps before writing (single bswap instruction).
-- ---------------------------------------------------------------------------

-- | Write a 'Word16' in little-endian byte order.
pokeWord16LE :: Ptr a -> Int -> Word16 -> IO ()
pokeWord16LE ptr off w = case targetByteOrder of
  LittleEndian -> pokeByteOff ptr off w
  BigEndian -> pokeByteOff ptr off (byteSwap16 w)
{-# INLINE pokeWord16LE #-}

-- | Write a 'Word32' in little-endian byte order.
pokeWord32LE :: Ptr a -> Int -> Word32 -> IO ()
pokeWord32LE ptr off w = case targetByteOrder of
  LittleEndian -> pokeByteOff ptr off w
  BigEndian -> pokeByteOff ptr off (byteSwap32 w)
{-# INLINE pokeWord32LE #-}

-- | Write a 'Word64' in little-endian byte order.
pokeWord64LE :: Ptr a -> Int -> Word64 -> IO ()
pokeWord64LE ptr off w = case targetByteOrder of
  LittleEndian -> pokeByteOff ptr off w
  BigEndian -> pokeByteOff ptr off (byteSwap64 w)
{-# INLINE pokeWord64LE #-}

-- | Write an 'Int16' in little-endian byte order.
pokeInt16LE :: Ptr a -> Int -> Int16 -> IO ()
pokeInt16LE ptr off i = pokeWord16LE ptr off (fromIntegral i)
{-# INLINE pokeInt16LE #-}

-- | Write an 'Int32' in little-endian byte order.
pokeInt32LE :: Ptr a -> Int -> Int32 -> IO ()
pokeInt32LE ptr off i = pokeWord32LE ptr off (fromIntegral i)
{-# INLINE pokeInt32LE #-}

-- | Write an 'Int64' in little-endian byte order.
pokeInt64LE :: Ptr a -> Int -> Int64 -> IO ()
pokeInt64LE ptr off i = pokeWord64LE ptr off (fromIntegral i)
{-# INLINE pokeInt64LE #-}

-- | Write a 'Float' in little-endian byte order.
pokeFloatLE :: Ptr a -> Int -> Float -> IO ()
pokeFloatLE ptr off f = pokeWord32LE ptr off (castFloatToWord32 f)
{-# INLINE pokeFloatLE #-}

-- | Write a 'Double' in little-endian byte order.
pokeDoubleLE :: Ptr a -> Int -> Double -> IO ()
pokeDoubleLE ptr off d = pokeWord64LE ptr off (castDoubleToWord64 d)
{-# INLINE pokeDoubleLE #-}

-- ---------------------------------------------------------------------------
-- Little-endian peek helpers
-- ---------------------------------------------------------------------------

-- | Read a 'Word16' in little-endian byte order.
peekWord16LE :: Ptr a -> Int -> IO Word16
peekWord16LE ptr off = case targetByteOrder of
  LittleEndian -> peekByteOff ptr off
  BigEndian -> byteSwap16 <$> peekByteOff ptr off
{-# INLINE peekWord16LE #-}

-- | Read a 'Word32' in little-endian byte order.
peekWord32LE :: Ptr a -> Int -> IO Word32
peekWord32LE ptr off = case targetByteOrder of
  LittleEndian -> peekByteOff ptr off
  BigEndian -> byteSwap32 <$> peekByteOff ptr off
{-# INLINE peekWord32LE #-}

-- | Read a 'Word64' in little-endian byte order.
peekWord64LE :: Ptr a -> Int -> IO Word64
peekWord64LE ptr off = case targetByteOrder of
  LittleEndian -> peekByteOff ptr off
  BigEndian -> byteSwap64 <$> peekByteOff ptr off
{-# INLINE peekWord64LE #-}

-- | Read an 'Int16' in little-endian byte order.
peekInt16LE :: Ptr a -> Int -> IO Int16
peekInt16LE ptr off = fromIntegral <$> peekWord16LE ptr off
{-# INLINE peekInt16LE #-}

-- | Read an 'Int32' in little-endian byte order.
peekInt32LE :: Ptr a -> Int -> IO Int32
peekInt32LE ptr off = fromIntegral <$> peekWord32LE ptr off
{-# INLINE peekInt32LE #-}

-- | Read an 'Int64' in little-endian byte order.
peekInt64LE :: Ptr a -> Int -> IO Int64
peekInt64LE ptr off = fromIntegral <$> peekWord64LE ptr off
{-# INLINE peekInt64LE #-}

-- | Read a 'Float' in little-endian byte order.
peekFloatLE :: Ptr a -> Int -> IO Float
peekFloatLE ptr off = castWord32ToFloat <$> peekWord32LE ptr off
{-# INLINE peekFloatLE #-}

-- | Read a 'Double' in little-endian byte order.
peekDoubleLE :: Ptr a -> Int -> IO Double
peekDoubleLE ptr off = castWord64ToDouble <$> peekWord64LE ptr off
{-# INLINE peekDoubleLE #-}
