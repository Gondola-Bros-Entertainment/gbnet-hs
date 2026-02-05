{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GBNet.Serialize.FastSupport
-- Description : Zero-allocation serialization for Storable types
--
-- Provides fast serialization for any type with a 'Storable' instance.
-- Use 'deriveStorable' from "GBNet.Serialize.FastTH" to generate instances,
-- then use 'serialize' and 'deserialize' for zero-copy conversion.
--
-- @
-- data Vec3 = Vec3 !Float !Float !Float
-- deriveStorable ''Vec3
--
-- -- Pure, zero-allocation serialization:
-- let bytes = serialize (Vec3 1.0 2.0 3.0)
-- let Right v = deserialize bytes :: Either String Vec3
-- @
module GBNet.Serialize.FastSupport
  ( -- * Serialization (pure, zero-copy)
    serialize,
    deserialize,

    -- * Re-exports for deriveStorable
    ByteString,
    Storable (..),
    plusPtr,
    castPtr,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr, unsafeCreate)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafePerformIO)

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
  | otherwise = unsafePerformIO $ do
      let (fptr, off, _) = toForeignPtr bs
      val <- withForeignPtr fptr $ \ptr ->
        peek (castPtr (ptr `plusPtr` off))
      return $ Right val
  where
    needed = sizeOf (undefined :: a)
{-# INLINE deserialize #-}

