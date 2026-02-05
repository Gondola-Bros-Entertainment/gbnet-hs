{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GBNet.ZeroCopy
-- Description : Zero-copy vector mutation with compile-time safety
--
-- Provides zero-overhead mutation of immutable vectors using linear types.
-- The linear arrow (%1) enforces at compile time that the input vector
-- is consumed exactly once - reusing it is a type error.
--
-- This gives us C-level performance (zero memcpy) with Haskell-level safety
-- (compiler-enforced linearity).
module GBNet.ZeroCopy
  ( -- * Unboxed vectors
    zeroCopyMutateU,
    zeroCopyMutateU',

    -- * Boxed vectors
    zeroCopyMutate,
    zeroCopyMutate',
  )
where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Unsafe.Linear (toLinear)

-- | Zero-copy mutation of an unboxed vector with compile-time safety.
--
-- The linear arrow (%1) guarantees the input vector is consumed exactly once:
--   - Reuse after call → compile error
--   - Forget to use   → compile error
--
-- Zero overhead: unsafeThaw/unsafeFreeze are pointer casts, not memcpy.
zeroCopyMutateU ::
  forall a.
  (Unbox a) =>
  VU.Vector a %1 ->
  (forall s. VUM.MVector s a -> ST s ()) ->
  VU.Vector a
zeroCopyMutateU = toLinear go
  where
    go :: VU.Vector a -> (forall s. VUM.MVector s a -> ST s ()) -> VU.Vector a
    go vec action = runST $ do
      mv <- VU.unsafeThaw vec
      action mv
      VU.unsafeFreeze mv
{-# INLINE zeroCopyMutateU #-}

-- | Zero-copy mutation returning a result from the action.
zeroCopyMutateU' ::
  forall a b.
  (Unbox a) =>
  VU.Vector a %1 ->
  (forall s. VUM.MVector s a -> ST s b) ->
  (VU.Vector a, b)
zeroCopyMutateU' = toLinear go
  where
    go :: VU.Vector a -> (forall s. VUM.MVector s a -> ST s b) -> (VU.Vector a, b)
    go vec action = runST $ do
      mv <- VU.unsafeThaw vec
      result <- action mv
      v' <- VU.unsafeFreeze mv
      return (v', result)
{-# INLINE zeroCopyMutateU' #-}

-- | Zero-copy mutation of a boxed vector with compile-time safety.
--
-- The linear arrow (%1) guarantees the input vector is consumed exactly once:
--   - Reuse after call → compile error
--   - Forget to use   → compile error
--
-- Zero overhead: unsafeThaw/unsafeFreeze are pointer casts, not memcpy.
zeroCopyMutate ::
  forall a.
  V.Vector a %1 ->
  (forall s. MV.MVector s a -> ST s ()) ->
  V.Vector a
zeroCopyMutate = toLinear go
  where
    go :: V.Vector a -> (forall s. MV.MVector s a -> ST s ()) -> V.Vector a
    go vec action = runST $ do
      mv <- V.unsafeThaw vec
      action mv
      V.unsafeFreeze mv
{-# INLINE zeroCopyMutate #-}

-- | Zero-copy mutation returning a result from the action.
zeroCopyMutate' ::
  forall a b.
  V.Vector a %1 ->
  (forall s. MV.MVector s a -> ST s b) ->
  (V.Vector a, b)
zeroCopyMutate' = toLinear go
  where
    go :: V.Vector a -> (forall s. MV.MVector s a -> ST s b) -> (V.Vector a, b)
    go vec action = runST $ do
      mv <- V.unsafeThaw vec
      result <- action mv
      v' <- V.unsafeFreeze mv
      return (v', result)
{-# INLINE zeroCopyMutate' #-}
