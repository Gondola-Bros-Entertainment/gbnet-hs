{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GBNet.ZeroCopy
-- Description : Zero-copy vector mutation via ST
--
-- Internal module providing zero-overhead in-place mutation of immutable
-- vectors. Safety is enforced through two mechanisms:
--
--   1. __ST rank-2 type__: the @forall s@ prevents the mutable vector
--      from escaping the callback — compiler-enforced, not convention.
--
--   2. __Module opacity__: this module is internal. All buffer types
--      (@SequenceBuffer@, @SentPacketBuffer@, etc.) are opaque, so
--      external code cannot access the raw vectors.
--
-- The @unsafeThaw@\/@unsafeFreeze@ pair are pointer casts (zero memcpy),
-- giving C-level mutation performance with Haskell's type-level guarantees.
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

-- | Zero-copy mutation of an unboxed vector.
--
-- @unsafeThaw@\/@unsafeFreeze@ are pointer casts — zero allocation.
-- The @forall s@ prevents the mutable vector from escaping the callback.
zeroCopyMutateU ::
  forall a.
  (Unbox a) =>
  VU.Vector a ->
  (forall s. VUM.MVector s a -> ST s ()) ->
  VU.Vector a
zeroCopyMutateU vec action = runST $ do
  mv <- VU.unsafeThaw vec
  action mv
  VU.unsafeFreeze mv
{-# INLINE zeroCopyMutateU #-}

-- | Zero-copy mutation of an unboxed vector, returning a result.
zeroCopyMutateU' ::
  forall a b.
  (Unbox a) =>
  VU.Vector a ->
  (forall s. VUM.MVector s a -> ST s b) ->
  (VU.Vector a, b)
zeroCopyMutateU' vec action = runST $ do
  mv <- VU.unsafeThaw vec
  result <- action mv
  v' <- VU.unsafeFreeze mv
  return (v', result)
{-# INLINE zeroCopyMutateU' #-}

-- | Zero-copy mutation of a boxed vector.
--
-- @unsafeThaw@\/@unsafeFreeze@ are pointer casts — zero allocation.
-- The @forall s@ prevents the mutable vector from escaping the callback.
zeroCopyMutate ::
  forall a.
  V.Vector a ->
  (forall s. MV.MVector s a -> ST s ()) ->
  V.Vector a
zeroCopyMutate vec action = runST $ do
  mv <- V.unsafeThaw vec
  action mv
  V.unsafeFreeze mv
{-# INLINE zeroCopyMutate #-}

-- | Zero-copy mutation of a boxed vector, returning a result.
zeroCopyMutate' ::
  forall a b.
  V.Vector a ->
  (forall s. MV.MVector s a -> ST s b) ->
  (V.Vector a, b)
zeroCopyMutate' vec action = runST $ do
  mv <- V.unsafeThaw vec
  result <- action mv
  v' <- V.unsafeFreeze mv
  return (v', result)
{-# INLINE zeroCopyMutate' #-}
