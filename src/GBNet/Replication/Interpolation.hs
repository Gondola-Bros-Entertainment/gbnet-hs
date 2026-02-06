{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Replication.Interpolation
-- Description : Snapshot interpolation for smooth client-side rendering
--
-- 'SnapshotBuffer' stores timestamped snapshots and interpolates between
-- them at a configurable playback delay, enabling smooth rendering despite
-- network jitter.
--
-- Example usage:
--
-- @
-- -- On receiving server snapshot
-- let buffer' = pushSnapshot serverTimestamp playerState buffer
--
-- -- On render tick
-- case sampleSnapshot renderTime buffer' of
--   Nothing -> renderLastKnown
--   Just interpolated -> render interpolated
-- @
module GBNet.Replication.Interpolation
  ( -- * Constants
    defaultBufferDepth,
    defaultPlaybackDelayMs,

    -- * Interpolatable typeclass
    Interpolatable (..),

    -- * Snapshot buffer
    SnapshotBuffer,
    newSnapshotBuffer,
    newSnapshotBufferWithConfig,

    -- * Operations
    pushSnapshot,
    sampleSnapshot,
    snapshotReset,

    -- * Queries
    snapshotCount,
    snapshotIsEmpty,
    snapshotReady,
    snapshotPlaybackDelayMs,

    -- * Configuration
    setPlaybackDelayMs,
  )
where

import Data.Sequence (Seq, ViewL (..), ViewR (..), (|>))
import qualified Data.Sequence as Seq
import Optics ((&), (.~))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Default number of snapshots to buffer before interpolation begins.
defaultBufferDepth :: Int
defaultBufferDepth = 3

-- | Default playback delay in milliseconds behind the latest received snapshot.
defaultPlaybackDelayMs :: Double
defaultPlaybackDelayMs = 100.0

-- | Typeclass for types that support linear interpolation between two states.
--
-- Implement this for any game state that needs smooth interpolation:
--
-- @
-- instance Interpolatable Vec3 where
--   lerp (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) t =
--     Vec3 (x1 + (x2 - x1) * t)
--          (y1 + (y2 - y1) * t)
--          (z1 + (z2 - z1) * t)
-- @
class Interpolatable a where
  -- | Linearly interpolate between @self@ and @other@ by factor @t@ in [0, 1].
  lerp :: a -> a -> Float -> a

-- | A timestamped snapshot for interpolation (internal).
data TimestampedSnapshot a = TimestampedSnapshot
  { tsTimestamp :: !Double,
    tsState :: !a
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''TimestampedSnapshot

-- | Ring buffer of timestamped snapshots with interpolation sampling.
data SnapshotBuffer a = SnapshotBuffer
  { sbSnapshots :: !(Seq (TimestampedSnapshot a)),
    sbBufferDepth :: !Int,
    sbPlaybackDelayMs :: !Double
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''SnapshotBuffer

-- | Create a snapshot buffer with default settings.
newSnapshotBuffer :: SnapshotBuffer a
newSnapshotBuffer =
  SnapshotBuffer
    { sbSnapshots = Seq.empty,
      sbBufferDepth = defaultBufferDepth,
      sbPlaybackDelayMs = defaultPlaybackDelayMs
    }

-- | Create a snapshot buffer with custom settings.
newSnapshotBufferWithConfig ::
  -- | Buffer depth (number of snapshots before ready)
  Int ->
  -- | Playback delay in milliseconds
  Double ->
  SnapshotBuffer a
newSnapshotBufferWithConfig depth delay =
  SnapshotBuffer
    { sbSnapshots = Seq.empty,
      sbBufferDepth = depth,
      sbPlaybackDelayMs = delay
    }

-- | Push a new snapshot with its server timestamp (in milliseconds).
--
-- Snapshots must arrive in order; out-of-order snapshots are dropped.
pushSnapshot :: Double -> a -> SnapshotBuffer a -> SnapshotBuffer a
pushSnapshot timestamp state buffer =
  let snapshots = sbSnapshots buffer
   in -- Check if newer than latest
      case Seq.viewr snapshots of
        EmptyR ->
          -- First snapshot
          buffer & #sbSnapshots .~ Seq.singleton (TimestampedSnapshot timestamp state)
        _ :> last' ->
          if timestamp <= tsTimestamp last'
            then buffer -- Drop out-of-order
            else
              let snapshots' = snapshots |> TimestampedSnapshot timestamp state
                  -- Keep buffer bounded
                  maxEntries = sbBufferDepth buffer * 2
                  trimmed =
                    if Seq.length snapshots' > maxEntries
                      then Seq.drop (Seq.length snapshots' - maxEntries) snapshots'
                      else snapshots'
               in buffer & #sbSnapshots .~ trimmed

-- | Sample an interpolated state at @renderTime@ (in milliseconds).
--
-- Returns 'Nothing' if insufficient snapshots are buffered.
sampleSnapshot :: (Interpolatable a) => Double -> SnapshotBuffer a -> Maybe a
sampleSnapshot renderTime buffer =
  let targetTime = renderTime - sbPlaybackDelayMs buffer
      snapshots = sbSnapshots buffer
   in if Seq.length snapshots < 2
        then Nothing
        else findAndInterpolate targetTime snapshots

-- | Find bracketing snapshots and interpolate.
findAndInterpolate :: (Interpolatable a) => Double -> Seq (TimestampedSnapshot a) -> Maybe a
findAndInterpolate targetTime snapshots =
  case findBracket targetTime (Seq.viewl snapshots) of
    Just (a, b) ->
      let duration = tsTimestamp b - tsTimestamp a
       in if duration <= 0.0
            then Just (tsState a)
            else
              let t = realToFrac ((targetTime - tsTimestamp a) / duration)
                  tClamped = max 0.0 (min 1.0 t)
               in Just (lerp (tsState a) (tsState b) tClamped)
    Nothing ->
      -- Target outside range - return boundary
      case (Seq.viewl snapshots, Seq.viewr snapshots) of
        (first :< _, _ :> lastSnap)
          | targetTime > tsTimestamp lastSnap -> Just (tsState lastSnap)
          | targetTime < tsTimestamp first -> Just (tsState first)
        _ -> Nothing

-- | Find two snapshots bracketing the target time.
findBracket ::
  Double ->
  ViewL (TimestampedSnapshot a) ->
  Maybe (TimestampedSnapshot a, TimestampedSnapshot a)
findBracket _ EmptyL = Nothing
findBracket _ (_ :< rest) | Seq.null rest = Nothing
findBracket targetTime (a :< rest) =
  case Seq.viewl rest of
    b :< _ ->
      if targetTime >= tsTimestamp a && targetTime <= tsTimestamp b
        then Just (a, b)
        else findBracket targetTime (Seq.viewl rest)
    EmptyL -> Nothing

-- | Number of buffered snapshots.
snapshotCount :: SnapshotBuffer a -> Int
snapshotCount = Seq.length . sbSnapshots

-- | Whether the buffer is empty.
snapshotIsEmpty :: SnapshotBuffer a -> Bool
snapshotIsEmpty = Seq.null . sbSnapshots

-- | Whether enough snapshots are buffered to begin interpolation.
snapshotReady :: SnapshotBuffer a -> Bool
snapshotReady buffer = Seq.length (sbSnapshots buffer) >= sbBufferDepth buffer

-- | Clear all buffered snapshots.
snapshotReset :: SnapshotBuffer a -> SnapshotBuffer a
snapshotReset buffer = buffer & #sbSnapshots .~ Seq.empty

-- | Get the playback delay in milliseconds.
snapshotPlaybackDelayMs :: SnapshotBuffer a -> Double
snapshotPlaybackDelayMs = sbPlaybackDelayMs

-- | Set the playback delay in milliseconds.
setPlaybackDelayMs :: Double -> SnapshotBuffer a -> SnapshotBuffer a
setPlaybackDelayMs delay buffer = buffer & #sbPlaybackDelayMs .~ delay

-- Common instances

instance Interpolatable Float where
  lerp a b t = a + (b - a) * t

instance Interpolatable Double where
  lerp a b t = a + (b - a) * realToFrac t

instance (Interpolatable a, Interpolatable b) => Interpolatable (a, b) where
  lerp (a1, b1) (a2, b2) t = (lerp a1 a2 t, lerp b1 b2 t)

instance (Interpolatable a, Interpolatable b, Interpolatable c) => Interpolatable (a, b, c) where
  lerp (a1, b1, c1) (a2, b2, c2) t = (lerp a1 a2 t, lerp b1 b2 t, lerp c1 c2 t)
