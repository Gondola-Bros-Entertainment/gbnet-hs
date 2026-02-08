-- |
-- Module      : GBNet.Replication.Interest
-- Description : Interest management for area-of-interest filtering
--
-- Determines which entities are relevant to a given observer, reducing
-- bandwidth by only replicating nearby or important entities.
--
-- Use 'RadiusInterest' for sphere-based filtering or 'GridInterest' for
-- cell-based spatial partitioning.
module GBNet.Replication.Interest
  ( -- * Position type
    Position,

    -- * Interest manager typeclass
    InterestManager (..),

    -- * Radius-based interest
    RadiusInterest,
    newRadiusInterest,
    radiusInterestRadius,

    -- * Grid-based interest
    GridInterest,
    newGridInterest,
    gridInterestCellSize,
  )
where

import Control.DeepSeq (NFData (..))

-- | 3D position as (x, y, z).
type Position = (Float, Float, Float)

-- | Typeclass for determining entity relevance to an observer.
--
-- Implement this for custom interest filtering strategies.
class InterestManager a where
  -- | Returns 'True' if the entity at @entityPos@ is relevant to the
  -- observer at @observerPos@.
  relevant :: a -> Position -> Position -> Bool

  -- | Returns a priority modifier for the entity relative to the observer.
  -- Values > 1.0 increase priority, < 1.0 decrease it.
  -- Default returns 1.0 (no modification).
  priorityMod :: a -> Position -> Position -> Float
  priorityMod _ _ _ = 1.0

-- | Radius-based interest: entities within @radius@ distance are relevant.
--
-- Also provides distance-based priority: closer entities get higher priority.
data RadiusInterest = RadiusInterest
  { riRadius :: !Float,
    riRadiusSq :: !Float
  }
  deriving (Show)

instance NFData RadiusInterest where
  rnf (RadiusInterest r rs) = rnf r `seq` rnf rs

-- | Create a radius-based interest manager.
newRadiusInterest :: Float -> RadiusInterest
newRadiusInterest radius =
  RadiusInterest
    { riRadius = radius,
      riRadiusSq = radius * radius
    }

-- | Get the radius.
radiusInterestRadius :: RadiusInterest -> Float
radiusInterestRadius = riRadius

instance InterestManager RadiusInterest where
  relevant ri (ex, ey, ez) (ox, oy, oz) =
    let dx = ex - ox
        dy = ey - oy
        dz = ez - oz
        distSq = dx * dx + dy * dy + dz * dz
     in distSq <= riRadiusSq ri

  priorityMod ri (ex, ey, ez) (ox, oy, oz) =
    let dx = ex - ox
        dy = ey - oy
        dz = ez - oz
        distSq = dx * dx + dy * dy + dz * dz
        radiusSq = riRadiusSq ri
     in if distSq >= radiusSq
          then 0.0
          else -- Linear falloff: closer entities get higher priority
            1.0 - sqrt (distSq / radiusSq)

-- | Grid-based area-of-interest: entities in the same or neighboring cells
-- are considered relevant.
--
-- More efficient than radius checks for large numbers of entities when
-- combined with spatial hashing.
data GridInterest = GridInterest
  { giCellSize :: !Float,
    giInvCellSize :: !Float
  }
  deriving (Show)

instance NFData GridInterest where
  rnf (GridInterest c i) = rnf c `seq` rnf i

-- | Create a grid-based interest manager.
newGridInterest :: Float -> GridInterest
newGridInterest cellSize =
  GridInterest
    { giCellSize = cellSize,
      giInvCellSize = 1.0 / cellSize
    }

-- | Get the cell size.
gridInterestCellSize :: GridInterest -> Float
gridInterestCellSize = giCellSize

-- | Grid cell coordinate (internal).
data GridCell = GridCell !Int !Int !Int
  deriving (Eq, Show)

-- | Convert position to grid cell.
toCell :: GridInterest -> Position -> GridCell
toCell gi (x, y, z) =
  let inv = giInvCellSize gi
   in GridCell
        (floor (x * inv))
        (floor (y * inv))
        (floor (z * inv))

instance InterestManager GridInterest where
  relevant gi entityPos observerPos =
    let GridCell ex ey ez = toCell gi entityPos
        GridCell ox oy oz = toCell gi observerPos
     in abs (ex - ox) <= 1 && abs (ey - oy) <= 1 && abs (ez - oz) <= 1
