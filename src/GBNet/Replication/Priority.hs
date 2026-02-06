{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Replication.Priority
-- Description : Priority accumulator for bandwidth-limited entity replication
--
-- 'PriorityAccumulator' tracks per-entity priority that grows over time,
-- ensuring all entities eventually get sent even at low priority.
--
-- Use this to fairly allocate bandwidth when replicating many entities:
--
-- @
-- let acc = newPriorityAccumulator
--       & register playerId 10.0    -- high priority (10 units/sec)
--       & register npcId 2.0        -- low priority (2 units/sec)
--
-- -- Each tick, accumulate priority based on elapsed time
-- let acc' = accumulate 0.016 acc   -- 16ms tick
--
-- -- Drain entities that fit in budget
-- let (selected, acc'') = drainTop 1200 entitySize acc'
-- @
module GBNet.Replication.Priority
  ( -- * Priority accumulator
    PriorityAccumulator,
    newPriorityAccumulator,

    -- * Entity management
    register,
    unregister,

    -- * Priority operations
    accumulate,
    applyModifier,
    drainTop,

    -- * Queries
    priorityCount,
    priorityIsEmpty,
    getPriority,
  )
where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Optics ((&), (.~), (%~))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Per-entity priority tracking entry.
data PriorityEntry = PriorityEntry
  { peBase :: !Float,
    peAccumulated :: !Float
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''PriorityEntry

-- | Accumulates priority per entity and drains the highest-priority entities
-- that fit within a byte budget.
--
-- Type parameter @id@ is the entity identifier type (must be 'Ord').
newtype PriorityAccumulator id = PriorityAccumulator
  { paEntries :: Map id PriorityEntry
  }
  deriving (Show)

-- | Create an empty priority accumulator.
newPriorityAccumulator :: PriorityAccumulator id
newPriorityAccumulator = PriorityAccumulator Map.empty

-- | Register an entity with a base priority.
--
-- Higher base priority = more frequent sends.
-- Priority accumulates as @base * dt@ each tick.
register :: (Ord id) => id -> Float -> PriorityAccumulator id -> PriorityAccumulator id
register entityId basePriority (PriorityAccumulator entries) =
  PriorityAccumulator $
    Map.insert
      entityId
      PriorityEntry {peBase = basePriority, peAccumulated = 0.0}
      entries

-- | Remove an entity from tracking.
unregister :: (Ord id) => id -> PriorityAccumulator id -> PriorityAccumulator id
unregister entityId (PriorityAccumulator entries) =
  PriorityAccumulator $ Map.delete entityId entries

-- | Advance accumulated priority for all entities by @dt@ seconds.
--
-- Call this once per tick with elapsed time.
accumulate :: Float -> PriorityAccumulator id -> PriorityAccumulator id
accumulate dt (PriorityAccumulator entries) =
  PriorityAccumulator $
    Map.map
      (\e -> e & #peAccumulated %~ (+ (peBase e * dt)))
      entries

-- | Apply a priority modifier to a specific entity.
--
-- Use with interest management: closer entities get modifier > 1.0,
-- farther entities get modifier < 1.0.
applyModifier :: (Ord id) => id -> Float -> PriorityAccumulator id -> PriorityAccumulator id
applyModifier entityId modifier (PriorityAccumulator entries) =
  PriorityAccumulator $
    Map.adjust
      (\e -> e & #peAccumulated %~ (* modifier))
      entityId
      entries

-- | Drain the highest-priority entities that fit within @budgetBytes@.
--
-- @sizeFunc@ returns the serialized size in bytes for a given entity ID.
-- Returns the selected entity IDs in priority order (highest first),
-- and the updated accumulator with those entities' priorities reset.
drainTop ::
  (Ord id) =>
  -- | Byte budget
  Int ->
  -- | Size function: id -> bytes
  (id -> Int) ->
  PriorityAccumulator id ->
  ([id], PriorityAccumulator id)
drainTop budgetBytes sizeFunc (PriorityAccumulator entries) =
  let -- Sort by accumulated priority (descending)
      sorted =
        sortBy (comparing (Down . peAccumulated . snd)) $
          Map.toList entries

      -- Select entities that fit in budget
      (selected, _remaining) = selectWithinBudget budgetBytes sizeFunc sorted []

      -- Reset priority for selected entities
      entries' =
        foldr
          (Map.adjust (#peAccumulated .~ 0.0))
          entries
          selected
   in (selected, PriorityAccumulator entries')

-- | Helper to select entities within budget.
selectWithinBudget ::
  Int ->
  (id -> Int) ->
  [(id, PriorityEntry)] ->
  [id] ->
  ([id], Int)
selectWithinBudget remaining _ [] acc = (reverse acc, remaining)
selectWithinBudget remaining sizeFunc ((eid, _) : rest) acc =
  let size = sizeFunc eid
   in if size > remaining
        then selectWithinBudget remaining sizeFunc rest acc
        else selectWithinBudget (remaining - size) sizeFunc rest (eid : acc)

-- | Number of tracked entities.
priorityCount :: PriorityAccumulator id -> Int
priorityCount = Map.size . paEntries

-- | Whether the accumulator is empty.
priorityIsEmpty :: PriorityAccumulator id -> Bool
priorityIsEmpty = Map.null . paEntries

-- | Get the current accumulated priority for an entity.
getPriority :: (Ord id) => id -> PriorityAccumulator id -> Maybe Float
getPriority entityId (PriorityAccumulator entries) =
  peAccumulated <$> Map.lookup entityId entries
