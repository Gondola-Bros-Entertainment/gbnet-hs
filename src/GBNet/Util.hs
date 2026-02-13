-- |
-- Module      : GBNet.Util
-- Description : Sequence number wraparound and deterministic RNG
--
-- Shared utilities used across the library: circular sequence number
-- comparison, signed difference, and a pure SplitMix-based RNG.
module GBNet.Util
  ( sequenceHalfRange,
    sequenceGreaterThan,
    sequenceDiff,

    -- * Deterministic RNG (LCG + SplitMix output mixing)
    nextRandom,
    randomDouble,
  )
where

import Data.Bits (shiftR, xor, (.&.))
import Data.Int (Int32)
import Data.Word (Word16, Word64)
import GBNet.Types (SequenceNum (..))

-- | Half the Word16 sequence space, used for wraparound comparison.
sequenceHalfRange :: Word16
sequenceHalfRange = 32768

-- | Compares two sequence numbers accounting for Word16 wraparound.
-- Returns True if s1 is "greater than" s2 in circular sequence space.
sequenceGreaterThan :: SequenceNum -> SequenceNum -> Bool
sequenceGreaterThan (SequenceNum s1) (SequenceNum s2) =
  ((s1 > s2) && (s1 - s2 <= sequenceHalfRange))
    || ((s1 < s2) && (s2 - s1 > sequenceHalfRange))
{-# INLINE sequenceGreaterThan #-}

-- | Computes the signed difference between two sequence numbers,
-- accounting for Word16 wraparound.
sequenceDiff :: SequenceNum -> SequenceNum -> Int32
sequenceDiff (SequenceNum s1) (SequenceNum s2)
  | diff > half = diff - full
  | diff < -half = diff + full
  | otherwise = diff
  where
    half = fromIntegral sequenceHalfRange :: Int32
    full = half * 2
    diff = fromIntegral s1 - fromIntegral s2 :: Int32
{-# INLINE sequenceDiff #-}

-- | LCG constants.
lcgMultiplier, lcgIncrement :: Word64
lcgMultiplier = 6364136223846793005
lcgIncrement = 1442695040888963407

-- | SplitMix-style random number generator.
-- Returns (output, nextState). Pure, deterministic, no IO.
nextRandom :: Word64 -> (Word64, Word64)
nextRandom s =
  let next = lcgMultiplier * s + lcgIncrement
      -- SplitMix-style output mixing (state is not exposed)
      z0 = next `xor` (next `shiftR` 30)
      z1 = z0 * 0xBF58476D1CE4E5B9
      z2 = z1 `xor` (z1 `shiftR` 27)
      z3 = z2 * 0x94D049BB133111EB
      output = z3 `xor` (z3 `shiftR` 31)
   in (output, next)
{-# INLINE nextRandom #-}

-- | Convert random Word64 to double in [0, 1).
-- Uses lower 32 bits; SplitMix output mixing ensures all bits are well-distributed.
randomDouble :: Word64 -> Double
randomDouble w = fromIntegral (w .&. 0xFFFFFFFF) / 4294967296.0
{-# INLINE randomDouble #-}
