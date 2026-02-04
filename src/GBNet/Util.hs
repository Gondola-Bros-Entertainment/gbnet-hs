-- | Shared sequence number utilities for Word16 wraparound arithmetic.
module GBNet.Util
  ( sequenceHalfRange,
    sequenceGreaterThan,
    sequenceDiff,
  )
where

import Data.Int (Int32)
import Data.Word (Word16)
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
sequenceDiff (SequenceNum s1) (SequenceNum s2) =
  let half = fromIntegral sequenceHalfRange :: Int32
      full = half * 2
      diff = fromIntegral s1 - fromIntegral s2 :: Int32
   in if diff > half
        then diff - full
        else
          if diff < negate half
            then diff + full
            else diff
{-# INLINE sequenceDiff #-}
