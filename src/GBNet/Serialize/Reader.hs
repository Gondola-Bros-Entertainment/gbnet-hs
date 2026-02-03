-- |
-- Module      : GBNet.Serialize.Reader
-- Description : Monadic bit reader for clean deserialization
--
-- Re-exports 'BitReader' and deserialization utilities for convenient imports.

module GBNet.Serialize.Reader
  ( BitReader
  , runBitReader
  , readBitM
  , readBitsM
  , deserializeM
  , runDeserialize
  ) where

import GBNet.Serialize.BitBuffer (BitReader(..), readBitM, readBitsM)
import GBNet.Serialize.Class (deserializeM, runDeserialize)
