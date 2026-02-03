-- |
-- Module      : GBNet.Serialize.Reader
-- Description : Monadic bit reader for clean deserialization
--
-- Provides 'BitReader', a monad that threads buffer state and error
-- handling through sequential read operations.

module GBNet.Serialize.Reader
  ( BitReader
  , runBitReader
  , readBitM
  , readBitsM
  , deserializeM
  ) where

import Data.Word (Word64)
import GBNet.Serialize.BitBuffer (BitBuffer, ReadResult(..), readBit, readBits)
import GBNet.Serialize.Class (BitDeserialize(..))

-- | State monad combining buffer threading with error handling.
newtype BitReader a = BitReader
  { runBitReader :: BitBuffer -> Either String (a, BitBuffer) }

instance Functor BitReader where
  fmap f (BitReader run) = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err
      Right (val, buf') -> Right (f val, buf')

instance Applicative BitReader where
  pure val = BitReader $ \buf -> Right (val, buf)

  (BitReader runF) <*> (BitReader runA) = BitReader $ \buf ->
    case runF buf of
      Left err         -> Left err
      Right (f, buf')  ->
        case runA buf' of
          Left err          -> Left err
          Right (val, buf'') -> Right (f val, buf'')

instance Monad BitReader where
  (BitReader run) >>= f = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err
      Right (val, buf') ->
        let (BitReader run') = f val
        in run' buf'

-- | Read a single bit.
readBitM :: BitReader Bool
readBitM = BitReader $ \buf ->
  case readBit buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Read N bits as a Word64.
readBitsM :: Int -> BitReader Word64
readBitsM n = BitReader $ \buf ->
  case readBits n buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Deserialize any BitDeserialize type via the monad.
deserializeM :: (BitDeserialize a) => BitReader a
deserializeM = BitReader $ \buf ->
  case bitDeserialize buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')
