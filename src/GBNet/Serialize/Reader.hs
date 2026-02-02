-- |
-- Module      : GBNet.Serialize.Reader
-- Description : Monadic bit reader for clean deserialization
--
-- == The Problem
--
-- Without this module, reading multiple values looks like:
--
-- @
-- case readBits 10 buf of
--   Left err -> Left err
--   Right (ReadResult x buf1) ->
--     case readBits 10 buf1 of
--       Left err -> Left err
--       Right (ReadResult y buf2) ->
--         Right (x, y, buf2)
-- @
--
-- That's 8 lines for 2 reads. With 5 fields it's 20+ lines of nesting.
-- This is the exact problem Rust's ? operator solves.
--
-- == The Solution: Monads
--
-- A monad is a design pattern for sequencing operations that have
-- "extra stuff" going on — in our case, error handling + state threading.
--
-- With our BitReader monad:
--
-- @
-- do
--   x <- readBitsM 10
--   y <- readBitsM 10
--   pure (x, y)
-- @
--
-- 3 lines. Errors auto-propagate. Buffer state auto-threads.
-- This is EXACTLY what Rust's ? does, but generalized.
--
-- == What IS a monad though?
--
-- Forget the math. Practically, a monad is any type that supports:
--
-- 1. 'pure' (aka 'return') — wrap a plain value.
--    Like Ok(value) in Rust.
--
-- 2. '>>=' (pronounced "bind") — chain operations.
--    Like .and_then() in Rust.
--
-- That's it. If your type has those two operations, it's a monad,
-- and you get 'do' notation for free.
--
-- Common monads you already know (by Rust analogy):
--   Maybe     = Option<T>      — chain operations that might return None
--   Either    = Result<T, E>   — chain operations that might fail
--   IO        = side effects   — chain operations that do IO
--   BitReader = our custom one — chain reads with error + state

module GBNet.Serialize.Reader
  ( -- * The BitReader monad
    BitReader
    -- * Running a reader
  , runBitReader

    -- * Reading operations
  , readBitM
  , readBitsM

    -- * Deserialization via the monad
  , deserializeM
  ) where

import Data.Word (Word64)
import GBNet.Serialize.BitBuffer (BitBuffer, ReadResult(..), readBit, readBits)
import GBNet.Serialize.Class (BitDeserialize(..))

-- | The BitReader monad. It wraps a function that takes a BitBuffer
-- and returns either an error or a value + updated buffer.
--
-- In Rust terms, this is like:
-- @
-- struct BitReader<A> {
--     run: Box<dyn FnOnce(&mut BitBuffer) -> Result<A, String>>
-- }
-- @
--
-- But in Haskell, functions are first-class and lightweight — no Box needed.
--
-- 'newtype' is like a zero-cost wrapper. Same as:
--   struct BitReader<A>(fn(BitBuffer) -> Result<(A, BitBuffer), String>);
-- It's erased at compile time — zero runtime overhead.
newtype BitReader a = BitReader
  { runBitReader :: BitBuffer -> Either String (a, BitBuffer)
    --               ^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    --               input state   Result: Ok((value, new_state)) or Err(msg)
  }

-- Now we implement the monad typeclasses. There are three, layered:
--   Functor -> Applicative -> Monad
-- Each builds on the previous. Think of it like a trait hierarchy:
--   trait Functor        { fn map(...) }
--   trait Applicative: Functor { fn pure(...); fn ap(...) }
--   trait Monad: Applicative   { fn bind(...) }

-- | Functor: "things you can map over."
-- Like implementing .map() for your type.
--
-- fmap f reader = "run the reader, then apply f to the result"
instance Functor BitReader where
  fmap f (BitReader run) = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err
      Right (val, buf') -> Right (f val, buf')
    -- Equivalent Rust:
    --   reader.map(|val| f(val))

-- | Applicative: "things you can combine."
-- Adds 'pure' (wrap a value) and '<*>' (apply a wrapped function).
instance Applicative BitReader where
  -- 'pure' wraps a plain value into the monad. Like Ok(value).
  pure val = BitReader $ \buf -> Right (val, buf)
    -- Returns the value without touching the buffer.
    -- Rust: Ok((val, buf))

  -- '<*>' applies a wrapped function to a wrapped value.
  -- You rarely use this directly — 'do' notation calls it for you.
  (BitReader runF) <*> (BitReader runA) = BitReader $ \buf ->
    case runF buf of
      Left err         -> Left err
      Right (f, buf')  ->
        case runA buf' of
          Left err          -> Left err
          Right (val, buf'') -> Right (f val, buf'')

-- | Monad: "things you can chain."
-- This is the big one. '>>='' (bind) is what makes 'do' notation work.
--
-- (>>=) :: BitReader a -> (a -> BitReader b) -> BitReader b
--
-- Translation: "run the first reader, feed its result to a function
-- that returns a second reader, run that one too."
--
-- This is EXACTLY like Result::and_then() in Rust:
--   reader1.and_then(|val| reader2(val))
instance Monad BitReader where
  (BitReader run) >>= f = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err           -- Short-circuit on error (like ?)
      Right (val, buf') ->
        let (BitReader run') = f val          -- Get the next reader
        in run' buf'                          -- Run it with updated buffer
    -- That's it. That's the whole monad. Everything else is sugar.

-- ────────────────────────────────────────────────────────────────────
-- Reading operations
-- ────────────────────────────────────────────────────────────────────

-- | Read a single bit. Monadic version of readBit.
--
-- Instead of returning Either String (ReadResult Bool),
-- it returns BitReader Bool — the error handling and state
-- threading are handled by the monad.
readBitM :: BitReader Bool
readBitM = BitReader $ \buf ->
  case readBit buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Read N bits as a Word64. Monadic version of readBits.
readBitsM :: Int -> BitReader Word64
readBitsM n = BitReader $ \buf ->
  case readBits n buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Deserialize any BitDeserialize type via the monad.
-- This bridges the typeclass world with the monad world.
--
-- Usage:
-- @
--   do
--     (x :: Word8) <- deserializeM
--     (y :: Word16) <- deserializeM
--     pure (x, y)
-- @
deserializeM :: (BitDeserialize a) => BitReader a
deserializeM = BitReader $ \buf ->
  case bitDeserialize buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

