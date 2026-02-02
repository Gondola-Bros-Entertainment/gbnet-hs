-- |
-- Module      : GBNet.Serialize.Reader
-- Description : Monadic bit reader for clean deserialization
--
-- Provides 'BitReader', a monad that threads buffer state and error
-- handling through sequential read operations — the Haskell equivalent
-- of chaining reads with Rust's @?@ operator.
--
-- == The Problem This Solves
--
-- Without the monad, deserializing multiple fields requires deeply
-- nested @case@ expressions:
--
-- @
--   case readBit buf of
--     Left err -> Left err
--     Right (ReadResult v1 buf1) ->
--       case readBits 16 buf1 of
--         Left err -> Left err
--         Right (ReadResult v2 buf2) ->
--           case readBits 8 buf2 of
--             Left err -> Left err
--             Right (ReadResult v3 buf3) -> ...
-- @
--
-- Each level manually propagates errors ('Left') and threads the
-- updated buffer. This is tedious and error-prone (easy to accidentally
-- reuse an old buffer variable like @buf1@ instead of @buf2@).
--
-- In Rust, the @?@ operator automates error propagation but not state
-- threading (the buffer is mutated in place via @&mut self@). In Haskell,
-- the 'BitReader' monad automates /both/:
--
-- @
--   do
--     v1 <- readBitM
--     v2 <- readBitsM 16
--     v3 <- readBitsM 8
--     pure (v1, v2, v3)
-- @
--
-- This is equivalent to the nested @case@ above but much cleaner. The
-- monad's @>>=@ (bind) operator handles both error propagation and
-- buffer threading behind the scenes.
--
-- == The Functor/Applicative/Monad Hierarchy
--
-- Haskell requires that every 'Monad' also be a 'Functor' and
-- 'Applicative'. This hierarchy exists because each level adds power:
--
--   * 'Functor': transform the value inside a context (@fmap@).
--     For 'BitReader', this means mapping over the result without
--     changing the buffer state or error behavior.
--
--   * 'Applicative': combine independent computations. @\<*\>@ runs
--     two readers sequentially and combines their results. Useful for
--     applying a multi-argument constructor to multiple reads.
--
--   * 'Monad': chain dependent computations. @>>=@ (bind) lets the
--     second computation depend on the /result/ of the first.
--     This is the most common pattern in deserialization.
--
-- Rust doesn't have this hierarchy — @?@ is baked into the language
-- syntax. Haskell's approach is more general: the same monad machinery
-- works for IO, parser combinators, state machines, and more.

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

-- | Monad wrapping @BitBuffer -> Either String (a, BitBuffer)@.
--
-- This is a /state monad/ combined with /error handling/:
--   * The @BitBuffer@ parameter is the state being threaded through
--   * The @Either String@ handles errors (buffer underflow, etc.)
--   * The @a@ is the value produced by a successful read
--
-- == Why 'newtype' (not 'data' or 'type')
--
-- 'newtype' is a zero-cost wrapper — it exists only at compile time for
-- type safety, and is completely erased at runtime. The generated code
-- is identical to a bare function @BitBuffer -> Either String (a, BitBuffer)@.
--
-- Compare to Rust's @struct BitReader(fn)@ — the wrapping is erased in
-- release builds. We use 'newtype' instead of:
--   * @data@ — would add an extra indirection (pointer) at runtime
--   * @type@ — would be a mere synonym with no type safety (any
--     function with the same signature could be used by accident)
newtype BitReader a = BitReader
  { runBitReader :: BitBuffer -> Either String (a, BitBuffer)
    -- ^ Unwrap and run the reader. Given a buffer, produces either
    -- an error message or a @(value, updatedBuffer)@ pair.
    --
    -- Note the return type uses a plain tuple @(a, BitBuffer)@ rather
    -- than 'ReadResult' — this is conventional for state monads and
    -- makes the Functor/Applicative/Monad instances cleaner.
  }

-- | 'Functor' instance: apply a function to the result without
-- changing the buffer state or error behavior.
--
-- @fmap f reader@ runs @reader@, and if it succeeds, applies @f@ to
-- the value. The buffer state passes through unchanged.
--
-- Example: @fmap (+1) (readBitsM 8)@ reads 8 bits, then adds 1.
instance Functor BitReader where
  fmap f (BitReader run) = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err
        -- ^ Errors pass through unchanged — 'fmap' never "catches"
        -- errors. This is a law: @fmap f (Left e) == Left e@.
      Right (val, buf') -> Right (f val, buf')
        -- ^ Apply @f@ to the value, pass the buffer through.

-- | 'Applicative' instance: combine independent readers.
--
-- 'pure' injects a value into the monad without reading anything.
-- @\<*\>@ sequences two readers, applying the function from the first
-- to the value from the second.
instance Applicative BitReader where
  pure val = BitReader $ \buf -> Right (val, buf)
    -- ^ 'pure' wraps a value without consuming any bits. The buffer
    -- passes through unchanged. This is the "return" operation —
    -- Haskell's @pure@ is equivalent to Rust's @Ok(val)@ in a
    -- Result-returning context.

  (BitReader runF) <*> (BitReader runA) = BitReader $ \buf ->
    case runF buf of
      Left err         -> Left err
      Right (f, buf')  ->
        -- ^ First reader succeeded with a function @f@ and updated
        -- buffer @buf'@. Now run the second reader with @buf'@.
        case runA buf' of
          Left err          -> Left err
          Right (val, buf'') -> Right (f val, buf'')
            -- ^ Both succeeded: apply the function to the value,
            -- return the final buffer state.

-- | 'Monad' instance: chain dependent readers.
--
-- @reader >>= f@ runs @reader@, feeds its value to @f@ (which returns
-- a new reader), and runs that. This is the key operation that lets
-- later reads depend on earlier results:
--
-- @
--   do
--     tag <- readBitsM 4
--     case tag of
--       0 -> readSmallPacket    -- different readers depending on tag
--       1 -> readLargePacket
-- @
--
-- The @>>=@ operator (pronounced "bind") is what makes @do@-notation
-- work. The @do@ block above desugars to:
--
-- @
--   readBitsM 4 >>= \\tag -> case tag of ...
-- @
instance Monad BitReader where
  (BitReader run) >>= f = BitReader $ \buf ->
    case run buf of
      Left err         -> Left err
        -- ^ Error propagation: if the first reader fails, short-circuit
        -- immediately. This is the monad doing what Rust's @?@ does.
      Right (val, buf') ->
        let (BitReader run') = f val
          -- ^ Apply @f@ to get the next reader. This is where the
          -- "dependent" part happens — @f@ can inspect @val@ and
          -- choose what to read next.
        in run' buf'
          -- ^ Run the next reader with the updated buffer.

-- --------------------------------------------------------------------
-- Reading operations
-- --------------------------------------------------------------------

-- | Read a single bit, yielding a 'Bool'.
--
-- Wraps 'readBit' from "GBNet.Serialize.BitBuffer" into the 'BitReader'
-- monad. The @ReadResult@ from the raw API is unpacked into the
-- @(value, buffer)@ tuple that the monad expects.
readBitM :: BitReader Bool
readBitM = BitReader $ \buf ->
  case readBit buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Read N bits as a 'Word64'.
--
-- Wraps 'readBits' from "GBNet.Serialize.BitBuffer" into the monad.
readBitsM :: Int -> BitReader Word64
readBitsM n = BitReader $ \buf ->
  case readBits n buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')

-- | Deserialize any 'BitDeserialize' type via the monad.
--
-- This is the bridge between the typeclass-based API and the monadic API.
-- Any type with a 'BitDeserialize' instance can be read in a @do@ block:
--
-- @
--   do
--     (header :: PacketHeader) <- deserializeM
--     (seqNum :: Word16)       <- deserializeM
--     pure (header, seqNum)
-- @
--
-- The @(BitDeserialize a) =>@ constraint means this function works for
-- any type @a@ that has a 'BitDeserialize' instance — GHC picks the
-- right implementation at compile time based on the inferred or
-- annotated type.
deserializeM :: (BitDeserialize a) => BitReader a
deserializeM = BitReader $ \buf ->
  case bitDeserialize buf of
    Left err -> Left err
    Right (ReadResult val buf') -> Right (val, buf')
