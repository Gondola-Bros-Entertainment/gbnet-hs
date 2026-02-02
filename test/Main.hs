{-# LANGUAGE ScopedTypeVariables #-}
-- ^ GHC language extension. Like #![feature(...)] in Rust.
-- Lets us write type annotations inside patterns (v1 :: Word8).

-- |
-- Test suite for gb-net-hs serialization.
--
-- 'module Main where' — every Haskell executable needs a Main module.
-- Test suites are executables that return exit codes (0 = pass).
module Main where

-- Import our modules
import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.Reader
import Data.Word (Word8, Word16)

-- | 'main' is the entry point. Like fn main() in Rust.
-- 'IO ()' means "performs IO and returns nothing" (like -> () in Rust).
--
-- 'do' notation is Haskell's way of sequencing IO actions.
-- Without 'do', you'd chain everything with >>= (bind).
-- Think of 'do' as syntactic sugar that makes IO look imperative.
main :: IO ()
main = do
  putStrLn "=== GB-Net Haskell Serialization Tests ==="
  putStrLn ""

  -- Test 1: Single bit round-trip
  testBoolRoundTrip

  -- Test 2: Word8 round-trip
  testWord8RoundTrip

  -- Test 3: Word16 round-trip
  testWord16RoundTrip

  -- Test 4: Multi-field serialization
  testMultiField

  -- Test 5: Bit-level packing (the whole point of bitpacking!)
  testBitPacking

  -- Test 6: Monadic deserialization (the clean way!)
  testMonadicReader

  putStrLn ""
  putStrLn "All tests passed!"

-- ────────────────────────────────────────────────────────────────────
-- Helper: assert equality (poor man's test framework)
-- ────────────────────────────────────────────────────────────────────

-- | Assert two values are equal. Panics with message if not.
--
-- Type signature reads:
--   "For any type 'a' that supports equality (Eq) and printing (Show),
--    take a test name, expected value, actual value, and perform IO."
--
-- The '=>' separates constraints from the type.
-- (Eq a, Show a) is like 'where T: Eq + Debug' in Rust.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then putStrLn $ "  PASS: " ++ name
    -- '$' is function application with low precedence.
    -- putStrLn $ "foo" ++ "bar"  =  putStrLn ("foo" ++ "bar")
    -- It saves you from writing parentheses. Very common Haskell idiom.
    else error $ "  FAIL: " ++ name
              ++ " expected " ++ show expected
              ++ " got " ++ show actual

-- ────────────────────────────────────────────────────────────────────
-- Tests
-- ────────────────────────────────────────────────────────────────────

testBoolRoundTrip :: IO ()
testBoolRoundTrip = do
  putStrLn "Bool round-trip:"

  -- Write True then False
  let buf = writeBit False $ writeBit True empty
  --        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- Read right to left: start with 'empty', write True, then write False.
  -- Function application in Haskell is right-to-left without parens.
  -- Equivalent to: writeBit False (writeBit True empty)

  -- Read them back
  case readBit buf of
    Left err -> error err
    Right (ReadResult val1 buf') ->
      case readBit buf' of
        Left err -> error err
        Right (ReadResult val2 _buf'') -> do
          assertEqual "first bit = True" True val1
          assertEqual "second bit = False" False val2

testWord8RoundTrip :: IO ()
testWord8RoundTrip = do
  putStrLn "Word8 round-trip:"

  let buf = bitSerialize (42 :: Word8) empty
  -- The ':: Word8' is a type annotation. Haskell needs it here because
  -- '42' is polymorphic — it could be any numeric type. In Rust, you'd
  -- write '42u8'. Same idea, different syntax.

  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult val _) ->
      assertEqual "Word8 42" (42 :: Word8) val

testWord16RoundTrip :: IO ()
testWord16RoundTrip = do
  putStrLn "Word16 round-trip:"

  let buf = bitSerialize (1234 :: Word16) empty

  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult val _) ->
      assertEqual "Word16 1234" (1234 :: Word16) val

testMultiField :: IO ()
testMultiField = do
  putStrLn "Multi-field serialization:"

  -- Serialize three values into one buffer (like a struct).
  -- '$' chains right-to-left, so the FIRST thing written is Bool,
  -- then Word16, then Word8. We must deserialize in the same order.
  --
  -- Think of it like a pipeline reading bottom-to-top:
  --   empty -> write Bool -> write Word16 -> write Word8
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
          $ empty

  -- Deserialize in WRITE order: Bool first, Word16 second, Word8 third
  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult (v1 :: Bool) buf1) ->
      case bitDeserialize buf1 of
        Left err -> error err
        Right (ReadResult (v2 :: Word16) buf2) ->
          case bitDeserialize buf2 of
            Left err -> error err
            Right (ReadResult (v3 :: Word8) _) -> do
              assertEqual "field 1 (Bool)" True v1
              assertEqual "field 2 (Word16)" (5000 :: Word16) v2
              assertEqual "field 3 (Word8)" (99 :: Word8) v3

testBitPacking :: IO ()
testBitPacking = do
  putStrLn "Bit-level packing:"

  -- Write 1 + 7 + 10 + 10 = 28 bits (3.5 bytes)
  -- This is your PlayerUpdate from the Rust README!
  -- Remember: $ chains right-to-left, so moving is written first.
  -- We read in the same order things were written to the buffer.
  let buf = writeBits 512 10     -- x: 10 bits   (written last)
          $ writeBits 768 10     -- y: 10 bits
          $ writeBits 100 7      -- health: 7 bits
          $ writeBits 1   1      -- moving: 1 bit (written first)
          $ empty

  -- 28 bits written — that's 3.5 bytes, same as your Rust PlayerUpdate
  assertEqual "bit position = 28" 28 (bitPosition buf)

  -- Read back in WRITE order: moving, health, y, x
  case readBits 1 buf of
    Left err -> error err
    Right (ReadResult mv buf1) ->
      case readBits 7 buf1 of
        Left err -> error err
        Right (ReadResult hp buf2) ->
          case readBits 10 buf2 of
            Left err -> error err
            Right (ReadResult y buf3) ->
              case readBits 10 buf3 of
                Left err -> error err
                Right (ReadResult x _) -> do
                  assertEqual "moving = 1" 1 mv
                  assertEqual "health = 100" 100 hp
                  assertEqual "y = 768" 768 y
                  assertEqual "x = 512" 512 x

-- | Monadic deserialization — the clean way.
--
-- Compare this to testMultiField above. Same logic, but using
-- 'do' notation with our BitReader monad instead of nested case.
--
-- This is the Haskell equivalent of Rust's ? operator.
testMonadicReader :: IO ()
testMonadicReader = do
  putStrLn "Monadic reader (the clean way):"

  -- Same buffer as testMultiField: Bool, Word16, Word8
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
            empty

  -- OLD WAY (what we had before — nested case hell):
  --   case bitDeserialize buf of
  --     Left err -> ...
  --     Right (ReadResult v1 buf1) ->
  --       case bitDeserialize buf1 of
  --         Left err -> ...
  --         ... 20 more lines ...

  -- NEW WAY — do notation with BitReader monad:
  -- 'readPlayer' is a BitReader action. It describes WHAT to read,
  -- not HOW. The monad handles error propagation and state threading.
  let readFields = do
        v1 <- deserializeM :: BitReader Bool     -- <- is "bind" in do notation
        v2 <- deserializeM :: BitReader Word16   -- like `let v2 = read()?;` in Rust
        v3 <- deserializeM :: BitReader Word8    -- errors auto-propagate!
        pure (v1, v2, v3)                        -- wrap result (like Ok((...)))

  -- 'runBitReader' executes the action against a buffer.
  -- Like calling a closure with an argument.
  case runBitReader readFields buf of
    Left err -> error $ "Monadic read failed: " ++ err
    Right ((v1, v2, v3), _remainingBuf) -> do
      assertEqual "monad: Bool" True v1
      assertEqual "monad: Word16" (5000 :: Word16) v2
      assertEqual "monad: Word8" (99 :: Word8) v3
