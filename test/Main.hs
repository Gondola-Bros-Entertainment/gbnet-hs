{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.Reader
import Data.Word (Word8, Word16)

main :: IO ()
main = do
  putStrLn "=== GB-Net Haskell Serialization Tests ==="
  putStrLn ""

  testBoolRoundTrip
  testWord8RoundTrip
  testWord16RoundTrip
  testMultiField
  testBitPacking
  testMonadicReader

  putStrLn ""
  putStrLn "All tests passed!"

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then putStrLn $ "  PASS: " ++ name
    else error $ "  FAIL: " ++ name
              ++ " expected " ++ show expected
              ++ " got " ++ show actual

-- --------------------------------------------------------------------
-- Tests
-- --------------------------------------------------------------------

testBoolRoundTrip :: IO ()
testBoolRoundTrip = do
  putStrLn "Bool round-trip:"
  let buf = writeBit False $ writeBit True empty
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
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
          $ empty
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
  let buf = writeBits 512 10
          $ writeBits 768 10
          $ writeBits 100 7
          $ writeBits 1   1
          $ empty
  assertEqual "bit position = 28" 28 (bitPosition buf)
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

testMonadicReader :: IO ()
testMonadicReader = do
  putStrLn "Monadic reader:"
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
            empty
  let readFields = do
        v1 <- deserializeM :: BitReader Bool
        v2 <- deserializeM :: BitReader Word16
        v3 <- deserializeM :: BitReader Word8
        pure (v1, v2, v3)
  case runBitReader readFields buf of
    Left err -> error $ "Monadic read failed: " ++ err
    Right ((v1, v2, v3), _remainingBuf) -> do
      assertEqual "monad: Bool" True v1
      assertEqual "monad: Word16" (5000 :: Word16) v2
      assertEqual "monad: Word8" (99 :: Word8) v3
