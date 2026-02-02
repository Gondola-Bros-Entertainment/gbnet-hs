{-# LANGUAGE ScopedTypeVariables #-}
-- ^ 'ScopedTypeVariables' is a GHC language extension that lets type
-- annotations inside patterns refer to type variables from the outer
-- signature. Without it, writing @(v1 :: Bool)@ in a pattern would
-- not work — Haskell2010 only allows type annotations on top-level
-- bindings.
--
-- We need this because 'bitDeserialize' is polymorphic — it can return
-- any type with a 'BitDeserialize' instance. The pattern annotation
-- @(v1 :: Bool)@ tells GHC which instance to use. Without it, GHC
-- would report an "ambiguous type variable" error.

module Main where

-- Import the buffer operations directly (writeBit, readBit, etc.)
import GBNet.Serialize.BitBuffer
-- Import the typeclass methods (bitSerialize, bitDeserialize)
import GBNet.Serialize.Class
-- Import the monadic reader for cleaner deserialization
import GBNet.Serialize.Reader
-- Import the packet types and header
import GBNet.Packet

-- Unsigned integer types
import Data.Word (Word8, Word16, Word32)
-- Signed integer types for testing two's complement serialization
import Data.Int (Int8, Int16, Int32, Int64)

-- | Entry point. Runs all test groups sequentially and prints results.
--
-- This is a simple test harness — each test function prints PASS/FAIL
-- for its assertions. If any assertion fails, 'error' is called, which
-- terminates the program with a non-zero exit code. The cabal test
-- runner detects this via @exitcode-stdio-1.0@ and reports failure.
--
-- A production test suite would use a framework like HUnit or Hspec,
-- but this direct approach has zero dependencies and makes the test
-- logic completely transparent.
main :: IO ()
main = do
  putStrLn "=== GB-Net Haskell Serialization Tests ==="
  putStrLn ""

  -- Original tests (Phase 1 — must still pass after ByteString refactor)
  testBoolRoundTrip
  testWord8RoundTrip
  testWord16RoundTrip
  testMultiField
  testBitPacking
  testMonadicReader

  -- New tests (Phase 3 — signed integers, floats, packet header)
  testSignedIntegers
  testFloatingPoint
  testPacketType
  testPacketHeaderRoundTrip
  testPacketHeaderMonadic

  putStrLn ""
  putStrLn "All tests passed!"

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

-- | Assert that two values are equal. Prints PASS on success, calls
-- 'error' on failure (which terminates the program).
--
-- The @(Eq a, Show a)@ constraint requires that the type supports
-- both equality testing and string conversion. This is Haskell's
-- equivalent of Rust's @where T: PartialEq + Debug@ bound.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then putStrLn $ "  PASS: " ++ name
    else error $ "  FAIL: " ++ name
              ++ " expected " ++ show expected
              ++ " got " ++ show actual


-- --------------------------------------------------------------------
-- Original tests
-- --------------------------------------------------------------------

-- | Test that Bool values (single bits) round-trip correctly.
--
-- Writes True then False into an empty buffer, reads them back.
-- Note that writes compose right-to-left via @$@, so True is written
-- first (into the empty buffer), then False is written second.
-- Reading happens left-to-right (from bit 0), so we read True first.
testBoolRoundTrip :: IO ()
testBoolRoundTrip = do
  putStrLn "Bool round-trip:"
  let buf = writeBit False $ writeBit True empty
  -- ^ Buffer now contains: bit0=True, bit1=False
  -- The nested case pattern: each read returns Either, so we must
  -- pattern-match on Left/Right, then extract the ReadResult to get
  -- both the value and the updated buffer for the next read.
  case readBit buf of
    Left err -> error err
    Right (ReadResult val1 buf') ->
      case readBit buf' of
        Left err -> error err
        Right (ReadResult val2 _buf'') -> do
          assertEqual "first bit = True" True val1
          assertEqual "second bit = False" False val2

-- | Test Word8 serialization round-trip.
testWord8RoundTrip :: IO ()
testWord8RoundTrip = do
  putStrLn "Word8 round-trip:"
  let buf = bitSerialize (42 :: Word8) empty
  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult val _) ->
      assertEqual "Word8 42" (42 :: Word8) val

-- | Test Word16 serialization round-trip.
testWord16RoundTrip :: IO ()
testWord16RoundTrip = do
  putStrLn "Word16 round-trip:"
  let buf = bitSerialize (1234 :: Word16) empty
  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult val _) ->
      assertEqual "Word16 1234" (1234 :: Word16) val

-- | Test serializing multiple fields of different types.
--
-- Demonstrates the "builder" pattern: values are serialized
-- right-to-left (innermost @$@ first), creating a buffer with
-- True, then Word16 5000, then Word8 99. Reading back happens
-- left-to-right, matching the write order.
--
-- The nested case pattern here shows exactly why the BitReader monad
-- exists — see testMonadicReader for the clean version.
testMultiField :: IO ()
testMultiField = do
  putStrLn "Multi-field serialization:"
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
            empty
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

-- | Test sub-byte bit packing with arbitrary widths.
--
-- Writes 4 fields at non-standard widths (1, 7, 10, 10 bits = 28 total).
-- Verifies the write cursor is at position 28, then reads all fields back.
-- This is the core use case for game networking: packing a position
-- update into as few bits as possible.
testBitPacking :: IO ()
testBitPacking = do
  putStrLn "Bit-level packing:"
  let buf = writeBits 512 10
          $ writeBits 768 10
          $ writeBits 100 7
          $ writeBits 1   1
            empty
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

-- | Test the monadic reader — same data as testMultiField but with
-- clean @do@-notation instead of nested cases.
--
-- Compare the 12 lines of nested cases in testMultiField to the 4
-- lines of @do@-notation here. The monad handles buffer threading
-- and error propagation automatically.
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

-- --------------------------------------------------------------------
-- Signed integer tests
-- --------------------------------------------------------------------

-- | Test signed integer round-trips across all widths.
--
-- Two's complement ensures that negative values survive the
-- signed -> unsigned -> write -> read -> unsigned -> signed conversion.
-- We test boundary values (min/max) and typical negative values.
testSignedIntegers :: IO ()
testSignedIntegers = do
  putStrLn "Signed integer round-trips:"

  -- Int8: range -128 to 127
  let buf8 = bitSerialize (-1 :: Int8) $ bitSerialize (127 :: Int8) $ bitSerialize (-128 :: Int8) empty
  case runBitReader (do { a <- deserializeM; b <- deserializeM; c <- deserializeM; pure (a :: Int8, b :: Int8, c :: Int8) }) buf8 of
    Left err -> error err
    Right ((a, b, c), _) -> do
      assertEqual "Int8 -128" (-128 :: Int8) a
      assertEqual "Int8 127" (127 :: Int8) b
      assertEqual "Int8 -1" (-1 :: Int8) c

  -- Int16: range -32768 to 32767
  let buf16 = bitSerialize (-1000 :: Int16) $ bitSerialize (32767 :: Int16) empty
  case runBitReader (do { a <- deserializeM; b <- deserializeM; pure (a :: Int16, b :: Int16) }) buf16 of
    Left err -> error err
    Right ((a, b), _) -> do
      assertEqual "Int16 32767" (32767 :: Int16) a
      assertEqual "Int16 -1000" (-1000 :: Int16) b

  -- Int32
  let buf32 = bitSerialize (-100000 :: Int32) empty
  case runBitReader (deserializeM :: BitReader Int32) buf32 of
    Left err -> error err
    Right (a, _) -> assertEqual "Int32 -100000" (-100000 :: Int32) a

  -- Int64
  let buf64 = bitSerialize (-9999999999 :: Int64) empty
  case runBitReader (deserializeM :: BitReader Int64) buf64 of
    Left err -> error err
    Right (a, _) -> assertEqual "Int64 -9999999999" (-9999999999 :: Int64) a

-- --------------------------------------------------------------------
-- Floating-point tests
-- --------------------------------------------------------------------

-- | Test Float and Double round-trips via IEEE 754 bit representation.
--
-- The cast functions (castFloatToWord32, etc.) preserve the exact bit
-- pattern, so round-tripping should be bit-exact — not just approximate.
-- We test with assertApproxEqual as a safety measure.
testFloatingPoint :: IO ()
testFloatingPoint = do
  putStrLn "Floating-point round-trips:"

  -- Float (32-bit IEEE 754)
  -- The bit cast is exact — no rounding occurs. So we can compare
  -- directly with assertEqual. We just need to compare Float to Float
  -- (not Float to Double), since the literal 3.14159 as a Float is
  -- a slightly different value than 3.14159 as a Double.
  let bufF = bitSerialize (3.14159 :: Float) $ bitSerialize (-0.0 :: Float) empty
  case runBitReader (do { a <- deserializeM; b <- deserializeM; pure (a :: Float, b :: Float) }) bufF of
    Left err -> error err
    Right ((a, b), _) -> do
      assertEqual "Float -0.0" (-0.0 :: Float) a
      assertEqual "Float 3.14159" (3.14159 :: Float) b

  -- Double (64-bit IEEE 754)
  let bufD = bitSerialize (2.718281828459045 :: Double) empty
  case runBitReader (deserializeM :: BitReader Double) bufD of
    Left err -> error err
    Right (a, _) -> assertEqual "Double 2.71828..." (2.718281828459045 :: Double) a

  -- Special values
  let bufInf = bitSerialize (1.0 / 0.0 :: Float) empty
  case runBitReader (deserializeM :: BitReader Float) bufInf of
    Left err -> error err
    Right (a, _) ->
      if isInfinite a
        then putStrLn "  PASS: Float Infinity"
        else error $ "  FAIL: Float Infinity, got " ++ show a

-- --------------------------------------------------------------------
-- Packet type tests
-- --------------------------------------------------------------------

-- | Test PacketType serialization round-trip for all 6 variants.
testPacketType :: IO ()
testPacketType = do
  putStrLn "PacketType round-trips:"
  let allTypes = [ConnectionRequest, ConnectionAccepted, ConnectionDenied,
                  Payload, Disconnect, Keepalive]
  -- Serialize all types into one buffer. We use 'foldl' with 'flip'
  -- because 'bitSerialize' takes @value -> buffer -> buffer@, so
  -- @flip bitSerialize@ gives us @buffer -> value -> buffer@, which
  -- is the accumulator signature 'foldl' expects. This writes the
  -- types left-to-right (ConnectionRequest first).
  let buf = foldl (flip bitSerialize) empty allTypes
  -- Read them back
  let readAll = mapM (\_ -> deserializeM :: BitReader PacketType) allTypes
  case runBitReader readAll buf of
    Left err -> error err
    Right (results, _) ->
      mapM_ (\(expected, actual) ->
        assertEqual ("PacketType " ++ show expected) expected actual
      ) (zip allTypes results)

-- | Test PacketHeader full round-trip.
testPacketHeaderRoundTrip :: IO ()
testPacketHeaderRoundTrip = do
  putStrLn "PacketHeader round-trip:"
  let hdr = PacketHeader
        { packetType  = Payload
        , sequenceNum = 42
        , ack         = 40
        , ackBitfield = 0xDEADBEEF
        }
  let buf = bitSerialize hdr empty
  -- Verify the header occupies exactly 68 bits
  assertEqual "header bit size" packetHeaderBitSize (bitPosition buf)
  case bitDeserialize buf of
    Left err -> error err
    Right (ReadResult hdr' _) -> do
      assertEqual "packetType" (packetType hdr) (packetType hdr')
      assertEqual "sequenceNum" (sequenceNum hdr) (sequenceNum hdr')
      assertEqual "ack" (ack hdr) (ack hdr')
      assertEqual "ackBitfield" (ackBitfield hdr) (ackBitfield hdr')

-- | Test PacketHeader deserialization using the monadic reader.
testPacketHeaderMonadic :: IO ()
testPacketHeaderMonadic = do
  putStrLn "PacketHeader monadic round-trip:"
  let hdr = PacketHeader
        { packetType  = ConnectionRequest
        , sequenceNum = 65535
        , ack         = 0
        , ackBitfield = 0xFFFFFFFF
        }
  let buf = bitSerialize (99 :: Word8) $ bitSerialize hdr empty
  -- Read header then a trailing Word8 using the monad
  let readAll = do
        h <- deserializeM :: BitReader PacketHeader
        trailing <- deserializeM :: BitReader Word8
        pure (h, trailing)
  case runBitReader readAll buf of
    Left err -> error $ "Monadic header read failed: " ++ err
    Right ((h, trailing), _) -> do
      assertEqual "monadic packetType" ConnectionRequest (packetType h)
      assertEqual "monadic sequenceNum" 65535 (sequenceNum h)
      assertEqual "monadic ack" 0 (ack h)
      assertEqual "monadic ackBitfield" (0xFFFFFFFF :: Word32) (ackBitfield h)
      assertEqual "monadic trailing Word8" (99 :: Word8) trailing
