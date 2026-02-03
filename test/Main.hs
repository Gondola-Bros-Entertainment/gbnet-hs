{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Main where

import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.Reader
import GBNet.Serialize.TH
import GBNet.Packet
import GBNet.Util
import GBNet.Reliability

import Data.Bits ((.&.))
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T



-- TH-derived test types (must be before main due to TH staging restriction)
data Vec3 = Vec3
  { vecX :: Float
  , vecY :: Float
  , vecZ :: Float
  } deriving (Eq, Show)

deriveNetworkSerialize ''Vec3

data Color = Red | Green | Blue | Yellow deriving (Eq, Show)

deriveNetworkSerialize ''Color

data GameEvent
  = PlayerJoin Word8
  | PlayerLeave Word8
  | ChatMessage Word8 Word16
  deriving (Eq, Show)

deriveNetworkSerialize ''GameEvent

data SimpleRecord = SimpleRecord
  { srFlag  :: Bool
  , srValue :: Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''SimpleRecord

-- Test type using BitWidth for custom bit-width fields.
-- A compact player state: 7-bit health (0-127), 4-bit direction (0-15).
data CompactPlayer = CompactPlayer
  { cpHealth    :: BitWidth 7 Word8
  , cpDirection :: BitWidth 4 Word8
  } deriving (Eq, Show)

deriveNetworkSerialize ''CompactPlayer

main :: IO ()
main = do
  putStrLn "=== GB-Net Haskell Serialization Tests ==="
  putStrLn ""

  -- Primitive types
  testBoolRoundTrip
  testWord8RoundTrip
  testWord16RoundTrip
  testMultiField
  testBitPacking
  testMonadicReader
  testSignedIntegers
  testFloatingPoint

  -- Packet types
  testPacketType
  testPacketHeaderRoundTrip
  testPacketHeaderMonadic

  -- Collection types
  testMaybeRoundTrip
  testListRoundTrip
  testTextRoundTrip
  testTupleRoundTrip

  -- Phase 1: Measure mode + debug
  testSerializedSize
  testToBitString

  -- Phase 2: Fast paths (verified via existing tests passing)
  testFastPathAligned

  -- Phase 3: TH derive
  testTHRecord
  testTHEnum
  testTHEnumWithPayloads

  -- Phase 4: Serialization fixes
  testFastPathAfterNonAligned
  testDeserializeBoundsCheck
  testTextUnicode
  testBitWidthRoundTrip

  -- Reliability module
  testSequenceGreaterThan
  testSequenceDiff
  testSequenceAtBoundaries
  testSequenceBufferOps
  testSequenceBufferWraparound
  testSequenceBufferCollision
  testRttConvergence
  testAdaptiveRto
  testPacketLossTracking
  testAckBitsNoFalseAck
  testProcessAcksReturnsChannelInfo
  testInFlightEviction
  testFastRetransmit

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

-- | Helper to run a BitReader and assert the result.
assertDeserialize :: (Eq a, Show a) => String -> a -> BitReader a -> BitBuffer -> IO ()
assertDeserialize name expected reader buf =
  case runBitReader reader buf of
    Left err -> error $ "  FAIL: " ++ name ++ " deserialization error: " ++ err
    Right (val, _) -> assertEqual name expected val

-- --------------------------------------------------------------------
-- Primitive tests
-- --------------------------------------------------------------------

testBoolRoundTrip :: IO ()
testBoolRoundTrip = do
  putStrLn "Bool round-trip:"
  let buf = writeBit False $ writeBit True empty
  let reader = do
        v1 <- readBitM
        v2 <- readBitM
        pure (v1, v2)
  assertDeserialize "Bool (True, False)" (True, False) reader buf

testWord8RoundTrip :: IO ()
testWord8RoundTrip = do
  putStrLn "Word8 round-trip:"
  let buf = bitSerialize (42 :: Word8) empty
  assertDeserialize "Word8 42" (42 :: Word8) deserializeM buf

testWord16RoundTrip :: IO ()
testWord16RoundTrip = do
  putStrLn "Word16 round-trip:"
  let buf = bitSerialize (1234 :: Word16) empty
  assertDeserialize "Word16 1234" (1234 :: Word16) deserializeM buf

testMultiField :: IO ()
testMultiField = do
  putStrLn "Multi-field serialization:"
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
            empty
  let reader = do
        v1 <- deserializeM :: BitReader Bool
        v2 <- deserializeM :: BitReader Word16
        v3 <- deserializeM :: BitReader Word8
        pure (v1, v2, v3)
  assertDeserialize "multi-field" (True, 5000 :: Word16, 99 :: Word8) reader buf

testBitPacking :: IO ()
testBitPacking = do
  putStrLn "Bit-level packing:"
  let buf = writeBits 512 10
          $ writeBits 768 10
          $ writeBits 100 7
          $ writeBits 1   1
            empty
  assertEqual "bit position = 28" 28 (bitPosition buf)
  let reader = do
        mv <- readBitsM 1
        hp <- readBitsM 7
        y  <- readBitsM 10
        x  <- readBitsM 10
        pure (mv, hp, y, x)
  assertDeserialize "bit packing" (1, 100, 768, 512) reader buf

testMonadicReader :: IO ()
testMonadicReader = do
  putStrLn "Monadic reader:"
  let buf = bitSerialize (99 :: Word8)
          $ bitSerialize (5000 :: Word16)
          $ bitSerialize True
            empty
  let reader = do
        v1 <- deserializeM :: BitReader Bool
        v2 <- deserializeM :: BitReader Word16
        v3 <- deserializeM :: BitReader Word8
        pure (v1, v2, v3)
  assertDeserialize "monadic multi-field" (True, 5000 :: Word16, 99 :: Word8) reader buf

-- --------------------------------------------------------------------
-- Signed integer tests
-- --------------------------------------------------------------------

testSignedIntegers :: IO ()
testSignedIntegers = do
  putStrLn "Signed integer round-trips:"

  -- Int8: range -128 to 127
  let buf8 = bitSerialize (-1 :: Int8)
           $ bitSerialize (127 :: Int8)
           $ bitSerialize (-128 :: Int8)
             empty
  let reader8 = do
        a <- deserializeM :: BitReader Int8
        b <- deserializeM :: BitReader Int8
        c <- deserializeM :: BitReader Int8
        pure (a, b, c)
  assertDeserialize "Int8 boundary" (-128, 127, -1 :: Int8) reader8 buf8

  -- Int16
  let buf16 = bitSerialize (-1000 :: Int16)
            $ bitSerialize (32767 :: Int16)
              empty
  let reader16 = do
        a <- deserializeM :: BitReader Int16
        b <- deserializeM :: BitReader Int16
        pure (a, b)
  assertDeserialize "Int16 boundary" (32767, -1000 :: Int16) reader16 buf16

  -- Int32
  let buf32 = bitSerialize (-100000 :: Int32) empty
  assertDeserialize "Int32 -100000" (-100000 :: Int32) deserializeM buf32

  -- Int64
  let buf64 = bitSerialize (-9999999999 :: Int64) empty
  assertDeserialize "Int64 -9999999999" (-9999999999 :: Int64) deserializeM buf64

-- --------------------------------------------------------------------
-- Floating-point tests
-- --------------------------------------------------------------------

testFloatingPoint :: IO ()
testFloatingPoint = do
  putStrLn "Floating-point round-trips:"

  -- Float
  let bufF = bitSerialize (3.14159 :: Float)
           $ bitSerialize (-0.0 :: Float)
             empty
  let readerF = do
        a <- deserializeM :: BitReader Float
        b <- deserializeM :: BitReader Float
        pure (a, b)
  assertDeserialize "Float pair" (-0.0, 3.14159 :: Float) readerF bufF

  -- Double
  let bufD = bitSerialize (2.718281828459045 :: Double) empty
  assertDeserialize "Double e" (2.718281828459045 :: Double) deserializeM bufD

  -- Infinity
  let bufInf = bitSerialize (1.0 / 0.0 :: Float) empty
  case runBitReader (deserializeM :: BitReader Float) bufInf of
    Left err -> error $ "  FAIL: Float Infinity: " ++ err
    Right (a, _) ->
      if isInfinite a
        then putStrLn "  PASS: Float Infinity"
        else error $ "  FAIL: Float Infinity, got " ++ show a

-- --------------------------------------------------------------------
-- Packet type tests
-- --------------------------------------------------------------------

testPacketType :: IO ()
testPacketType = do
  putStrLn "PacketType round-trips:"
  let allTypes = [ConnectionRequest, ConnectionAccepted, ConnectionDenied,
                  Payload, Disconnect, Keepalive]
  let buf = foldl (flip bitSerialize) empty allTypes
  let reader = mapM (\_ -> deserializeM :: BitReader PacketType) allTypes
  case runBitReader reader buf of
    Left err -> error err
    Right (results, _) ->
      mapM_ (\(expected, actual) ->
        assertEqual ("PacketType " ++ show expected) expected actual
      ) (zip allTypes results)

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
  assertEqual "header bit size" packetHeaderBitSize (bitPosition buf)
  case runBitReader (deserializeM :: BitReader PacketHeader) buf of
    Left err -> error err
    Right (hdr', _) -> do
      assertEqual "packetType"  (packetType hdr)  (packetType hdr')
      assertEqual "sequenceNum" (sequenceNum hdr) (sequenceNum hdr')
      assertEqual "ack"         (ack hdr)         (ack hdr')
      assertEqual "ackBitfield" (ackBitfield hdr) (ackBitfield hdr')

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
  let reader = do
        h        <- deserializeM :: BitReader PacketHeader
        trailing <- deserializeM :: BitReader Word8
        pure (h, trailing)
  case runBitReader reader buf of
    Left err -> error $ "Monadic header read failed: " ++ err
    Right ((h, trailing), _) -> do
      assertEqual "monadic packetType"  ConnectionRequest (packetType h)
      assertEqual "monadic sequenceNum" 65535             (sequenceNum h)
      assertEqual "monadic ack"         0                 (ack h)
      assertEqual "monadic ackBitfield" (0xFFFFFFFF :: Word32) (ackBitfield h)
      assertEqual "monadic trailing"    (99 :: Word8)     trailing

-- --------------------------------------------------------------------
-- Collection type tests
-- --------------------------------------------------------------------

testMaybeRoundTrip :: IO ()
testMaybeRoundTrip = do
  putStrLn "Maybe round-trips:"
  let bufN = bitSerialize (Nothing :: Maybe Word8) empty
  assertDeserialize "Nothing" (Nothing :: Maybe Word8) deserializeM bufN

  let bufJ = bitSerialize (Just (42 :: Word8)) empty
  assertDeserialize "Just 42" (Just 42 :: Maybe Word8) deserializeM bufJ

  let bufNested = bitSerialize (Just (Just True) :: Maybe (Maybe Bool)) empty
  assertDeserialize "Just (Just True)" (Just (Just True) :: Maybe (Maybe Bool)) deserializeM bufNested

testListRoundTrip :: IO ()
testListRoundTrip = do
  putStrLn "List round-trips:"
  let bufEmpty = bitSerialize ([] :: [Word8]) empty
  assertDeserialize "empty list" ([] :: [Word8]) deserializeM bufEmpty

  let xs = [10, 20, 30] :: [Word16]
  let bufList = bitSerialize xs empty
  assertDeserialize "list [10,20,30]" xs deserializeM bufList

testTextRoundTrip :: IO ()
testTextRoundTrip = do
  putStrLn "Text round-trips:"

  -- Empty text
  let bufEmpty = bitSerialize (T.empty :: T.Text) empty
  assertDeserialize "empty Text" T.empty deserializeM bufEmpty

  -- ASCII
  let bufHello = bitSerialize ("hello" :: T.Text) empty
  assertDeserialize "Text hello" ("hello" :: T.Text) deserializeM bufHello

  -- Unicode (multi-byte UTF-8)
  let bufUni = bitSerialize ("café" :: T.Text) empty
  assertDeserialize "Text café" ("café" :: T.Text) deserializeM bufUni

  -- Emoji (4-byte UTF-8)
  let bufEmoji = bitSerialize ("\x1F680" :: T.Text) empty
  assertDeserialize "Text rocket emoji" ("\x1F680" :: T.Text) deserializeM bufEmoji

testTupleRoundTrip :: IO ()
testTupleRoundTrip = do
  putStrLn "Tuple round-trips:"
  let buf2 = bitSerialize (True, 42 :: Word8) empty
  assertDeserialize "2-tuple" (True, 42 :: Word8) deserializeM buf2

  let buf3 = bitSerialize (False, 1000 :: Word16, -5 :: Int8) empty
  assertDeserialize "3-tuple" (False, 1000 :: Word16, -5 :: Int8) deserializeM buf3

  let buf4 = bitSerialize (True, 255 :: Word8, 3.14 :: Float, -100 :: Int16) empty
  assertDeserialize "4-tuple" (True, 255 :: Word8, 3.14 :: Float, -100 :: Int16) deserializeM buf4

-- --------------------------------------------------------------------
-- Phase 1: Measure mode + debug tests
-- --------------------------------------------------------------------

testSerializedSize :: IO ()
testSerializedSize = do
  putStrLn "Serialized size measurement:"
  assertEqual "Bool = 1 bit" 1 (serializedSizeBits (bitSerialize True))
  assertEqual "Word8 = 8 bits" 8 (serializedSizeBits (bitSerialize (42 :: Word8)))
  assertEqual "Word16 = 16 bits" 16 (serializedSizeBits (bitSerialize (0 :: Word16)))
  assertEqual "Word32 = 32 bits" 32 (serializedSizeBits (bitSerialize (0 :: Word32)))
  assertEqual "Word64 = 64 bits" 64 (serializedSizeBits (bitSerialize (0 :: Word64)))
  assertEqual "Word8 = 1 byte" 1 (serializedSizeBytes (bitSerialize (42 :: Word8)))
  assertEqual "Bool = 1 byte (rounded)" 1 (serializedSizeBytes (bitSerialize True))
  assertEqual "9 bits = 2 bytes" 2 (serializedSizeBytes (writeBits 0 9))
  -- PacketHeader should be 68 bits = 9 bytes
  let hdr = PacketHeader Payload 0 0 0
  assertEqual "PacketHeader = 68 bits" 68 (serializedSizeBits (bitSerialize hdr))
  assertEqual "PacketHeader = 9 bytes" 9 (serializedSizeBytes (bitSerialize hdr))

testToBitString :: IO ()
testToBitString = do
  putStrLn "toBitString debug output:"
  -- Single bit
  let buf1 = writeBit True empty
  assertEqual "single 1" "1" (toBitString buf1)
  -- Single zero bit
  let buf0 = writeBit False empty
  assertEqual "single 0" "0" (toBitString buf0)
  -- Full byte: 0xFF = 11111111
  let bufFF = writeBits 0xFF 8 empty
  assertEqual "0xFF" "11111111" (toBitString bufFF)
  -- 0x00 = 00000000
  let buf00 = writeBits 0x00 8 empty
  assertEqual "0x00" "00000000" (toBitString buf00)
  -- Multi-byte: 0xAB = 10101011, then 3 more bits of value 5 (101)
  let bufMixed = writeBits 5 3 $ writeBits 0xAB 8 empty
  assertEqual "0xAB + 3 bits" "10101011 101" (toBitString bufMixed)
  -- Empty buffer
  assertEqual "empty" "" (toBitString empty)

-- --------------------------------------------------------------------
-- Phase 2: Fast path tests
-- --------------------------------------------------------------------

testFastPathAligned :: IO ()
testFastPathAligned = do
  putStrLn "Fast path (byte-aligned) round-trips:"
  -- Write aligned bytes and read them back
  let buf = writeBits 0xDEADBEEF 32 $ writeBits 0xCAFE 16 $ writeBits 0x42 8 empty
  let reader = do
        a <- readBitsM 8
        b <- readBitsM 16
        c <- readBitsM 32
        pure (a, b, c)
  assertDeserialize "aligned 8+16+32" (0x42, 0xCAFE, 0xDEADBEEF) reader buf

  -- Verify fast path produces same result as bit-by-bit for Word64
  let buf64 = writeBits 0x0123456789ABCDEF 64 empty
  assertDeserialize "aligned 64-bit" (0x0123456789ABCDEF :: Word64) (readBitsM 64) buf64

  -- Mixed: non-aligned write followed by aligned
  let bufMixed = writeBits 0xFF 8 $ writeBit True empty
  let readerMixed = do
        b <- readBitM
        v <- readBitsM 8
        pure (b, v)
  assertDeserialize "non-aligned then aligned" (True, 0xFF) readerMixed bufMixed

-- --------------------------------------------------------------------
-- Phase 3: TH derive tests
-- --------------------------------------------------------------------

testTHRecord :: IO ()
testTHRecord = do
  putStrLn "TH derive record round-trips:"
  let v = Vec3 { vecX = 1.0, vecY = -2.5, vecZ = 100.0 }
  let buf = bitSerialize v empty
  assertDeserialize "Vec3" v deserializeM buf

  let sr = SimpleRecord { srFlag = True, srValue = 12345 }
  let bufSR = bitSerialize sr empty
  assertDeserialize "SimpleRecord" sr deserializeM bufSR

testTHEnum :: IO ()
testTHEnum = do
  putStrLn "TH derive enum round-trips:"
  let colors = [Red, Green, Blue, Yellow]
  let buf = foldl (flip bitSerialize) empty colors
  let reader = mapM (\_ -> deserializeM :: BitReader Color) colors
  case runBitReader reader buf of
    Left err -> error err
    Right (results, _) ->
      mapM_ (\(expected, actual) ->
        assertEqual ("Color " ++ show expected) expected actual
      ) (zip colors results)

testTHEnumWithPayloads :: IO ()
testTHEnumWithPayloads = do
  putStrLn "TH derive enum with payloads:"
  let events = [PlayerJoin 1, PlayerLeave 2, ChatMessage 3 1000]
  let buf = foldl (flip bitSerialize) empty events
  let reader = mapM (\_ -> deserializeM :: BitReader GameEvent) events
  case runBitReader reader buf of
    Left err -> error err
    Right (results, _) ->
      mapM_ (\(expected, actual) ->
        assertEqual ("GameEvent " ++ show expected) expected actual
      ) (zip events results)

-- --------------------------------------------------------------------
-- Phase 4: Serialization fix tests
-- --------------------------------------------------------------------

-- | Fix 1: Writing non-aligned bits then aligned bytes should preserve
-- all data. The old code truncated the buffer in the fast path.
testFastPathAfterNonAligned :: IO ()
testFastPathAfterNonAligned = do
  putStrLn "Fast path after non-aligned write:"
  -- Write 1 bit (non-aligned), then 7 bits to fill the byte,
  -- then 16 aligned bits via fast path
  let buf = writeBits 0xBEEF 16 $ writeBits 0x7F 7 $ writeBit True empty
  let reader = do
        b  <- readBitM
        v7 <- readBitsM 7
        v16 <- readBitsM 16
        pure (b, v7, v16)
  assertDeserialize "1+7+16 bits" (True, 0x7F, 0xBEEF) reader buf

  -- Write 8 aligned bits, then 8 more aligned (fast path overwrote the first 8 before fix)
  let buf2 = writeBits 0xCD 8 $ writeBits 0xAB 8 empty
  let reader2 = do
        a <- readBitsM 8
        b <- readBitsM 8
        pure (a, b)
  assertDeserialize "8+8 aligned" (0xAB, 0xCD) reader2 buf2

-- | Fix 2: Deserializing a buffer with a huge length prefix should
-- fail gracefully instead of attempting unbounded allocation.
testDeserializeBoundsCheck :: IO ()
testDeserializeBoundsCheck = do
  putStrLn "Deserialization bounds checking:"
  -- Craft a buffer with a 16-bit length of 65535 but no actual data.
  -- The deserializer should fail with a buffer underflow, not hang.
  let buf = writeBits 65535 16 empty
  case runBitReader (deserializeM :: BitReader [Word8]) buf of
    Left _  -> putStrLn "  PASS: huge list length rejected"
    Right _ -> error "  FAIL: should have rejected list with length 65535 and no data"

  case runBitReader (deserializeM :: BitReader T.Text) buf of
    Left _  -> putStrLn "  PASS: huge Text length rejected"
    Right _ -> error "  FAIL: should have rejected Text with length 65535 and no data"

-- | Text handles full Unicode correctly.
testTextUnicode :: IO ()
testTextUnicode = do
  putStrLn "Text Unicode handling:"
  -- CJK characters (3-byte UTF-8)
  let cjk = "\x4E16\x754C" :: T.Text  -- 世界
  let bufCjk = bitSerialize cjk empty
  assertDeserialize "Text CJK" cjk deserializeM bufCjk

  -- Mixed ASCII and multi-byte
  let mixed = "hello \x1F680 world" :: T.Text
  let bufMixed = bitSerialize mixed empty
  assertDeserialize "Text mixed Unicode" mixed deserializeM bufMixed

-- | Fix 4: BitWidth newtype for custom bit-width fields.
testBitWidthRoundTrip :: IO ()
testBitWidthRoundTrip = do
  putStrLn "BitWidth round-trips:"

  -- Direct BitWidth usage: 7 bits for a value 0-127
  let bw7 = BitWidth 100 :: BitWidth 7 Word8
  let buf7 = bitSerialize bw7 empty
  -- Should use exactly 7 bits
  assertEqual "BitWidth 7 = 7 bits" 7 (bitPosition buf7)
  assertDeserialize "BitWidth 7 Word8" bw7 deserializeM buf7

  -- TH-derived struct with BitWidth fields
  let player = CompactPlayer
        { cpHealth    = BitWidth 100
        , cpDirection = BitWidth 12
        }
  let bufP = bitSerialize player empty
  -- 7 + 4 = 11 bits total
  assertEqual "CompactPlayer = 11 bits" 11 (bitPosition bufP)
  assertDeserialize "CompactPlayer" player deserializeM bufP

-- --------------------------------------------------------------------
-- Reliability module tests
-- --------------------------------------------------------------------

testSequenceGreaterThan :: IO ()
testSequenceGreaterThan = do
  putStrLn "sequenceGreaterThan:"
  assertEqual "1 > 0" True (sequenceGreaterThan 1 0)
  assertEqual "0 > 1" False (sequenceGreaterThan 0 1)
  assertEqual "100 > 50" True (sequenceGreaterThan 100 50)
  assertEqual "50 > 100" False (sequenceGreaterThan 50 100)
  -- Wraparound
  assertEqual "0 > 65535" True (sequenceGreaterThan 0 65535)
  assertEqual "65535 > 0" False (sequenceGreaterThan 65535 0)
  assertEqual "1 > 65534" True (sequenceGreaterThan 1 65534)
  assertEqual "100 > 65500" True (sequenceGreaterThan 100 65500)

testSequenceDiff :: IO ()
testSequenceDiff = do
  putStrLn "sequenceDiff:"
  assertEqual "diff(5,3)" 2 (sequenceDiff 5 3)
  assertEqual "diff(3,5)" (-2) (sequenceDiff 3 5)
  assertEqual "diff(100,100)" 0 (sequenceDiff 100 100)
  -- Wraparound
  assertEqual "diff(0,65535)" 1 (sequenceDiff 0 65535)
  assertEqual "diff(65535,0)" (-1) (sequenceDiff 65535 0)
  assertEqual "diff(5,65530)" 11 (sequenceDiff 5 65530)

testSequenceAtBoundaries :: IO ()
testSequenceAtBoundaries = do
  putStrLn "Sequence at Word16 boundaries:"
  assertEqual "0 > maxBound" True (sequenceGreaterThan 0 maxBound)
  assertEqual "maxBound > 0" False (sequenceGreaterThan maxBound 0)
  assertEqual "diff(0,max)" 1 (sequenceDiff 0 maxBound)
  assertEqual "diff(max,0)" (-1) (sequenceDiff maxBound 0)

testSequenceBufferOps :: IO ()
testSequenceBufferOps = do
  putStrLn "SequenceBuffer operations:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert 0 100 buf0
  let buf2 = sbInsert 1 200 buf1
  let buf3 = sbInsert 2 300 buf2
  assertEqual "exists 0" True (sbExists 0 buf3)
  assertEqual "exists 1" True (sbExists 1 buf3)
  assertEqual "exists 2" True (sbExists 2 buf3)
  assertEqual "exists 3" False (sbExists 3 buf3)
  assertEqual "get 0" (Just 100) (sbGet 0 buf3)
  assertEqual "get 1" (Just 200) (sbGet 1 buf3)
  assertEqual "get 2" (Just 300) (sbGet 2 buf3)

testSequenceBufferWraparound :: IO ()
testSequenceBufferWraparound = do
  putStrLn "SequenceBuffer wraparound:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert 65534 100 buf0
  let buf2 = sbInsert 65535 200 buf1
  let buf3 = sbInsert 0 300 buf2
  let buf4 = sbInsert 1 400 buf3
  assertEqual "exists 65534" True (sbExists 65534 buf4)
  assertEqual "exists 65535" True (sbExists 65535 buf4)
  assertEqual "exists 0" True (sbExists 0 buf4)
  assertEqual "exists 1" True (sbExists 1 buf4)

testSequenceBufferCollision :: IO ()
testSequenceBufferCollision = do
  putStrLn "SequenceBuffer collision:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert 0 100 buf0
  assertEqual "exists 0 before" True (sbExists 0 buf1)
  -- Sequence 16 maps to the same slot (16 % 16 == 0)
  let buf2 = sbInsert 16 200 buf1
  assertEqual "exists 16" True (sbExists 16 buf2)
  assertEqual "exists 0 after" False (sbExists 0 buf2)
  assertEqual "get 16" (Just 200) (sbGet 16 buf2)
  assertEqual "get 0 after" Nothing (sbGet 0 buf2)

testRttConvergence :: IO ()
testRttConvergence = do
  putStrLn "RTT convergence:"
  let ep0 = newReliableEndpoint 256
  let ep = foldl (\e _ -> updateRtt 50.0 e) ep0 [1..20 :: Int]
  let srtt = srttMs ep
  assertEqual "SRTT near 50ms" True (srtt > 40.0 && srtt < 60.0)

testAdaptiveRto :: IO ()
testAdaptiveRto = do
  putStrLn "Adaptive RTO:"
  let ep0 = newReliableEndpoint 256
  -- First sample
  let ep1 = updateRtt 50.0 ep0
  assertEqual "RTO >= 50" True (rtoMs ep1 >= 50.0)
  -- High jitter
  let ep2 = updateRtt 200.0 ep1
  assertEqual "RTO increases with jitter" True (rtoMs ep2 > 50.0)
  -- RTO bounded
  let ep3 = updateRtt 5000.0 ep2
  assertEqual "RTO capped at 2000" True (rtoMs ep3 <= 2000.0)

testPacketLossTracking :: IO ()
testPacketLossTracking = do
  putStrLn "Packet loss tracking:"
  let ep0 = newReliableEndpoint 256
  -- 8 successes, 2 losses
  let ep1 = foldl (\e _ -> recordLossSample False e) ep0 [1..8 :: Int]
  let ep2 = foldl (\e _ -> recordLossSample True e) ep1 [1..2 :: Int]
  let loss = packetLossPercent ep2
  assertEqual "~20% loss" True (abs (loss - 0.2) < 0.01)

testAckBitsNoFalseAck :: IO ()
testAckBitsNoFalseAck = do
  putStrLn "ACK bits no false ack:"
  let ep0 = newReliableEndpoint 256
  -- Receive packet 0, then packet 2 (skip 1)
  let ep1 = onPacketReceived 0 ep0
  let ep2 = onPacketReceived 2 ep1
  let (ackVal, ackBitsVal) = getAckInfo ep2
  assertEqual "remote_sequence = 2" 2 ackVal
  -- bit 0 = ack-1 = seq 1 (NOT received)
  assertEqual "seq 1 not acked" 0 (ackBitsVal .&. 1)
  -- bit 1 = ack-2 = seq 0 (received)
  assertEqual "seq 0 acked" True ((ackBitsVal .&. 2) /= 0)

testProcessAcksReturnsChannelInfo :: IO ()
testProcessAcksReturnsChannelInfo = do
  putStrLn "processAcks returns channel info:"
  let ep0 = newReliableEndpoint 256
  let now = 1000000000 :: MonoTime  -- 1 second in nanoseconds
  let ep1 = onPacketSent 10 now 2 5 100 ep0
  let ep2 = onPacketSent 11 now 3 7 200 ep1
  -- ACK packet 11 directly, packet 10 via ack_bits (bit 0 = seq 10)
  let ackTime = 1050000000 :: MonoTime  -- 1.05 seconds
  let ((acked, _fastRetransmit), _ep3) = processAcks 11 1 ackTime ep2
  assertEqual "2 acked" 2 (length acked)
  assertEqual "contains (3,7)" True (elem (3, 7) acked)
  assertEqual "contains (2,5)" True (elem (2, 5) acked)

testInFlightEviction :: IO ()
testInFlightEviction = do
  putStrLn "In-flight eviction:"
  let ep0 = withMaxInFlight 4 $ newReliableEndpoint 256
  -- Send 4 packets
  let ep1 = foldl (\e i -> onPacketSent i (fromIntegral i * 1000000) 0 i 100 e) ep0 [0..3]
  assertEqual "4 in flight" 4 (packetsInFlight ep1)
  -- Send 5th — should evict one
  let ep2 = onPacketSent 4 4000000 0 4 100 ep1
  assertEqual "still 4 in flight" 4 (packetsInFlight ep2)
  assertEqual "1 evicted" 1 (rePacketsEvicted ep2)

testFastRetransmit :: IO ()
testFastRetransmit = do
  putStrLn "Fast retransmit:"
  let ep0 = newReliableEndpoint 256
  let now = 1000000000 :: MonoTime
  -- Send packets 0-4
  let ep1 = foldl (\e i -> onPacketSent i now 0 i 100 e) ep0 [0..4]
  -- ACK packets 1,2,3,4 but NOT 0 — seq 0 should accumulate nacks
  -- Each ack where 0 is in range but not covered increments nack
  let ackTime = 1050000000 :: MonoTime
  -- ACK 1: ack=1, ack_bits=0 (no bits). seq 0 is older, diff=1, bit 0 not set -> nack 0 once
  let (_, ep2) = processAcks 1 0 ackTime ep1
  -- ACK 2: ack=2, ack_bits=0. seq 0 diff=2, bit 1 not set -> nack again
  let (_, ep3) = processAcks 2 0 ackTime ep2
  -- ACK 3: ack=3, ack_bits=0. seq 0 diff=3, bit 2 not set -> nack = 3, triggers fast retransmit
  let ((_, fastRetransmit), _ep4) = processAcks 3 0 ackTime ep3
  assertEqual "fast retransmit triggered" True (not (null fastRetransmit))
  assertEqual "retransmit is (0,0)" True (elem (0, 0) fastRetransmit)
