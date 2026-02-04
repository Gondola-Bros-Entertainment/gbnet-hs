{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Config (NetworkConfig (..), defaultNetworkConfig)
import GBNet.Congestion
import GBNet.Interest
import GBNet.Interpolation
import GBNet.Packet
import GBNet.Connection (DisconnectReason (..))
import GBNet.Peer
import GBNet.Priority
import GBNet.Reliability
import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.TH
import GBNet.Socket (UdpSocket (..))
import GBNet.Stats (CongestionLevel (..), SocketStats (..), defaultSocketStats)
import GBNet.TestNet
import GBNet.Util
import qualified Network.Socket as NS
import Network.Socket (SockAddr (..), tupleToHostAddress)

-- TH-derived test types (must be before main due to TH staging restriction)
data Vec3 = Vec3
  { vecX :: Float,
    vecY :: Float,
    vecZ :: Float
  }
  deriving (Eq, Show)

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
  { srFlag :: Bool,
    srValue :: Word16
  }
  deriving (Eq, Show)

deriveNetworkSerialize ''SimpleRecord

-- Test type using BitWidth for custom bit-width fields.
-- A compact player state: 7-bit health (0-127), 4-bit direction (0-15).
data CompactPlayer = CompactPlayer
  { cpHealth :: BitWidth 7 Word8,
    cpDirection :: BitWidth 4 Word8
  }
  deriving (Eq, Show)

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

  -- Congestion control
  testBinaryCongestionModeTransition
  testBinaryRateRecovery
  testCwndSlowStart
  testCwndLossHalvesCwnd
  testCwndSlowStartRestart
  testCongestionLevelBinary
  testCongestionLevelWindow
  testBatchAndUnbatch

  -- Integration: Pure peer API
  testPeerHandshake
  testPeerMessageDelivery
  testPeerDisconnect
  testPeerConnectionTimeout
  testPeerMaxClients

  -- Integration: Full TestNet polymorphic lifecycle
  testTestNetHandshake
  testTestNetMessageRoundTrip

  -- Replication: Interest management
  testRadiusInterestRelevant
  testRadiusInterestPriority
  testGridInterestRelevant

  -- Replication: Priority accumulator
  testPriorityAccumulate
  testPriorityDrain
  testPriorityUnregister

  -- Replication: Snapshot interpolation
  testSnapshotPushAndReady
  testSnapshotInterpolation
  testSnapshotOutOfOrder

  putStrLn ""
  putStrLn "All tests passed!"

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then putStrLn $ "  PASS: " ++ name
    else
      error $
        "  FAIL: "
          ++ name
          ++ " expected "
          ++ show expected
          ++ " got "
          ++ show actual

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
  let buf =
        bitSerialize (99 :: Word8) $
          bitSerialize (5000 :: Word16) $
            bitSerialize
              True
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
  let buf =
        writeBits 512 10 $
          writeBits 768 10 $
            writeBits 100 7 $
              writeBits
                1
                1
                empty
  assertEqual "bit position = 28" 28 (bitPosition buf)
  let reader = do
        mv <- readBitsM 1
        hp <- readBitsM 7
        y <- readBitsM 10
        x <- readBitsM 10
        pure (mv, hp, y, x)
  assertDeserialize "bit packing" (1, 100, 768, 512) reader buf

testMonadicReader :: IO ()
testMonadicReader = do
  putStrLn "Monadic reader:"
  let buf =
        bitSerialize (99 :: Word8) $
          bitSerialize (5000 :: Word16) $
            bitSerialize
              True
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
  let buf8 =
        bitSerialize (-1 :: Int8) $
          bitSerialize (127 :: Int8) $
            bitSerialize
              (-128 :: Int8)
              empty
  let reader8 = do
        a <- deserializeM :: BitReader Int8
        b <- deserializeM :: BitReader Int8
        c <- deserializeM :: BitReader Int8
        pure (a, b, c)
  assertDeserialize "Int8 boundary" (-128, 127, -1 :: Int8) reader8 buf8

  -- Int16
  let buf16 =
        bitSerialize (-1000 :: Int16) $
          bitSerialize
            (32767 :: Int16)
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
  let bufF =
        bitSerialize (3.14159 :: Float) $
          bitSerialize
            (-0.0 :: Float)
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
  let allTypes =
        [ ConnectionRequest,
          ConnectionAccepted,
          ConnectionDenied,
          Payload,
          Disconnect,
          Keepalive
        ]
  let buf = foldl (flip bitSerialize) empty allTypes
  let reader = mapM (\_ -> deserializeM :: BitReader PacketType) allTypes
  case runBitReader reader buf of
    Left err -> error err
    Right (results, _) ->
      mapM_
        ( \(expected, actual) ->
            assertEqual ("PacketType " ++ show expected) expected actual
        )
        (zip allTypes results)

testPacketHeaderRoundTrip :: IO ()
testPacketHeaderRoundTrip = do
  putStrLn "PacketHeader round-trip:"
  let hdr =
        PacketHeader
          { packetType = Payload,
            sequenceNum = 42,
            ack = 40,
            ackBitfield = 0xDEADBEEF
          }
  let buf = bitSerialize hdr empty
  assertEqual "header bit size" packetHeaderBitSize (bitPosition buf)
  case runBitReader (deserializeM :: BitReader PacketHeader) buf of
    Left err -> error err
    Right (hdr', _) -> do
      assertEqual "packetType" (packetType hdr) (packetType hdr')
      assertEqual "sequenceNum" (sequenceNum hdr) (sequenceNum hdr')
      assertEqual "ack" (ack hdr) (ack hdr')
      assertEqual "ackBitfield" (ackBitfield hdr) (ackBitfield hdr')

testPacketHeaderMonadic :: IO ()
testPacketHeaderMonadic = do
  putStrLn "PacketHeader monadic round-trip:"
  let hdr =
        PacketHeader
          { packetType = ConnectionRequest,
            sequenceNum = 65535,
            ack = 0,
            ackBitfield = 0xFFFFFFFF
          }
  let buf = bitSerialize (99 :: Word8) $ bitSerialize hdr empty
  let reader = do
        h <- deserializeM :: BitReader PacketHeader
        trailing <- deserializeM :: BitReader Word8
        pure (h, trailing)
  case runBitReader reader buf of
    Left err -> error $ "Monadic header read failed: " ++ err
    Right ((h, trailing), _) -> do
      assertEqual "monadic packetType" ConnectionRequest (packetType h)
      assertEqual "monadic sequenceNum" 65535 (sequenceNum h)
      assertEqual "monadic ack" 0 (ack h)
      assertEqual "monadic ackBitfield" (0xFFFFFFFF :: Word32) (ackBitfield h)
      assertEqual "monadic trailing" (99 :: Word8) trailing

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
  let v = Vec3 {vecX = 1.0, vecY = -2.5, vecZ = 100.0}
  let buf = bitSerialize v empty
  assertDeserialize "Vec3" v deserializeM buf

  let sr = SimpleRecord {srFlag = True, srValue = 12345}
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
      mapM_
        ( \(expected, actual) ->
            assertEqual ("Color " ++ show expected) expected actual
        )
        (zip colors results)

testTHEnumWithPayloads :: IO ()
testTHEnumWithPayloads = do
  putStrLn "TH derive enum with payloads:"
  let events = [PlayerJoin 1, PlayerLeave 2, ChatMessage 3 1000]
  let buf = foldl (flip bitSerialize) empty events
  let reader = mapM (\_ -> deserializeM :: BitReader GameEvent) events
  case runBitReader reader buf of
    Left err -> error err
    Right (results, _) ->
      mapM_
        ( \(expected, actual) ->
            assertEqual ("GameEvent " ++ show expected) expected actual
        )
        (zip events results)

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
        b <- readBitM
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
    Left _ -> putStrLn "  PASS: huge list length rejected"
    Right _ -> error "  FAIL: should have rejected list with length 65535 and no data"

  case runBitReader (deserializeM :: BitReader T.Text) buf of
    Left _ -> putStrLn "  PASS: huge Text length rejected"
    Right _ -> error "  FAIL: should have rejected Text with length 65535 and no data"

-- | Text handles full Unicode correctly.
testTextUnicode :: IO ()
testTextUnicode = do
  putStrLn "Text Unicode handling:"
  -- CJK characters (3-byte UTF-8)
  let cjk = "\x4E16\x754C" :: T.Text -- 世界
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
  let player =
        CompactPlayer
          { cpHealth = BitWidth 100,
            cpDirection = BitWidth 12
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
  let ep = iterate (updateRtt 50.0) ep0 !! (20 :: Int)
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
  let ep1 = iterate (recordLossSample False) ep0 !! (8 :: Int)
  let ep2 = iterate (recordLossSample True) ep1 !! (2 :: Int)
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
  let now = 1000000000 :: MonoTime -- 1 second in nanoseconds
  let ep1 = onPacketSent 10 now 2 5 100 ep0
  let ep2 = onPacketSent 11 now 3 7 200 ep1
  -- ACK packet 11 directly, packet 10 via ack_bits (bit 0 = seq 10)
  let ackTime = 1050000000 :: MonoTime -- 1.05 seconds
  let ((acked, _fastRetransmit), _ep3) = processAcks 11 1 ackTime ep2
  assertEqual "2 acked" 2 (length acked)
  assertEqual "contains (3,7)" True ((3, 7) `elem` acked)
  assertEqual "contains (2,5)" True ((2, 5) `elem` acked)

testInFlightEviction :: IO ()
testInFlightEviction = do
  putStrLn "In-flight eviction:"
  let ep0 = withMaxInFlight 4 $ newReliableEndpoint 256
  -- Send 4 packets
  let ep1 = foldl (\e i -> onPacketSent i (fromIntegral i * 1000000) 0 i 100 e) ep0 [0 .. 3]
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
  let ep1 = foldl (\e i -> onPacketSent i now 0 i 100 e) ep0 [0 .. 4]
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
  assertEqual "retransmit is (0,0)" True ((0, 0) `elem` fastRetransmit)

-- --------------------------------------------------------------------
-- Congestion control tests
-- --------------------------------------------------------------------

testBinaryCongestionModeTransition :: IO ()
testBinaryCongestionModeTransition = do
  putStrLn "Binary congestion mode transition:"
  let cc0 = newCongestionController 10.0 0.05 250.0 1000.0
  assertEqual "initial mode" CongestionGood (ccMode cc0)
  -- High loss triggers Bad
  let cc1 = ccUpdate 0.10 100.0 1000000000 cc0
  assertEqual "bad on high loss" CongestionBad (ccMode cc1)
  -- Good conditions start recovery timer
  let cc2 = ccUpdate 0.00 50.0 2000000000 cc1
  assertEqual "still bad (recovering)" CongestionBad (ccMode cc2)
  -- After recovery time passes, transition back to Good
  let cc3 = ccUpdate 0.00 50.0 4000000000 cc2
  assertEqual "back to good" CongestionGood (ccMode cc3)

testBinaryRateRecovery :: IO ()
testBinaryRateRecovery = do
  putStrLn "Binary rate AIMD recovery:"
  let cc0 = newCongestionController 10.0 0.05 250.0 1000.0
  assertEqual "initial rate" 10.0 (ccCurrentSendRate cc0)
  -- Additive increase in Good mode
  let cc1 = ccUpdate 0.00 50.0 1000000000 cc0
  assertEqual "rate increased" True (ccCurrentSendRate cc1 > ccCurrentSendRate cc0)
  -- Multiplicative decrease on loss
  let cc2 = ccUpdate 0.10 100.0 2000000000 cc1
  assertEqual "rate decreased" True (ccCurrentSendRate cc2 < ccCurrentSendRate cc1)
  -- Rate should be halved from current, not from base
  let expectedRate = ccCurrentSendRate cc1 * congestionRateReduction
  assertEqual "rate = current * 0.5" True (abs (ccCurrentSendRate cc2 - expectedRate) < 0.01)

testCwndSlowStart :: IO ()
testCwndSlowStart = do
  putStrLn "CWND slow start:"
  let cw0 = newCongestionWindow 1200
  assertEqual "initial phase" SlowStart (cwPhase cw0)
  let initialCwnd = cwCwnd cw0
  -- ACK doubles cwnd in slow start
  let cw1 = cwOnAck 1200 cw0
  assertEqual "cwnd grew" True (cwCwnd cw1 > initialCwnd)
  assertEqual "still slow start" SlowStart (cwPhase cw1)

testCwndLossHalvesCwnd :: IO ()
testCwndLossHalvesCwnd = do
  putStrLn "CWND loss halves cwnd:"
  let cw0 = newCongestionWindow 1200
  let cw1 = cwOnAck 12000 cw0 -- Grow cwnd
  let cw2 = cwOnLoss cw1
  assertEqual "cwnd halved" True (cwCwnd cw2 < cwCwnd cw1)
  assertEqual "enters recovery" Recovery (cwPhase cw2)
  let expectedCwnd = max (fromIntegral minCwndBytes) (cwCwnd cw1 / 2.0)
  assertEqual "cwnd = max(min, old/2)" True (abs (cwCwnd cw2 - expectedCwnd) < 1.0)

testCwndSlowStartRestart :: IO ()
testCwndSlowStartRestart = do
  putStrLn "CWND slow start restart (RFC 2861):"
  let mtu = 1200
  let cw0 = newCongestionWindow mtu
  -- Grow cwnd past initial
  let cw1 = cwOnAck 24000 $ cwOnAck 24000 cw0
  let bigCwnd = cwCwnd cw1
  -- Record a send time
  let now = 1000000000 :: MonoTime
  let cw2 = cwOnSend 1200 now cw1
  -- Idle for longer than 2 * RTO (say RTO = 200ms)
  let laterTime = now + 500000000 -- 500ms later
  let rtoMs = 200.0
  let cw3 = cwSlowStartRestart rtoMs laterTime cw2
  assertEqual "resets to SlowStart" SlowStart (cwPhase cw3)
  assertEqual "cwnd reset to initial" True (cwCwnd cw3 < bigCwnd)
  assertEqual "ssthresh = old cwnd" True (abs (cwSsthresh cw3 - bigCwnd) < 1.0)

testCongestionLevelBinary :: IO ()
testCongestionLevelBinary = do
  putStrLn "Binary congestion level:"
  let cc0 = newCongestionController 10.0 0.05 250.0 1000.0
  assertEqual "good = None" CongestionNone (ccCongestionLevel cc0)
  let cc1 = ccUpdate 0.10 100.0 1000000000 cc0
  assertEqual "bad = Critical" CongestionCritical (ccCongestionLevel cc1)

testCongestionLevelWindow :: IO ()
testCongestionLevelWindow = do
  putStrLn "Window congestion level:"
  let cw0 = newCongestionWindow 1200
  assertEqual "empty = None" CongestionNone (cwCongestionLevel cw0)
  -- Fill most of the window
  let cw1 = cw0 {cwBytesInFlight = floor (cwCwnd cw0 * 0.96)}
  assertEqual "96% = Critical" CongestionCritical (cwCongestionLevel cw1)
  let cw2 = cw0 {cwBytesInFlight = floor (cwCwnd cw0 * 0.75)}
  assertEqual "75% = Elevated" CongestionElevated (cwCongestionLevel cw2)

testBatchAndUnbatch :: IO ()
testBatchAndUnbatch = do
  putStrLn "Message batching round-trip:"
  let msgs = ["hello", "world", "foo"]
  let batched = batchMessages msgs 1200
  assertEqual "1 batch" 1 (length batched)
  case unbatchMessages (head batched) of
    Nothing -> error "  FAIL: unbatch returned Nothing"
    Just result -> assertEqual "round-trip" msgs result

-- --------------------------------------------------------------------
-- Integration: TestNet peer lifecycle
-- --------------------------------------------------------------------

-- Helper to create SockAddr for tests
testAddr :: Word16 -> SockAddr
testAddr port = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

-- | Create a dummy UdpSocket for testing pure peer operations.
-- The socket is real (to satisfy strict fields) but never used for I/O.
newTestUdpSocket :: IO UdpSocket
newTestUdpSocket = do
  sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
  pure UdpSocket {usSocket = sock, usStats = defaultSocketStats}

testPeerHandshake :: IO ()
testPeerHandshake = do
  putStrLn "Peer handshake via TestNet:"
  let serverAddr = testAddr 7777
      clientAddr = testAddr 8888
      config = defaultNetworkConfig
      now = 0 :: MonoTime

  sock <- newTestUdpSocket

  -- Create client peer state and initiate connection
  let clientPeer0 = newPeerState sock clientAddr config now
      clientPeer1 = peerConnect (peerIdFromAddr serverAddr) now clientPeer0

  -- Process: client produces connect request
  let clientResult = peerProcess now [] clientPeer1
      clientOutgoing = prOutgoing clientResult

  assertEqual "client sends packets" True (not (null clientOutgoing))

  -- Verify server starts empty
  let serverPeer = newPeerState sock serverAddr config now
  assertEqual "server starts empty" 0 (peerCount serverPeer)

  -- Client has 0 actual connections (all pending)
  assertEqual "client has 0 connections (pending)" 0 (peerCount clientPeer1)

  putStrLn "  PASS: Peer handshake (packet generation verified)"

testPeerMessageDelivery :: IO ()
testPeerMessageDelivery = do
  putStrLn "Peer message delivery:"
  sock <- newTestUdpSocket
  let addr = testAddr 9999
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Send a message to a non-connected peer should fail
  let result = peerSend (peerIdFromAddr (testAddr 1234)) 0 "hello" now peer
  case result of
    Left _ -> putStrLn "  PASS: send to unconnected peer fails"
    Right _ -> error "  FAIL: should have failed"

  -- Broadcast to empty peer should be no-op
  let peer' = peerBroadcast 0 "test" Nothing now peer
  assertEqual "broadcast to empty" 0 (peerCount peer')
  putStrLn "  PASS: broadcast to empty peer is no-op"

testPeerDisconnect :: IO ()
testPeerDisconnect = do
  putStrLn "Peer disconnect:"
  sock <- newTestUdpSocket
  let addr = testAddr 5555
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Disconnect from non-connected peer is no-op
  let peer' = peerDisconnect (peerIdFromAddr (testAddr 1111)) now peer
  assertEqual "disconnect non-existing" 0 (peerCount peer')

  -- Connect then disconnect
  let peer1 = peerConnect (peerIdFromAddr (testAddr 2222)) now peer
  let peer2 = peerDisconnect (peerIdFromAddr (testAddr 2222)) now peer1
  -- Disconnect removes from pending (no actual connection yet)
  assertEqual "no connections after disconnect" 0 (peerCount peer2)
  putStrLn "  PASS: Peer disconnect"

testPeerConnectionTimeout :: IO ()
testPeerConnectionTimeout = do
  putStrLn "Peer connection timeout:"
  sock <- newTestUdpSocket
  let addr = testAddr 6666
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer0 = newPeerState sock addr config now

  -- Initiate connection
  let peer1 = peerConnect (peerIdFromAddr (testAddr 3333)) now peer0

  -- Process far in the future (past timeout)
  let futureTime = 30000000000 :: MonoTime -- 30 seconds
  let result = peerProcess futureTime [] peer1
  let timeoutEvents = filter isDisconnectTimeout (prEvents result)

  assertEqual "timeout fires" True (not (null timeoutEvents))
  putStrLn "  PASS: Connection timeout"
  where
    isDisconnectTimeout (PeerDisconnected _ ReasonTimeout) = True
    isDisconnectTimeout _ = False

testPeerMaxClients :: IO ()
testPeerMaxClients = do
  putStrLn "Peer max clients:"
  sock <- newTestUdpSocket
  let addr = testAddr 4444
      config = defaultNetworkConfig {ncMaxClients = 2}
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Outbound connections aren't capped by maxClients (only inbound)
  let peer1 = peerConnect (peerIdFromAddr (testAddr 1001)) now peer
  let peer2 = peerConnect (peerIdFromAddr (testAddr 1002)) now peer1
  let peer3 = peerConnect (peerIdFromAddr (testAddr 1003)) now peer2

  -- All are pending, none are "connected"
  assertEqual "no connected yet" 0 (peerCount peer3)

  -- Process to drain send queue and verify packets generated
  let result = peerProcess now [] peer3
  assertEqual "sends connection requests" True (not (null (prOutgoing result)))
  putStrLn "  PASS: Max clients config"

-- --------------------------------------------------------------------
-- Full TestNet polymorphic lifecycle
-- --------------------------------------------------------------------

-- | Run peerTick for a peer inside TestWorld, returning events and updated world.
tickPeerInWorld ::
  SockAddr ->
  [(Word8, BS.ByteString)] ->
  NetPeer ->
  TestWorld ->
  (([PeerEvent], NetPeer), TestWorld)
tickPeerInWorld addr msgs peer world =
  runPeerInWorld addr (peerTick msgs peer) world

-- | Advance world time by a step in milliseconds.
stepWorld :: MonoTime -> TestWorld -> TestWorld
stepWorld ms world = worldAdvanceTime (twGlobalTime world + ms * 1000000) world

-- | Register both peers in the world at the given start time.
-- Both addresses must be registered so deliverPackets can route between them.
initWorld :: MonoTime -> SockAddr -> SockAddr -> TestWorld
initWorld startTime addr1 addr2 =
  let w0 = worldAdvanceTime startTime newTestWorld
      (_, w1) = runPeerInWorld addr1 (pure ()) w0
      (_, w2) = runPeerInWorld addr2 (pure ()) w1
   in w2

testTestNetHandshake :: IO ()
testTestNetHandshake = do
  putStrLn "TestNet full handshake:"
  sock <- newTestUdpSocket
  let serverAddr = testAddr 7000
      clientAddr = testAddr 8000
      config = defaultNetworkConfig
      -- Use different RNG seeds (via init time) to avoid salt collision
      startTime = 1000000000 :: MonoTime -- 1 second

  let serverPeer = newPeerState sock serverAddr config 100000000
      clientPeer0 = newPeerState sock clientAddr config 200000000

  -- Pre-register both addresses and set world to start time
  let world0 = initWorld startTime serverAddr clientAddr

  -- Client initiates connection
  let clientPeer1 = peerConnect (peerIdFromAddr serverAddr) startTime clientPeer0

  -- Tick client: sends ConnectionRequest
  let ((_, clientPeer2), world1) =
        tickPeerInWorld clientAddr [] clientPeer1 world0

  -- Deliver packets (client -> server)
  let world2 = stepWorld 10 world1

  -- Tick server: receives ConnectionRequest, sends Challenge
  let ((_, serverPeer1), world3) =
        tickPeerInWorld serverAddr [] serverPeer world2

  -- Deliver packets (server -> client)
  let world4 = stepWorld 10 world3

  -- Tick client: receives Challenge, sends Response
  let ((_, clientPeer3), world5) =
        tickPeerInWorld clientAddr [] clientPeer2 world4

  -- Deliver packets
  let world6 = stepWorld 10 world5

  -- Tick server: receives Response, accepts connection, sends Accepted
  let ((serverEvents2, serverPeer2), world7) =
        tickPeerInWorld serverAddr [] serverPeer1 world6

  -- Check server got a PeerConnected event
  let serverConnected = any isConnected serverEvents2
  assertEqual "server sees connection" True serverConnected

  -- Deliver packets
  let world8 = stepWorld 10 world7

  -- Tick client: receives Accepted
  let ((clientEvents3, clientPeer4), _world9) =
        tickPeerInWorld clientAddr [] clientPeer3 world8

  -- Check client got a PeerConnected event
  let clientConnected = any isConnected clientEvents3
  assertEqual "client sees connection" True clientConnected

  -- Both sides should now have 1 connection
  assertEqual "server has 1 connection" 1 (peerCount serverPeer2)
  assertEqual "client has 1 connection" 1 (peerCount clientPeer4)

  putStrLn "  PASS: Full handshake via TestNet"
  where
    isConnected (PeerConnected _ _) = True
    isConnected _ = False

testTestNetMessageRoundTrip :: IO ()
testTestNetMessageRoundTrip = do
  putStrLn "TestNet message round-trip:"
  sock <- newTestUdpSocket
  let serverAddr = testAddr 7001
      clientAddr = testAddr 8001
      config = defaultNetworkConfig
      startTime = 1000000000 :: MonoTime

  -- Different init times for different RNG seeds
  let serverPeer = newPeerState sock serverAddr config 100000000
      clientPeer0 = newPeerState sock clientAddr config 200000000
      clientPeer1 = peerConnect (peerIdFromAddr serverAddr) startTime clientPeer0

  -- Pre-register both addresses at start time
  let world0 = initWorld startTime serverAddr clientAddr

  -- Tick 1: client sends request
  let ((_, cp2), w1) = tickPeerInWorld clientAddr [] clientPeer1 world0
  let w2 = stepWorld 10 w1

  -- Tick 2: server sends challenge
  let ((_, sp1), w3) = tickPeerInWorld serverAddr [] serverPeer w2
  let w4 = stepWorld 10 w3

  -- Tick 3: client sends response
  let ((_, cp3), w5) = tickPeerInWorld clientAddr [] cp2 w4
  let w6 = stepWorld 10 w5

  -- Tick 4: server accepts
  let ((_, sp2), w7) = tickPeerInWorld serverAddr [] sp1 w6
  let w8 = stepWorld 10 w7

  -- Tick 5: client receives accepted
  let ((_, cp4), w9) = tickPeerInWorld clientAddr [] cp3 w8
  let w10 = stepWorld 10 w9

  -- Now both are connected. Client sends a message on channel 0.
  let testMsg = "hello from client"
  let ((_, _cp5), w11) = tickPeerInWorld clientAddr [(0, testMsg)] cp4 w10
  let w12 = stepWorld 10 w11

  -- Server receives the message
  let ((serverEvents, _sp3), _w13) = tickPeerInWorld serverAddr [] sp2 w12

  -- Note: TestNet doesn't strip CRC (unlike the IO backend), so received
  -- messages contain a 4-byte CRC suffix. We check the message is a prefix.
  let receivedMsgs = [msg | PeerMessage _ _ msg <- serverEvents]
  let hasMsg = any (BS.isPrefixOf testMsg) receivedMsgs
  assertEqual "server received message" True hasMsg

  putStrLn "  PASS: Message round-trip via TestNet"

-- --------------------------------------------------------------------
-- Replication: Interest management
-- --------------------------------------------------------------------

testRadiusInterestRelevant :: IO ()
testRadiusInterestRelevant = do
  putStrLn "Radius interest relevance:"
  let interest = newRadiusInterest 100.0
  assertEqual "close is relevant" True (relevant interest (10.0, 10.0, 0.0) (0.0, 0.0, 0.0))
  assertEqual "far is not relevant" False (relevant interest (200.0, 0.0, 0.0) (0.0, 0.0, 0.0))
  assertEqual "exactly at boundary" True (relevant interest (100.0, 0.0, 0.0) (0.0, 0.0, 0.0))
  assertEqual "same position" True (relevant interest (50.0, 50.0, 50.0) (50.0, 50.0, 50.0))

testRadiusInterestPriority :: IO ()
testRadiusInterestPriority = do
  putStrLn "Radius interest priority:"
  let interest = newRadiusInterest 100.0
  let closePri = priorityMod interest (10.0, 0.0, 0.0) (0.0, 0.0, 0.0)
  let farPri = priorityMod interest (90.0, 0.0, 0.0) (0.0, 0.0, 0.0)
  let outPri = priorityMod interest (200.0, 0.0, 0.0) (0.0, 0.0, 0.0)
  assertEqual "close > far priority" True (closePri > farPri)
  assertEqual "close priority > 0" True (closePri > 0.0)
  assertEqual "far priority > 0" True (farPri > 0.0)
  assertEqual "out of range = 0" True (outPri == 0.0)

testGridInterestRelevant :: IO ()
testGridInterestRelevant = do
  putStrLn "Grid interest relevance:"
  let interest = newGridInterest 100.0
  -- Same cell
  assertEqual "same cell" True (relevant interest (50.0, 50.0, 0.0) (80.0, 80.0, 0.0))
  -- Neighbor cell
  assertEqual "neighbor cell" True (relevant interest (150.0, 50.0, 0.0) (50.0, 50.0, 0.0))
  -- Far cell (more than 1 cell apart)
  assertEqual "far cell" False (relevant interest (350.0, 50.0, 0.0) (50.0, 50.0, 0.0))

-- --------------------------------------------------------------------
-- Replication: Priority accumulator
-- --------------------------------------------------------------------

testPriorityAccumulate :: IO ()
testPriorityAccumulate = do
  putStrLn "Priority accumulator:"
  let acc0 = register ("a" :: String) 10.0
           $ register "b" 5.0
             newPriorityAccumulator
  assertEqual "2 entities" 2 (priorityCount acc0)
  assertEqual "initial priority" (Just 0.0) (getPriority "a" acc0)

  -- Accumulate 0.1s
  let acc1 = accumulate 0.1 acc0
  assertEqual "a = 1.0" True (withinEpsilon 1.0 (getPriority "a" acc1))
  assertEqual "b = 0.5" True (withinEpsilon 0.5 (getPriority "b" acc1))
  where
    withinEpsilon expected (Just actual) = abs (actual - expected) < 0.001
    withinEpsilon _ Nothing = False

testPriorityDrain :: IO ()
testPriorityDrain = do
  putStrLn "Priority drain:"
  let acc0 = accumulate 1.0
           $ register ("high" :: String) 20.0
           $ register "low" 1.0
             newPriorityAccumulator
  -- High = 20.0, Low = 1.0
  -- Budget fits one entity at 100 bytes each
  let (selected, acc1) = drainTop 100 (const 100) acc0
  assertEqual "high selected first" ["high"] selected
  -- High priority should be reset
  assertEqual "high reset to 0" (Just 0.0) (getPriority "high" acc1)
  -- Low should still have accumulated priority
  assertEqual "low still has priority" True (getPriority "low" acc1 > Just 0.0)

testPriorityUnregister :: IO ()
testPriorityUnregister = do
  putStrLn "Priority unregister:"
  let acc0 = register ("x" :: String) 5.0 newPriorityAccumulator
  assertEqual "has x" True (not (priorityIsEmpty acc0))
  let acc1 = unregister "x" acc0
  assertEqual "empty after unregister" True (priorityIsEmpty acc1)

-- --------------------------------------------------------------------
-- Replication: Snapshot interpolation
-- --------------------------------------------------------------------

testSnapshotPushAndReady :: IO ()
testSnapshotPushAndReady = do
  putStrLn "Snapshot push and ready:"
  let buf0 = newSnapshotBuffer :: SnapshotBuffer Float
  assertEqual "empty not ready" False (snapshotReady buf0)
  assertEqual "empty count" 0 (snapshotCount buf0)

  let buf1 = pushSnapshot 0.0 1.0 buf0
  let buf2 = pushSnapshot 50.0 2.0 buf1
  let buf3 = pushSnapshot 100.0 3.0 buf2
  assertEqual "3 snapshots ready" True (snapshotReady buf3)
  assertEqual "count = 3" 3 (snapshotCount buf3)

testSnapshotInterpolation :: IO ()
testSnapshotInterpolation = do
  putStrLn "Snapshot interpolation:"
  let buf0 = newSnapshotBufferWithConfig 2 100.0 :: SnapshotBuffer Float
  let buf1 = pushSnapshot 200.0 20.0
           $ pushSnapshot 100.0 10.0
           $ pushSnapshot 0.0 0.0
             buf0

  -- At render time 250, target = 250 - 100 = 150
  -- Interpolate between t=100 (10.0) and t=200 (20.0), t=0.5
  case sampleSnapshot 250.0 buf1 of
    Nothing -> error "  FAIL: should have interpolated"
    Just val -> do
      let expected = 15.0 :: Float
      assertEqual "interpolated 15.0" True (abs (val - expected) < 0.01)

testSnapshotOutOfOrder :: IO ()
testSnapshotOutOfOrder = do
  putStrLn "Snapshot out-of-order rejection:"
  let buf0 = newSnapshotBuffer :: SnapshotBuffer Float
  let buf1 = pushSnapshot 100.0 1.0 buf0
  let buf2 = pushSnapshot 50.0 2.0 buf1 -- Out of order, should be dropped
  assertEqual "out-of-order dropped" 1 (snapshotCount buf2)
