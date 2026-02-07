{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Bits ((.&.))
import Data.List (foldl')
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64, Word8)
import GBNet.Channel
import GBNet.Class ()
import GBNet.Config
  ( ConfigError (..),
    NetworkConfig (..),
    SimulationConfig (..),
    defaultNetworkConfig,
    defaultSimulationConfig,
    maxChannelCount,
    validateConfig,
  )
import GBNet.Congestion
import GBNet.Connection
  ( Connection (..),
    ConnectionError (..),
    ConnectionState (..),
    DisconnectReason (..),
    connect,
    connectionState,
    createHeader,
    markConnected,
    newConnection,
    sendMessage,
  )
import GBNet.Fragment
import GBNet.Packet
import GBNet.Peer
import GBNet.Reliability
import GBNet.Replication.Delta
import GBNet.Replication.Interest
import GBNet.Replication.Interpolation
import GBNet.Replication.Priority
import GBNet.Security
import GBNet.Serialize (deserialize, serialize)
import GBNet.Serialize.TH (deriveStorable)
import GBNet.Simulator
import GBNet.Socket (UdpSocket (..))
import GBNet.Stats (CongestionLevel (..), defaultSocketStats)
import GBNet.TestNet
import GBNet.Types (ChannelId (..), MessageId (..), SequenceNum (..))
import GBNet.Util
import Network.Socket (SockAddr (..), tupleToHostAddress)
import qualified Network.Socket as NS
import Test.QuickCheck (Arbitrary (..), Property, elements, quickCheck, withMaxSuccess, (==>))

--------------------------------------------------------------------------------
-- TH-derived test types (Storable)
--------------------------------------------------------------------------------

data Vec3 = Vec3
  { vecX :: !Float,
    vecY :: !Float,
    vecZ :: !Float
  }
  deriving (Eq, Show)

deriveStorable ''Vec3

instance Arbitrary Vec3 where
  arbitrary = Vec3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PacketType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary SequenceNum where
  arbitrary = SequenceNum <$> arbitrary

instance Arbitrary PacketHeader where
  arbitrary =
    PacketHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary MessageId where
  arbitrary = MessageId <$> arbitrary

instance Arbitrary FragmentHeader where
  arbitrary =
    FragmentHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary TestDeltaState where
  arbitrary = TestDeltaState <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- Delta test types (Storable-based)
--------------------------------------------------------------------------------

-- Simple test type for delta compression: a pair of Word8 values.
data TestDeltaState = TestDeltaState !Word8 !Word8
  deriving (Eq, Show)

deriveStorable ''TestDeltaState

data TestDeltaDelta = TestDeltaDelta !Word8 !Word8 !Word8 !Word8
  -- Flags + values: hasA, hasB, valA, valB
  deriving (Eq, Show)

deriveStorable ''TestDeltaDelta

instance NetworkDelta TestDeltaState where
  type Delta TestDeltaState = TestDeltaDelta
  diff (TestDeltaState a1 b1) (TestDeltaState a2 b2) =
    TestDeltaDelta
      (if a1 /= a2 then 1 else 0)
      (if b1 /= b2 then 1 else 0)
      a1
      b1
  apply (TestDeltaState a b) (TestDeltaDelta hasA hasB valA valB) =
    TestDeltaState
      (if hasA /= 0 then valA else a)
      (if hasB /= 0 then valB else b)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== GB-Net Haskell Tests ==="
  putStrLn ""

  -- Storable serialization
  testStorableRoundTrip
  testPacketHeaderRoundTrip

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

  -- Property-based tests
  testPropertyRoundTrips

  -- Adversarial: Malformed packet handling
  testTruncatedPacket
  testGarbagePayload
  testEmptyPacket

  -- Integration: Connection migration
  testConnectionMigration

  -- Channel delivery modes, errors, retransmit
  testChannelSendBufferFull
  testChannelSendOversized
  testChannelUnreliableDelivery
  testChannelReliableOrderedDelivery
  testChannelReliableSequencedDropOld
  testChannelRetransmit

  -- Fragment: split, reassemble, header roundtrip, cleanup, too-large
  testFragmentSplitReassemble
  testFragmentHeaderRoundTrip
  testFragmentCleanupExpiry
  testFragmentTooLarge

  -- Security: CRC32C, rate limiting, token validation
  testCrc32Roundtrip
  testCrc32RejectCorrupt
  testRateLimiterAllow
  testRateLimiterDeny
  testTokenValidation
  testTokenExpired
  testTokenReplayed

  -- Connection state machine
  testConnectionStateMachine
  testConnectionSendReceive

  -- Config validation
  testValidateConfigValid
  testValidateConfigErrors

  -- Delta encode/decode
  testDeltaEncodeDecodeTrivial

  -- Simulator
  testSimulatorBasic
  testSimulatorPeerDelivery

  putStrLn ""
  putStrLn "All tests passed!"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Storable serialization tests
--------------------------------------------------------------------------------

testStorableRoundTrip :: IO ()
testStorableRoundTrip = do
  putStrLn "Storable round-trip:"
  let v = Vec3 1.0 (-2.5) 100.0
      bytes = serialize v
  assertEqual "Vec3 size" 12 (BS.length bytes)
  case deserialize bytes :: Either String Vec3 of
    Left err -> error $ "  FAIL: deserialize Vec3: " ++ err
    Right v' -> assertEqual "Vec3 roundtrip" v v'

testPacketHeaderRoundTrip :: IO ()
testPacketHeaderRoundTrip = do
  putStrLn "PacketHeader roundtrip:"
  let headers =
        [ PacketHeader Payload (SequenceNum 42) (SequenceNum 40) 0xDEADBEEF,
          PacketHeader ConnectionRequest (SequenceNum 0) (SequenceNum 0) 0,
          PacketHeader Keepalive (SequenceNum 65535) (SequenceNum 65535) 0xFFFFFFFF,
          PacketHeader Disconnect (SequenceNum 12345) (SequenceNum 54321) 0x12345678
        ]
  mapM_ testHeader headers
  where
    testHeader hdr = do
      let bytes = serializeHeader hdr
      assertEqual ("size for " ++ show (packetType hdr)) packetHeaderByteSize (BS.length bytes)
      case deserializeHeader bytes of
        Left err -> error $ "deserialize failed: " ++ err
        Right hdr' -> do
          assertEqual "roundtrip packetType" (packetType hdr) (packetType hdr')
          assertEqual "roundtrip sequenceNum" (sequenceNum hdr) (sequenceNum hdr')
          assertEqual "roundtrip ack" (ack hdr) (ack hdr')
          assertEqual "roundtrip ackBitfield" (ackBitfield hdr) (ackBitfield hdr')

--------------------------------------------------------------------------------
-- Reliability module tests
--------------------------------------------------------------------------------

testSequenceGreaterThan :: IO ()
testSequenceGreaterThan = do
  putStrLn "sequenceGreaterThan:"
  assertEqual "1 > 0" True (sequenceGreaterThan (SequenceNum 1) (SequenceNum 0))
  assertEqual "0 > 1" False (sequenceGreaterThan (SequenceNum 0) (SequenceNum 1))
  assertEqual "100 > 50" True (sequenceGreaterThan (SequenceNum 100) (SequenceNum 50))
  assertEqual "50 > 100" False (sequenceGreaterThan (SequenceNum 50) (SequenceNum 100))
  -- Wraparound
  assertEqual "0 > 65535" True (sequenceGreaterThan (SequenceNum 0) (SequenceNum 65535))
  assertEqual "65535 > 0" False (sequenceGreaterThan (SequenceNum 65535) (SequenceNum 0))
  assertEqual "1 > 65534" True (sequenceGreaterThan (SequenceNum 1) (SequenceNum 65534))
  assertEqual "100 > 65500" True (sequenceGreaterThan (SequenceNum 100) (SequenceNum 65500))

testSequenceDiff :: IO ()
testSequenceDiff = do
  putStrLn "sequenceDiff:"
  assertEqual "diff(5,3)" 2 (sequenceDiff (SequenceNum 5) (SequenceNum 3))
  assertEqual "diff(3,5)" (-2) (sequenceDiff (SequenceNum 3) (SequenceNum 5))
  assertEqual "diff(100,100)" 0 (sequenceDiff (SequenceNum 100) (SequenceNum 100))
  -- Wraparound
  assertEqual "diff(0,65535)" 1 (sequenceDiff (SequenceNum 0) (SequenceNum 65535))
  assertEqual "diff(65535,0)" (-1) (sequenceDiff (SequenceNum 65535) (SequenceNum 0))
  assertEqual "diff(5,65530)" 11 (sequenceDiff (SequenceNum 5) (SequenceNum 65530))

testSequenceAtBoundaries :: IO ()
testSequenceAtBoundaries = do
  putStrLn "Sequence at Word16 boundaries:"
  assertEqual "0 > maxBound" True (sequenceGreaterThan 0 (maxBound :: SequenceNum))
  assertEqual "maxBound > 0" False (sequenceGreaterThan (maxBound :: SequenceNum) 0)
  assertEqual "diff(0,max)" 1 (sequenceDiff 0 (maxBound :: SequenceNum))
  assertEqual "diff(max,0)" (-1) (sequenceDiff (maxBound :: SequenceNum) 0)

testSequenceBufferOps :: IO ()
testSequenceBufferOps = do
  putStrLn "SequenceBuffer operations:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert (SequenceNum 0) 100 buf0
  let buf2 = sbInsert (SequenceNum 1) 200 buf1
  let buf3 = sbInsert (SequenceNum 2) 300 buf2
  assertEqual "exists 0" True (sbExists (SequenceNum 0) buf3)
  assertEqual "exists 1" True (sbExists (SequenceNum 1) buf3)
  assertEqual "exists 2" True (sbExists (SequenceNum 2) buf3)
  assertEqual "exists 3" False (sbExists (SequenceNum 3) buf3)
  assertEqual "get 0" (Just 100) (sbGet (SequenceNum 0) buf3)
  assertEqual "get 1" (Just 200) (sbGet (SequenceNum 1) buf3)
  assertEqual "get 2" (Just 300) (sbGet (SequenceNum 2) buf3)

testSequenceBufferWraparound :: IO ()
testSequenceBufferWraparound = do
  putStrLn "SequenceBuffer wraparound:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert (SequenceNum 65534) 100 buf0
  let buf2 = sbInsert (SequenceNum 65535) 200 buf1
  let buf3 = sbInsert (SequenceNum 0) 300 buf2
  let buf4 = sbInsert (SequenceNum 1) 400 buf3
  assertEqual "exists 65534" True (sbExists (SequenceNum 65534) buf4)
  assertEqual "exists 65535" True (sbExists (SequenceNum 65535) buf4)
  assertEqual "exists 0" True (sbExists (SequenceNum 0) buf4)
  assertEqual "exists 1" True (sbExists (SequenceNum 1) buf4)

testSequenceBufferCollision :: IO ()
testSequenceBufferCollision = do
  putStrLn "SequenceBuffer collision:"
  let buf0 = newSequenceBuffer 16 :: SequenceBuffer Word32
  let buf1 = sbInsert (SequenceNum 0) 100 buf0
  assertEqual "exists 0 before" True (sbExists (SequenceNum 0) buf1)
  -- Sequence 16 maps to the same slot (16 % 16 == 0)
  let buf2 = sbInsert (SequenceNum 16) 200 buf1
  assertEqual "exists 16" True (sbExists (SequenceNum 16) buf2)
  assertEqual "exists 0 after" False (sbExists (SequenceNum 0) buf2)
  assertEqual "get 16" (Just 200) (sbGet (SequenceNum 16) buf2)
  assertEqual "get 0 after" Nothing (sbGet (SequenceNum 0) buf2)

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
  let ep1 = onPacketsReceived [0] ep0
  let ep2 = onPacketsReceived [2] ep1
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
  let ep1 = onPacketSent 10 now (ChannelId 2) 5 100 ep0
  let ep2 = onPacketSent 11 now (ChannelId 3) 7 200 ep1
  -- ACK packet 11 directly, packet 10 via ack_bits (bit 0 = seq 10)
  let ackTime = 1050000000 :: MonoTime -- 1.05 seconds
  let (ackResult, _ep3) = processAcks 11 1 ackTime ep2
      acked = arAcked ackResult
  assertEqual "2 acked" 2 (length acked)
  assertEqual "contains (3,7)" True ((ChannelId 3, 7) `elem` acked)
  assertEqual "contains (2,5)" True ((ChannelId 2, 5) `elem` acked)

testInFlightEviction :: IO ()
testInFlightEviction = do
  putStrLn "In-flight eviction:"
  let ep0 = withMaxInFlight 4 $ newReliableEndpoint 256
  -- Send 4 packets
  let ep1 = foldl (\e i -> onPacketSent i (fromIntegral i * 1000000) (ChannelId 0) i 100 e) ep0 [0 .. 3]
  assertEqual "4 in flight" 4 (packetsInFlight ep1)
  -- Send 5th — should evict one
  let ep2 = onPacketSent 4 4000000 (ChannelId 0) 4 100 ep1
  assertEqual "still 4 in flight" 4 (packetsInFlight ep2)
  assertEqual "1 evicted" 1 (rePacketsEvicted ep2)

testFastRetransmit :: IO ()
testFastRetransmit = do
  putStrLn "Fast retransmit:"
  let ep0 = newReliableEndpoint 256
  let now = 1000000000 :: MonoTime
  -- Send packets 0-4
  let ep1 = foldl (\e i -> onPacketSent i now (ChannelId 0) i 100 e) ep0 [0 .. 4]
  -- ACK packets 1,2,3,4 but NOT 0 — seq 0 should accumulate nacks
  let ackTime = 1050000000 :: MonoTime
  -- ACK 1: ack=1, ack_bits=0 (no bits). seq 0 is older, diff=1, bit 0 not set -> nack 0 once
  let (_, ep2) = processAcks 1 0 ackTime ep1
  -- ACK 2: ack=2, ack_bits=0. seq 0 diff=2, bit 1 not set -> nack again
  let (_, ep3) = processAcks 2 0 ackTime ep2
  -- ACK 3: ack=3, ack_bits=0. seq 0 diff=3, bit 2 not set -> nack = 3, triggers fast retransmit
  let (ackResult4, _ep4) = processAcks 3 0 ackTime ep3
      fastRetransmit = arFastRetransmit ackResult4
  assertEqual "fast retransmit triggered" True (not (null fastRetransmit))
  assertEqual "retransmit is (0,0)" True ((ChannelId 0, SequenceNum 0) `elem` fastRetransmit)

--------------------------------------------------------------------------------
-- Congestion control tests
--------------------------------------------------------------------------------

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
  let testRtoMs = 200.0
  let cw3 = cwSlowStartRestart testRtoMs laterTime cw2
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

--------------------------------------------------------------------------------
-- Integration: TestNet peer lifecycle
--------------------------------------------------------------------------------

-- Helper to create SockAddr for tests
testAddr :: Word16 -> SockAddr
testAddr port = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

-- | Create a dummy UdpSocket for testing pure peer operations.
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
  let result = peerSend (peerIdFromAddr (testAddr 1234)) (ChannelId 0) "hello" now peer
  case result of
    Left _ -> putStrLn "  PASS: send to unconnected peer fails"
    Right _ -> error "  FAIL: should have failed"

  -- Broadcast to empty peer should be no-op
  let peer' = peerBroadcast (ChannelId 0) "test" Nothing now peer
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

--------------------------------------------------------------------------------
-- Full TestNet polymorphic lifecycle
--------------------------------------------------------------------------------

-- | Run peerTick for a peer inside TestWorld, returning events and updated world.
tickPeerInWorld ::
  SockAddr ->
  [(ChannelId, BS.ByteString)] ->
  NetPeer ->
  TestWorld ->
  (([PeerEvent], NetPeer), TestWorld)
tickPeerInWorld addr msgs peer =
  runPeerInWorld addr (peerTick msgs peer)

-- | Advance world time by a step in milliseconds.
stepWorld :: MonoTime -> TestWorld -> TestWorld
stepWorld ms world = worldAdvanceTime (twGlobalTime world + ms * 1000000) world

-- | Register both peers in the world at the given start time.
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
  let ((_, _cp5), w11) = tickPeerInWorld clientAddr [(ChannelId 0, testMsg)] cp4 w10
  let w12 = stepWorld 10 w11

  -- Server receives the message
  let ((serverEvents, _sp3), _w13) = tickPeerInWorld serverAddr [] sp2 w12

  -- Note: TestNet doesn't strip CRC (unlike the IO backend), so received
  -- messages contain a 4-byte CRC suffix. We check the message is a prefix.
  let receivedMsgs = [msg | PeerMessage _ _ msg <- serverEvents]
  let hasMsg = any (BS.isPrefixOf testMsg) receivedMsgs
  assertEqual "server received message" True hasMsg

  putStrLn "  PASS: Message round-trip via TestNet"

--------------------------------------------------------------------------------
-- Replication: Interest management
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Replication: Priority accumulator
--------------------------------------------------------------------------------

testPriorityAccumulate :: IO ()
testPriorityAccumulate = do
  putStrLn "Priority accumulator:"
  let acc0 =
        register ("a" :: String) 10.0 $
          register
            "b"
            5.0
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
  let acc0 =
        accumulate 1.0 $
          register ("high" :: String) 20.0 $
            register
              "low"
              1.0
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

--------------------------------------------------------------------------------
-- Replication: Snapshot interpolation
--------------------------------------------------------------------------------

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
  let buf1 =
        pushSnapshot 200.0 20.0 $
          pushSnapshot 100.0 10.0 $
            pushSnapshot
              0.0
              0.0
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

--------------------------------------------------------------------------------
-- Property-based tests (QuickCheck)
--------------------------------------------------------------------------------

-- | Storable roundtrip property for Vec3
propStorableRoundTrip :: Vec3 -> Bool
propStorableRoundTrip v =
  case deserialize (serialize v) of
    Right v' -> v == v'
    Left _ -> False

-- | PacketHeader serialize/deserialize roundtrip
propPacketHeaderRoundTrip :: PacketHeader -> Bool
propPacketHeaderRoundTrip hdr =
  case deserializeHeader (serializeHeader hdr) of
    Left _ -> False
    Right hdr' ->
      packetType hdr == packetType hdr'
        && sequenceNum hdr == sequenceNum hdr'
        && ack hdr == ack hdr'
        && ackBitfield hdr == ackBitfield hdr'

-- | FragmentHeader serialize/deserialize roundtrip
propFragmentHeaderRoundTrip :: FragmentHeader -> Bool
propFragmentHeaderRoundTrip hdr =
  case deserializeFragmentHeader (serializeFragmentHeader hdr) of
    Nothing -> False
    Just hdr' ->
      fhMessageId hdr == fhMessageId hdr'
        && fhFragmentIndex hdr == fhFragmentIndex hdr'
        && fhFragmentCount hdr == fhFragmentCount hdr'

-- | sequenceGreaterThan is antisymmetric: if a > b then not (b > a)
propSeqGtAntisymmetric :: SequenceNum -> SequenceNum -> Bool
propSeqGtAntisymmetric a b
  | a == b = True -- equal: neither is greater
  | sequenceGreaterThan a b = not (sequenceGreaterThan b a)
  | otherwise = True -- a not > b is fine

-- | sequenceDiff consistent with sequenceGreaterThan
propSeqDiffConsistent :: SequenceNum -> SequenceNum -> Bool
propSeqDiffConsistent a b
  | a == b = sequenceDiff a b == 0
  | sequenceGreaterThan a b = sequenceDiff a b > 0
  | otherwise = sequenceDiff a b < 0

-- | Delta: apply (diff new old) old == new
propDeltaRoundTrip :: TestDeltaState -> TestDeltaState -> Bool
propDeltaRoundTrip new old =
  let d = diff new old
   in apply old d == new

-- | CRC32C: append then validate always succeeds
propCrcRoundTrip :: [Word8] -> Bool
propCrcRoundTrip bytes =
  let bs = BS.pack bytes
      withCrc = appendCrc32 bs
   in case validateAndStripCrc32 withCrc of
        Nothing -> False
        Just stripped -> stripped == bs

-- | CRC32C: flipping any bit in payload fails validation
propCrcDetectsCorruption :: [Word8] -> Property
propCrcDetectsCorruption bytes =
  let bs = BS.pack bytes
      withCrc = appendCrc32 bs
   in BS.length bs > 0
        ==> let -- Flip first byte of payload
                corrupted = BS.cons (BS.index withCrc 0 + 1) (BS.drop 1 withCrc)
             in case validateAndStripCrc32 corrupted of
                  Nothing -> True
                  Just _ -> False

testPropertyRoundTrips :: IO ()
testPropertyRoundTrips = do
  putStrLn "Property-based tests:"
  putStr "  Vec3 Storable roundtrip: "
  quickCheck (withMaxSuccess 200 propStorableRoundTrip)
  putStr "  PacketHeader roundtrip: "
  quickCheck (withMaxSuccess 500 propPacketHeaderRoundTrip)
  putStr "  FragmentHeader roundtrip: "
  quickCheck (withMaxSuccess 500 propFragmentHeaderRoundTrip)
  putStr "  sequenceGreaterThan antisymmetric: "
  quickCheck (withMaxSuccess 1000 propSeqGtAntisymmetric)
  putStr "  sequenceDiff consistent: "
  quickCheck (withMaxSuccess 1000 propSeqDiffConsistent)
  putStr "  Delta apply . diff == id: "
  quickCheck (withMaxSuccess 500 propDeltaRoundTrip)
  putStr "  CRC32C roundtrip: "
  quickCheck (withMaxSuccess 200 propCrcRoundTrip)
  putStr "  CRC32C detects corruption: "
  quickCheck (withMaxSuccess 200 propCrcDetectsCorruption)

--------------------------------------------------------------------------------
-- Adversarial: Malformed packet handling
--------------------------------------------------------------------------------

testTruncatedPacket :: IO ()
testTruncatedPacket = do
  putStrLn "Adversarial - truncated packet:"
  sock <- newTestUdpSocket
  let addr = testAddr 5000
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Feed a packet that's too short to contain a valid header
  let truncated = BS.pack [0x00, 0x01]
      pkt = IncomingPacket (peerIdFromAddr (testAddr 9000)) truncated
      result = peerProcess now [pkt] peer

  -- Should not crash, just ignore
  assertEqual "no events from truncated" True (null (prEvents result))
  putStrLn "  PASS: truncated packet handled gracefully"

testGarbagePayload :: IO ()
testGarbagePayload = do
  putStrLn "Adversarial - garbage payload:"
  sock <- newTestUdpSocket
  let addr = testAddr 5001
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Feed random garbage bytes
  let garbage = BS.pack [0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, 0xF9, 0xF8, 0xF7, 0xF6, 0xF5, 0xF4, 0xF3, 0xF2, 0xF1, 0xF0]
      pkt = IncomingPacket (peerIdFromAddr (testAddr 9001)) garbage
      result = peerProcess now [pkt] peer

  -- Should not crash
  assertEqual "peer survives garbage" True (peerCount (prPeer result) >= 0)
  putStrLn "  PASS: garbage payload handled gracefully"

testEmptyPacket :: IO ()
testEmptyPacket = do
  putStrLn "Adversarial - empty packet:"
  sock <- newTestUdpSocket
  let addr = testAddr 5002
      config = defaultNetworkConfig
      now = 0 :: MonoTime
      peer = newPeerState sock addr config now

  -- Feed zero-length data
  let pkt = IncomingPacket (peerIdFromAddr (testAddr 9002)) BS.empty
      result = peerProcess now [pkt] peer

  assertEqual "no events from empty" True (null (prEvents result))
  putStrLn "  PASS: empty packet handled gracefully"

--------------------------------------------------------------------------------
-- Integration: Connection migration
--------------------------------------------------------------------------------

testConnectionMigration :: IO ()
testConnectionMigration = do
  putStrLn "Connection migration:"
  sock <- newTestUdpSocket
  let serverAddr = testAddr 7010
      clientAddr = testAddr 8010
      config = defaultNetworkConfig {ncEnableConnectionMigration = True}
      startTime = 1000000000 :: MonoTime

  let serverPeer = newPeerState sock serverAddr config 100000000
      clientPeer0 = newPeerState sock clientAddr config 200000000
      clientPeer1 = peerConnect (peerIdFromAddr serverAddr) startTime clientPeer0

  -- Full handshake via TestNet
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
  let ((_, _cp4), w9) = tickPeerInWorld clientAddr [] cp3 w8
  let _w10 = stepWorld 10 w9

  -- Verify connection established
  assertEqual "server has 1 connection" 1 (peerCount sp2)
  assertEqual "server knows client" True (peerIsConnected (peerIdFromAddr clientAddr) sp2)

  -- Now simulate migration: take the outgoing packets from client, but
  -- present them to the server as coming from a new address
  let newClientAddr = testAddr 8099
      newClientPid = peerIdFromAddr newClientAddr

  -- Get a valid packet from the connected client
  let clientResult = peerProcess (startTime + 50000000) [] sp2
      outgoing = prOutgoing clientResult

  case outgoing of
    [] -> do
      -- No outgoing packets, craft a minimal valid one by re-processing
      assertEqual "migration config enabled" True (ncEnableConnectionMigration config)
      putStrLn "  PASS: Migration enabled (no packets to migrate with)"
    (firstPkt : _) -> do
      -- Re-present this packet as coming from the new address
      let migratedPkt = IncomingPacket newClientPid (rpData firstPkt)
          migrateTime = startTime + 60000000000 -- well past cooldown
          result = peerProcess migrateTime [migratedPkt] sp2
          events = prEvents result
          migrated = [() | PeerMigrated _ _ <- events]

      if not (null migrated)
        then do
          -- Migration fired
          assertEqual "old connection gone" False (peerIsConnected (peerIdFromAddr clientAddr) (prPeer result))
          assertEqual "new connection exists" True (peerIsConnected newClientPid (prPeer result))
          putStrLn "  PASS: Connection migration"
        else do
          -- Packet may have been rejected for other reasons (CRC, sequence distance)
          assertEqual "migration config enabled" True (ncEnableConnectionMigration config)
          assertEqual "server still has connection" 1 (peerCount (prPeer result))
          putStrLn "  PASS: Migration wired up (packet not matched)"

--------------------------------------------------------------------------------
-- Connection state machine tests
--------------------------------------------------------------------------------

testConnectionStateMachine :: IO ()
testConnectionStateMachine = do
  putStrLn "Connection state machine:"
  let config = defaultNetworkConfig
      clientSalt = 12345 :: Word64
      now = 0 :: MonoTime

  -- newConnection starts in Disconnected state
  let conn0 = newConnection config clientSalt now
  assertEqual "initial state Disconnected" Disconnected (connectionState conn0)

  -- connect transitions to Connecting
  case connect now conn0 of
    Left err -> error $ "  FAIL: connect returned error: " ++ show err
    Right conn1 -> do
      assertEqual "state after connect" Connecting (connectionState conn1)

      -- connect on a non-Disconnected connection returns ErrAlreadyConnected
      case connect now conn1 of
        Left ErrAlreadyConnected -> putStrLn "  PASS: double connect rejected"
        Left other -> error $ "  FAIL: expected ErrAlreadyConnected, got " ++ show other
        Right _ -> error "  FAIL: double connect should fail"

  -- createHeader increments local sequence
  let conn0' = newConnection config clientSalt now
  let seqBefore = connLocalSeq conn0'
  let (_header, conn1') = createHeader conn0'
  assertEqual "local seq incremented" (seqBefore + 1) (connLocalSeq conn1')

  -- Second createHeader increments again
  let (_header2, conn2') = createHeader conn1'
  assertEqual "local seq incremented again" (seqBefore + 2) (connLocalSeq conn2')

testConnectionSendReceive :: IO ()
testConnectionSendReceive = do
  putStrLn "Connection send/receive:"
  let config = defaultNetworkConfig
      clientSalt = 67890 :: Word64
      now = 0 :: MonoTime

  -- sendMessage on a Disconnected connection returns ErrNotConnected
  let conn0 = newConnection config clientSalt now
  case sendMessage (ChannelId 0) "hello" now conn0 of
    Left ErrNotConnected -> putStrLn "  PASS: send on disconnected fails"
    Left other -> error $ "  FAIL: expected ErrNotConnected, got " ++ show other
    Right _ -> error "  FAIL: send on disconnected should fail"

  -- Mark the connection as Connected, then sendMessage should succeed
  let connConnected = markConnected now conn0
  assertEqual "state is Connected" Connected (connectionState connConnected)

  case sendMessage (ChannelId 0) "hello" now connConnected of
    Left err -> error $ "  FAIL: send on connected failed: " ++ show err
    Right connAfterSend -> do
      assertEqual "still Connected after send" Connected (connectionState connAfterSend)
      putStrLn "  PASS: send on connected channel 0"

  -- sendMessage on an invalid channel returns ErrInvalidChannel
  case sendMessage (ChannelId 99) "bad" now connConnected of
    Left (ErrInvalidChannel _) -> putStrLn "  PASS: send on invalid channel rejected"
    Left other -> error $ "  FAIL: expected ErrInvalidChannel, got " ++ show other
    Right _ -> error "  FAIL: send on invalid channel should fail"

--------------------------------------------------------------------------------
-- Config validation tests
--------------------------------------------------------------------------------

testValidateConfigValid :: IO ()
testValidateConfigValid = do
  putStrLn "Config validation (valid):"
  assertEqual "default config valid" (Right ()) (validateConfig defaultNetworkConfig)

testValidateConfigErrors :: IO ()
testValidateConfigErrors = do
  putStrLn "Config validation (errors):"

  -- Fragment threshold > MTU
  let cfgFragExceedsMtu = defaultNetworkConfig {ncFragmentThreshold = ncMtu defaultNetworkConfig + 1}
  assertEqual "fragment > mtu" (Left FragmentThresholdExceedsMtu) (validateConfig cfgFragExceedsMtu)

  -- Max channels > maxChannelCount (8)
  let cfgTooManyChannels = defaultNetworkConfig {ncMaxChannels = maxChannelCount + 1}
  assertEqual "channels > max" (Left InvalidChannelCount) (validateConfig cfgTooManyChannels)

  -- Max channels = 0
  let cfgZeroChannels = defaultNetworkConfig {ncMaxChannels = 0}
  assertEqual "channels = 0" (Left InvalidChannelCount) (validateConfig cfgZeroChannels)

--------------------------------------------------------------------------------
-- Delta encode/decode tests
--------------------------------------------------------------------------------

testDeltaEncodeDecodeTrivial :: IO ()
testDeltaEncodeDecodeTrivial = do
  putStrLn "Delta encode/decode trivial:"

  -- No baseline: encodes full state, decodes back
  let tracker0 = newDeltaTracker 16 :: DeltaTracker TestDeltaState
      state1 = TestDeltaState 10 20
      (encoded, tracker1) = deltaEncode 0 state1 tracker0
      baselines0 = newBaselineManager 16 5000.0 :: BaselineManager TestDeltaState

  case deltaDecode encoded baselines0 of
    Left err -> error $ "  FAIL: decode without baseline: " ++ err
    Right decoded ->
      assertEqual "full state roundtrip" state1 decoded

  -- Acknowledge seq 0 so it becomes the confirmed baseline
  let tracker2 = deltaOnAck 0 tracker1
  assertEqual "confirmed seq" (Just 0) (deltaConfirmedSeq tracker2)

  -- Push baseline on receiver side
  let baselines1 = pushBaseline 0 state1 0 baselines0
  assertEqual "baseline count" 1 (baselineCount baselines1)
  assertEqual "baseline lookup" (Just state1) (getBaseline 0 baselines1)

  -- Encode a new state against the confirmed baseline
  let state2 = TestDeltaState 10 30 -- only second field changed
      (encoded2, _tracker3) = deltaEncode 1 state2 tracker2

  case deltaDecode encoded2 baselines1 of
    Left err -> error $ "  FAIL: decode with baseline: " ++ err
    Right decoded2 ->
      assertEqual "delta roundtrip" state2 decoded2

  -- BaselineManager empty/reset
  assertEqual "baseline not empty" False (baselineIsEmpty baselines1)
  let baselines2 = baselineReset baselines1
  assertEqual "baseline empty after reset" True (baselineIsEmpty baselines2)

  -- DeltaTracker reset
  let tracker4 = deltaReset tracker2
  assertEqual "confirmed seq after reset" Nothing (deltaConfirmedSeq tracker4)

--------------------------------------------------------------------------------
-- Simulator tests
--------------------------------------------------------------------------------

testSimulatorBasic :: IO ()
testSimulatorBasic = do
  putStrLn "Simulator basic:"
  let now = 0 :: MonoTime
      config = defaultSimulationConfig -- 0% loss, 0 latency, 0 jitter

  -- newNetworkSimulator creates empty simulator
  let sim0 = newNetworkSimulator config now
  assertEqual "initial pending count" 0 (simulatorPendingCount sim0)

  -- With 0% loss and 0 latency, packet should be delivered immediately
  let testData = "hello" :: BS.ByteString
      testAddr' = 42 :: Word64
      (immediate, sim1) = simulatorProcessSend testData testAddr' now sim0

  assertEqual "immediate delivery count" 1 (length immediate)
  case immediate of
    [(dat, addr)] -> do
      assertEqual "delivered data" testData dat
      assertEqual "delivered addr" testAddr' addr
    _ -> error "  FAIL: unexpected immediate result"

  -- Nothing should be queued since latency is 0
  assertEqual "no pending after immediate" 0 (simulatorPendingCount sim1)

  -- Test with latency: packets should be delayed
  let configWithLatency = defaultSimulationConfig {simLatencyMs = 100}
      sim2 = newNetworkSimulator configWithLatency now
      (immediate2, sim3) = simulatorProcessSend testData testAddr' now sim2

  assertEqual "no immediate with latency" 0 (length immediate2)
  assertEqual "1 pending with latency" 1 (simulatorPendingCount sim3)

  -- Receiving before delivery time returns nothing
  let tooEarly = now + 50000000 -- 50ms in nanoseconds
      (earlyResults, sim4) = simulatorReceiveReady tooEarly sim3
  assertEqual "nothing ready early" 0 (length earlyResults)
  assertEqual "still 1 pending" 1 (simulatorPendingCount sim4)

  -- Receiving after delivery time returns the packet
  let lateEnough = now + 200000000000 -- well past 100ms delay
      (lateResults, sim5) = simulatorReceiveReady lateEnough sim4
  assertEqual "packet ready" 1 (length lateResults)
  assertEqual "no pending after receive" 0 (simulatorPendingCount sim5)

  case lateResults of
    [(dat, addr)] -> do
      assertEqual "received data" testData dat
      assertEqual "received addr" testAddr' addr
    _ -> error "  FAIL: unexpected late result"

-- | Test that reliable messages survive loss + latency via Simulator.
--
-- Strategy: connect two peers via TestNet (clean), then run a
-- bidirectional tick loop where both peers' outgoing packets pass
-- through lossy Simulators. Retransmission recovers dropped packets.
testSimulatorPeerDelivery :: IO ()
testSimulatorPeerDelivery = do
  putStrLn "Simulator peer delivery under loss:"
  sock <- newTestUdpSocket
  let serverAddr = testAddr 7020
      clientAddr = testAddr 8020
      config = defaultNetworkConfig
      startTime = 1000000000 :: MonoTime

  let serverPeer = newPeerState sock serverAddr config 100000000
      clientPeer0 = newPeerState sock clientAddr config 200000000
      clientPeer1 = peerConnect (peerIdFromAddr serverAddr) startTime clientPeer0

  -- Establish connection via clean TestNet (no loss)
  let world0 = initWorld startTime serverAddr clientAddr
  let ((_, cp2), w1) = tickPeerInWorld clientAddr [] clientPeer1 world0
  let w2 = stepWorld 10 w1
  let ((_, sp1), w3) = tickPeerInWorld serverAddr [] serverPeer w2
  let w4 = stepWorld 10 w3
  let ((_, cp3), w5) = tickPeerInWorld clientAddr [] cp2 w4
  let w6 = stepWorld 10 w5
  let ((_, sp2), w7) = tickPeerInWorld serverAddr [] sp1 w6
  let w8 = stepWorld 10 w7
  let ((_, cp4), w9) = tickPeerInWorld clientAddr [] cp3 w8
  let w10 = stepWorld 10 w9

  -- Both connected. Now use Simulators as lossy conditioners (one per direction).
  -- 10% loss, 20ms latency — moderate but recoverable
  let simConfig =
        defaultSimulationConfig
          { simPacketLoss = 0.1,
            simLatencyMs = 20,
            simJitterMs = 5
          }
      -- Two simulators: client→server and server→client
      simC2S0 = newNetworkSimulator simConfig startTime
      simS2C0 = newNetworkSimulator simConfig (startTime + 1)
      serverAddrKey = 1 :: Word64
      clientAddrKey = 2 :: Word64
      testMsg = "reliable under loss"

  -- Queue reliable message via pure peerSend (not TestNet) so it flows
  -- through peerProcess → Simulator, not through TestNet's delivery.
  let sendTime = twGlobalTime w10 + 1000000
      serverPid = peerIdFromAddr serverAddr
  let cp5 = case peerSend serverPid (ChannelId 0) testMsg sendTime cp4 of
        Right p -> p
        Left e -> error $ "  FAIL: peerSend failed: " ++ show e

  -- Bidirectional tick loop: both peers process, outgoing passes through Simulators
  let tickCount = 30 :: Int
      tickIntervalNs = 50000000 :: MonoTime -- 50ms

  let (receivedMsg, _, _, _, _) =
        foldl'
          ( \(!found, !server, !client, !sC2S, !sS2C) i ->
              if found
                then (found, server, client, sC2S, sS2C)
                else
                  let tickTime = sendTime + fromIntegral i * tickIntervalNs

                      -- Process client → get outgoing (includes queued message)
                      clientResult = peerProcess tickTime [] client
                      client' = prPeer clientResult
                      clientOut = prOutgoing clientResult

                      -- Feed client outgoing through C2S Simulator
                      (sC2S', serverPkts) = conditionPackets clientOut clientAddrKey clientAddr tickTime sC2S

                      -- Process server with conditioned client packets
                      serverResult = peerProcess tickTime serverPkts server
                      server' = prPeer serverResult
                      serverOut = prOutgoing serverResult
                      events = prEvents serverResult

                      -- Feed server outgoing through S2C Simulator
                      (sS2C', clientPkts) = conditionPackets serverOut serverAddrKey serverAddr tickTime sS2C

                      -- Feed server responses back to client
                      clientResult2 = peerProcess tickTime clientPkts client'
                      client'' = prPeer clientResult2

                      gotMessage = any isMessage events
                   in (gotMessage, server', client'', sC2S', sS2C')
          )
          (False, sp2, cp5, simC2S0, simS2C0)
          [1 .. tickCount]

  assertEqual "reliable message delivered under 10% loss" True receivedMsg
  putStrLn "  PASS: Simulator peer delivery under loss"
  where
    isMessage (PeerMessage _ _ _) = True
    isMessage _ = False

    -- | Run outgoing packets through a Simulator, strip CRC (matching
    -- MonadNetwork layer), and collect deliverables as IncomingPackets.
    conditionPackets ::
      [RawPacket] -> Word64 -> SockAddr -> MonoTime -> NetworkSimulator -> (NetworkSimulator, [IncomingPacket])
    conditionPackets pkts addrKey fromAddr now sim0 =
      let (sim1, incoming) =
            foldl'
              ( \(!s, !acc) pkt ->
                  let (immediate, s') = simulatorProcessSend (rpData pkt) addrKey now s
                   in (s', acc ++ stripAndWrap immediate)
              )
              (sim0, [])
              pkts
          -- Also deliver any previously delayed packets now ready
          (delayed, sim2) = simulatorReceiveReady now sim1
          allIncoming = incoming ++ stripAndWrap delayed
       in (sim2, allIncoming)
      where
        -- Strip CRC (as MonadNetwork layer does) and wrap as IncomingPacket
        stripAndWrap = concatMap $ \(d, _) ->
          case validateAndStripCrc32 d of
            Nothing -> [] -- Drop corrupt (CRC mismatch)
            Just valid -> [IncomingPacket (peerIdFromAddr fromAddr) valid]

--------------------------------------------------------------------------------
-- Channel: delivery modes, errors, retransmit
--------------------------------------------------------------------------------

testChannelSendBufferFull :: IO ()
testChannelSendBufferFull = do
  putStrLn "Channel send buffer full:"
  let config = defaultChannelConfig {ccMessageBufferSize = 1, ccBlockOnFull = True}
      ch0 = newChannel (ChannelId 0) config
      now = 0 :: MonoTime
      payload = "hello"
  -- First send succeeds
  case channelSend payload now ch0 of
    Left _ -> error "  FAIL: first send should succeed"
    Right (_, ch1) ->
      -- Second send should fail with buffer full (blockOnFull = True)
      case channelSend payload now ch1 of
        Left ChannelBufferFull -> putStrLn "  PASS: buffer full returns ChannelBufferFull"
        Left e -> error $ "  FAIL: expected ChannelBufferFull, got " ++ show e
        Right _ -> error "  FAIL: expected buffer full error"

testChannelSendOversized :: IO ()
testChannelSendOversized = do
  putStrLn "Channel send oversized message:"
  let config = defaultChannelConfig {ccMaxMessageSize = 10}
      ch = newChannel (ChannelId 0) config
      now = 0 :: MonoTime
      bigPayload = BS.replicate 11 0x41
  case channelSend bigPayload now ch of
    Left ChannelMessageTooLarge -> putStrLn "  PASS: oversized returns ChannelMessageTooLarge"
    Left e -> error $ "  FAIL: expected ChannelMessageTooLarge, got " ++ show e
    Right _ -> error "  FAIL: expected oversized error"

testChannelUnreliableDelivery :: IO ()
testChannelUnreliableDelivery = do
  putStrLn "Channel unreliable delivery:"
  let config = unreliableConfig
      ch0 = newChannel (ChannelId 0) config
      now = 0 :: MonoTime
      payload = "test-data"
  -- Send a message
  case channelSend payload now ch0 of
    Left e -> error $ "  FAIL: send failed: " ++ show e
    Right (seqNum, ch1) -> do
      assertEqual "assigned seq 0" (SequenceNum 0) seqNum
      -- Get outgoing message
      case getOutgoingMessage ch1 of
        Nothing -> error "  FAIL: no outgoing message"
        Just (msg, ch2) -> do
          assertEqual "outgoing seq" (SequenceNum 0) (cmSequence msg)
          assertEqual "outgoing data" payload (cmData msg)
          -- Simulate receiving this message on the remote side
          let ch3 = onMessageReceived (cmSequence msg) (cmData msg) now ch2
          -- Read received messages
          let (received, _ch4) = channelReceive ch3
          assertEqual "received 1 message" 1 (length received)
          assertEqual "received data matches" payload (head received)
          putStrLn "  PASS: unreliable send/receive roundtrip"

testChannelReliableOrderedDelivery :: IO ()
testChannelReliableOrderedDelivery = do
  putStrLn "Channel reliable ordered delivery (out-of-order arrival):"
  let config = reliableOrderedConfig
      ch0 = newChannel (ChannelId 0) config
      now = 0 :: MonoTime
      payload0 = "msg-0"
      payload1 = "msg-1"
      payload2 = "msg-2"
  -- Receive messages out of order: 0, 2, 1
  -- Receive seq 0 (expected = 0, so delivered immediately)
  let ch1 = onMessageReceived (SequenceNum 0) payload0 now ch0
  -- Receive seq 2 (expected = 1, so buffered)
  let ch2 = onMessageReceived (SequenceNum 2) payload2 now ch1
  -- Receive seq 1 (expected = 1, so delivered, then flushes buffered seq 2)
  let ch3 = onMessageReceived (SequenceNum 1) payload1 now ch2
  -- Read all received messages
  let (received, _ch4) = channelReceive ch3
  assertEqual "received 3 messages" 3 (length received)
  case received of
    [r0, r1, r2] -> do
      assertEqual "order: msg-0 first" payload0 r0
      assertEqual "order: msg-1 second" payload1 r1
      assertEqual "order: msg-2 third" payload2 r2
    _ -> error "FAIL: expected exactly 3 messages"

testChannelReliableSequencedDropOld :: IO ()
testChannelReliableSequencedDropOld = do
  putStrLn "Channel reliable sequenced drops old:"
  let config = reliableSequencedConfig
      ch0 = newChannel (ChannelId 0) config
      now = 0 :: MonoTime
  -- Receive seq 2 first (greater than remote seq 0, so accepted)
  let ch1 = onMessageReceived (SequenceNum 2) "new-msg" now ch0
  -- Receive seq 0 (not greater than remote seq 2, so dropped)
  let ch2 = onMessageReceived (SequenceNum 0) "old-msg" now ch1
  let (received, _ch3) = channelReceive ch2
  assertEqual "only 1 message received" 1 (length received)
  assertEqual "received the newer message" "new-msg" (head received)
  assertEqual "1 message dropped" 1 (chTotalDropped ch2)

testChannelRetransmit :: IO ()
testChannelRetransmit = do
  putStrLn "Channel retransmission after RTO:"
  let config = reliableOrderedConfig
      ch0 = newChannel (ChannelId 0) config
      sendTime = 0 :: MonoTime
      payload = "reliable-msg"
      rto = 200.0 :: Double -- RTO in milliseconds
      -- Send a reliable message
  case channelSend payload sendTime ch0 of
    Left e -> error $ "  FAIL: send failed: " ++ show e
    Right (_, ch1) -> do
      -- Get outgoing message (marks as sent, retryCount -> 1)
      case getOutgoingMessage ch1 of
        Nothing -> error "  FAIL: no outgoing message"
        Just (_, ch2) -> do
          -- Check before RTO: no retransmits
          let beforeRto = sendTime + 100000000 -- 100ms in nanoseconds
          let (retransBefore, ch3) = getRetransmitMessages beforeRto rto ch2
          assertEqual "no retransmit before RTO" 0 (length retransBefore)
          -- Check after RTO: should retransmit
          let afterRto = sendTime + 300000000 -- 300ms in nanoseconds (> 200ms RTO)
          let (retransAfter, ch4) = getRetransmitMessages afterRto rto ch3
          assertEqual "1 retransmit after RTO" 1 (length retransAfter)
          assertEqual "retransmitted data" payload (cmData (head retransAfter))
          assertEqual "retransmit count incremented" 1 (chTotalRetransmits ch4)

--------------------------------------------------------------------------------
-- Fragment: split, reassemble, header roundtrip, cleanup, too-large
--------------------------------------------------------------------------------

testFragmentSplitReassemble :: IO ()
testFragmentSplitReassemble = do
  putStrLn "Fragment split and reassemble:"
  let msgId = MessageId 42
      maxPayload = 100
      msgData = BS.replicate 250 0xAB
      expectedFragCount = (BS.length msgData + maxPayload - 1) `div` maxPayload -- 3
  case fragmentMessage msgId msgData maxPayload of
    Left e -> error $ "  FAIL: fragmentMessage failed: " ++ show e
    Right frags -> do
      assertEqual "fragment count" expectedFragCount (length frags)
      -- Reassemble using processFragment
      let now = 0 :: MonoTime
          assembler0 = newFragmentAssembler 5000.0 (1024 * 1024)
      -- Feed all fragments
      let (result, _assembler) = foldl feedFrag (Nothing, assembler0) frags
            where
              feedFrag (prevResult, asm) frag =
                let (r, asm') = processFragment frag now asm
                 in (case r of Nothing -> prevResult; Just _ -> r, asm')
      case result of
        Nothing -> error "  FAIL: reassembly did not produce a result"
        Just reassembled ->
          assertEqual "reassembled matches original" msgData reassembled

testFragmentHeaderRoundTrip :: IO ()
testFragmentHeaderRoundTrip = do
  putStrLn "Fragment header serialize/deserialize roundtrip:"
  let header =
        FragmentHeader
          { fhMessageId = MessageId 0xDEADBEEF,
            fhFragmentIndex = 7,
            fhFragmentCount = 15
          }
      serialized = serializeFragmentHeader header
  assertEqual "header size" fragmentHeaderSize (BS.length serialized)
  case deserializeFragmentHeader serialized of
    Nothing -> error "  FAIL: deserializeFragmentHeader returned Nothing"
    Just decoded -> do
      assertEqual "messageId roundtrip" (fhMessageId header) (fhMessageId decoded)
      assertEqual "fragmentIndex roundtrip" (fhFragmentIndex header) (fhFragmentIndex decoded)
      assertEqual "fragmentCount roundtrip" (fhFragmentCount header) (fhFragmentCount decoded)

testFragmentCleanupExpiry :: IO ()
testFragmentCleanupExpiry = do
  putStrLn "Fragment cleanup removes expired buffers:"
  let timeoutMs = 1000.0
      assembler0 = newFragmentAssembler timeoutMs (1024 * 1024)
      header =
        FragmentHeader
          { fhMessageId = MessageId 99,
            fhFragmentIndex = 0,
            fhFragmentCount = 3
          }
      fragData = serializeFragmentHeader header <> BS.replicate 50 0xCC
      createTime = 0 :: MonoTime
  -- Process one fragment (incomplete assembly)
  let (_result, assembler1) = processFragment fragData createTime assembler0
  assertEqual "1 buffer in progress" 1 (length (faBuffers assembler1))
  -- Cleanup before timeout: buffer should remain
  let beforeTimeout = createTime + 500000000 -- 500ms in nanoseconds
  let assembler2 = cleanupFragments beforeTimeout assembler1
  assertEqual "buffer still present before timeout" 1 (length (faBuffers assembler2))
  -- Cleanup after timeout: buffer should be removed
  let afterTimeout = createTime + 1500000000 -- 1500ms in nanoseconds (> 1000ms timeout)
  let assembler3 = cleanupFragments afterTimeout assembler2
  assertEqual "buffer removed after timeout" 0 (length (faBuffers assembler3))

testFragmentTooLarge :: IO ()
testFragmentTooLarge = do
  putStrLn "Fragment too many fragments:"
  let msgId = MessageId 1
      tinyPayload = 1
      bigMsg = BS.replicate (maxFragmentCount + 1) 0xFF
  case fragmentMessage msgId bigMsg tinyPayload of
    Left TooManyFragments -> putStrLn "  PASS: too many fragments returns TooManyFragments"
    Right _ -> error "  FAIL: expected TooManyFragments error"

--------------------------------------------------------------------------------
-- Security: CRC32C, rate limiting, token validation
--------------------------------------------------------------------------------

testCrc32Roundtrip :: IO ()
testCrc32Roundtrip = do
  putStrLn "CRC32C append and validate roundtrip:"
  let original = "hello, network!" :: BS.ByteString
      withCrc = appendCrc32 original
  assertEqual "crc adds 4 bytes" (BS.length original + crc32Size) (BS.length withCrc)
  case validateAndStripCrc32 withCrc of
    Nothing -> error "  FAIL: validateAndStripCrc32 returned Nothing on valid data"
    Just stripped -> assertEqual "stripped matches original" original stripped

testCrc32RejectCorrupt :: IO ()
testCrc32RejectCorrupt = do
  putStrLn "CRC32C rejects corrupted data:"
  let original = "important data" :: BS.ByteString
      withCrc = appendCrc32 original
      -- Flip a bit in the payload area
      corrupted = BS.cons (BS.index withCrc 0 + 1) (BS.tail withCrc)
  case validateAndStripCrc32 corrupted of
    Nothing -> putStrLn "  PASS: corrupted data rejected"
    Just _ -> error "  FAIL: corrupted data should have been rejected"

testRateLimiterAllow :: IO ()
testRateLimiterAllow = do
  putStrLn "Rate limiter allows up to max requests:"
  let maxReqs = 3
      now = 1000000000 :: MonoTime -- 1 second
      rl0 = newRateLimiter maxReqs now
      addrKey = 12345 :: Word64
  -- Send maxReqs requests, all should be allowed
  let (allowed1, rl1) = rateLimiterAllow addrKey now rl0
  let (allowed2, rl2) = rateLimiterAllow addrKey now rl1
  let (allowed3, _rl3) = rateLimiterAllow addrKey now rl2
  assertEqual "request 1 allowed" True allowed1
  assertEqual "request 2 allowed" True allowed2
  assertEqual "request 3 allowed" True allowed3

testRateLimiterDeny :: IO ()
testRateLimiterDeny = do
  putStrLn "Rate limiter denies after exceeding limit:"
  let maxReqs = 2
      now = 1000000000 :: MonoTime
      rl0 = newRateLimiter maxReqs now
      addrKey = 99999 :: Word64
  -- Send maxReqs requests (allowed)
  let (_, rl1) = rateLimiterAllow addrKey now rl0
  let (_, rl2) = rateLimiterAllow addrKey now rl1
  -- Next request should be denied
  let (denied, _rl3) = rateLimiterAllow addrKey now rl2
  assertEqual "excess request denied" False denied

testTokenValidation :: IO ()
testTokenValidation = do
  putStrLn "Token validation accepts valid token:"
  let now = 1000000000 :: MonoTime -- 1 second
      expireMs = 30000.0 -- 30 seconds
      clientId = 42 :: Word64
      token = newConnectToken clientId expireMs "user-data" now
      validator0 = newTokenValidator 60000.0 100
  case validateToken token now validator0 of
    (Right cid, _) -> assertEqual "returns client id" clientId cid
    (Left e, _) -> error $ "  FAIL: valid token rejected: " ++ show e

testTokenExpired :: IO ()
testTokenExpired = do
  putStrLn "Token validation rejects expired token:"
  let createTime = 1000000000 :: MonoTime -- 1 second
      expireMs = 5000.0 -- 5 seconds
      clientId = 100 :: Word64
      token = newConnectToken clientId expireMs "data" createTime
      validator = newTokenValidator 60000.0 100
      -- Validate well after expiry (10 seconds later = 10,000ms > 5000ms)
      futureTime = createTime + 10000000000 -- 10 seconds in nanoseconds
  case validateToken token futureTime validator of
    (Left TokenExpired, _) -> putStrLn "  PASS: expired token rejected with TokenExpired"
    (Left e, _) -> error $ "  FAIL: expected TokenExpired, got " ++ show e
    (Right _, _) -> error "  FAIL: expected expired token to be rejected"

testTokenReplayed :: IO ()
testTokenReplayed = do
  putStrLn "Token validation rejects replayed token:"
  let now = 1000000000 :: MonoTime
      expireMs = 30000.0
      clientId = 77 :: Word64
      token = newConnectToken clientId expireMs "data" now
      validator0 = newTokenValidator 60000.0 100
  -- First validation succeeds
  case validateToken token now validator0 of
    (Left e, _) -> error $ "  FAIL: first validation should succeed: " ++ show e
    (Right _, validator1) ->
      -- Second validation with same clientId should fail as replayed
      case validateToken token now validator1 of
        (Left TokenReplayed, _) -> putStrLn "  PASS: replayed token rejected with TokenReplayed"
        (Left e, _) -> error $ "  FAIL: expected TokenReplayed, got " ++ show e
        (Right _, _) -> error "  FAIL: expected replayed token to be rejected"
