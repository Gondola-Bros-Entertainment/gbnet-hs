{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word32)
import GBNet.Channel
  ( Channel,
    channelSend,
    defaultChannelConfig,
    getRetransmitMessages,
    newChannel,
    onMessageReceived,
    unreliableConfig,
  )
import GBNet.Class (MonoTime (..))
import GBNet.Config (defaultNetworkConfig)
import GBNet.Congestion
  ( batchMessages,
    ccRefillBudget,
    ccUpdate,
    cwOnAck,
    cwOnLoss,
    newCongestionController,
    newCongestionWindow,
    unbatchMessages,
  )
import GBNet.Connection
  ( Connection,
    drainSendQueue,
    newConnection,
    sendMessage,
  )
import GBNet.Crypto (EncryptionKey (..), NonceCounter (..), decrypt, encrypt)
import GBNet.Fragment
  ( FragmentHeader (..),
    deserializeFragmentHeader,
    fragmentMessage,
    newFragmentAssembler,
    processFragment,
    serializeFragmentHeader,
  )
import GBNet.Packet
  ( PacketHeader (..),
    PacketType (Payload),
    deserializeHeader,
    serializeHeader,
  )
import GBNet.Reliability
  ( ReliableEndpoint,
    SequenceBuffer,
    newReliableEndpoint,
    newSequenceBuffer,
    onPacketSent,
    onPacketsReceived,
    processAcks,
    sbExists,
    sbInsert,
    sbInsertMany,
    updateRtt,
  )
import GBNet.Replication.Delta (DeltaTracker, NetworkDelta (..), deltaEncode, newDeltaTracker)
import GBNet.Replication.Interest (InterestManager (..), newRadiusInterest)
import GBNet.Security (appendCrc32, validateAndStripCrc32)
import GBNet.Serialize (deserialize, serialize)
import GBNet.Serialize.TH (deriveStorable)
import GBNet.Types (ChannelId (..), MessageId (..), SequenceNum (..))

--------------------------------------------------------------------------------
-- TH-derived benchmark types
--------------------------------------------------------------------------------

-- Storable-based serialization
data Vec3S = Vec3S !Float !Float !Float
  deriving (Eq, Show)

deriveStorable ''Vec3S

-- Nested type - demonstrates composition
data Transform = Transform !Vec3S !Float -- position + rotation angle
  deriving (Eq, Show)

deriveStorable ''Transform

--------------------------------------------------------------------------------
-- NFData instances for benchmark-only types
--------------------------------------------------------------------------------

instance NFData Vec3S where rnf (Vec3S x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Transform where rnf (Transform v r) = rnf v `seq` rnf r

-- | Trivial delta instance for benchmarking tracker mechanics.
-- Uses full state as delta (no field-level compression).
instance NetworkDelta Vec3S where
  type Delta Vec3S = Vec3S
  diff current _baseline = current
  apply _state delta = delta

--------------------------------------------------------------------------------
-- Setup helpers
--------------------------------------------------------------------------------

-- | A sample packet header for benchmarking.
sampleHeader :: PacketHeader
sampleHeader =
  PacketHeader
    { packetType = Payload,
      sequenceNum = SequenceNum 42,
      ack = SequenceNum 40,
      ackBitfield = 0xDEADBEEF
    }

-- | Pre-serialized header bytes.
headerBytes :: BS.ByteString
headerBytes = serializeHeader sampleHeader

-- | A sample fragment header for benchmarking.
sampleFragmentHeader :: FragmentHeader
sampleFragmentHeader =
  FragmentHeader
    { fhMessageId = MessageId 0xDEADBEEF,
      fhFragmentIndex = 3,
      fhFragmentCount = 10
    }

-- | Pre-serialized fragment header bytes.
fragmentHeaderBytes :: BS.ByteString
fragmentHeaderBytes = serializeFragmentHeader sampleFragmentHeader

-- | 64-byte payload for channel benchmarks.
payload64 :: BS.ByteString
payload64 = BS.replicate 64 0xAB

-- | 1KB payload for fragment benchmarks.
payload1k :: BS.ByteString
payload1k = BS.replicate 1024 0xCD

-- | 64-byte payload with CRC appended.
payload64WithCrc :: BS.ByteString
payload64WithCrc = appendCrc32 payload64

-- | Test encryption key (32 bytes).
benchKey :: EncryptionKey
benchKey = EncryptionKey (BS.replicate 32 0xAA)

-- | Test protocol ID for benchmarks.
benchProtocolId :: Word32
benchProtocolId = 0x12345678

-- | Build a fresh Connection for benchmarking.
buildConnection :: Connection
buildConnection = newConnection defaultNetworkConfig 0x12345678 (MonoTime 0)

-- | MTU for fragmentation benchmarks.
benchMtu :: Int
benchMtu = 1200

-- | Build a ReliableEndpoint with N in-flight sent packets.
buildEndpointWithInFlight :: Int -> ReliableEndpoint
buildEndpointWithInFlight n =
  foldl' sendOne newReliableEndpoint [0 .. fromIntegral (n - 1)]
  where
    sendOne ep i =
      let seqNum = SequenceNum i
          sendTime = MonoTime (fromIntegral i * 1000000) -- 1ms apart
       in onPacketSent seqNum sendTime (ChannelId 0) seqNum 64 ep

-- | Build a SequenceBuffer with N sequential entries.
buildSequenceBuffer :: Int -> SequenceBuffer ()
buildSequenceBuffer n =
  foldl' (\buf i -> sbInsert (SequenceNum (fromIntegral i)) () buf) (newSequenceBuffer 256) [0 .. n - 1]

-- | Build a Channel pre-loaded with N pending reliable messages.
buildChannelWithPending :: Int -> Channel
buildChannelWithPending n =
  let ch = newChannel (ChannelId 0) defaultChannelConfig
   in foldl' sendMsg ch [0 .. n - 1]
  where
    sendMsg ch i =
      case channelSend payload64 (MonoTime (fromIntegral i * 1000000)) ch of
        Right (_, ch') -> ch'
        Left _ -> ch

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ -- Group 1: Reliability (ACK processing)
      bgroup
        "reliability"
        [ env (pure (newReliableEndpoint, map SequenceNum [0 .. 99])) $ \ ~(ep, seqs) ->
            bench "onPacketsReceived/100-sequential" $
              nf (onPacketsReceived seqs) ep,
          env (pure (newReliableEndpoint, map (\i -> SequenceNum (i * 3)) [0 .. 99])) $ \ ~(ep, seqs) ->
            bench "onPacketsReceived/100-gaps" $
              nf (onPacketsReceived seqs) ep,
          env (pure newReliableEndpoint) $ \ep ->
            bench "onPacketsReceived/single" $
              nf (onPacketsReceived [SequenceNum 42]) ep,
          env (pure (buildEndpointWithInFlight 10)) $ \ep ->
            bench "processAcks/10-inflight" $
              nf
                (processAcks (SequenceNum 9) 0x1FF (MonoTime 50000000))
                ep,
          env (pure (buildEndpointWithInFlight 100)) $ \ep ->
            bench "processAcks/100-inflight" $
              nf
                (processAcks (SequenceNum 99) maxBound (MonoTime 500000000))
                ep,
          env (pure newReliableEndpoint) $ \ep ->
            bench "updateRtt" $
              nf (updateRtt 25.0 . updateRtt 30.0) ep
        ],
      -- Group 2: SequenceBuffer
      bgroup
        "sequencebuffer"
        [ env (pure (newSequenceBuffer 256 :: SequenceBuffer ())) $ \buf ->
            bench "sbInsert/sequential" $
              nf
                ( \b ->
                    foldl'
                      (\acc i -> sbInsert (SequenceNum i) () acc)
                      b
                      [0 .. 99]
                )
                buf,
          env (pure (newSequenceBuffer 256 :: SequenceBuffer ())) $ \buf ->
            bench "sbInsert/wraparound" $
              nf
                ( \b ->
                    foldl'
                      (\acc i -> sbInsert (SequenceNum (maxBound - 50 + i)) () acc)
                      b
                      [0 .. 99]
                )
                buf,
          env (pure (newSequenceBuffer 256 :: SequenceBuffer (), [(SequenceNum i, ()) | i <- [0 .. 99]])) $ \ ~(buf, items) ->
            bench "sbInsertMany/100" $
              nf (sbInsertMany items) buf,
          env (pure (buildSequenceBuffer 100)) $ \buf ->
            bench "sbExists/hit" $
              whnf (sbExists (SequenceNum 50)) buf,
          env (pure (buildSequenceBuffer 100)) $ \buf ->
            bench "sbExists/miss" $
              whnf (sbExists (SequenceNum 200)) buf
        ],
      -- Group 3: Channel (message pipeline)
      bgroup
        "channel"
        [ env (pure (newChannel (ChannelId 0) defaultChannelConfig)) $ \ch ->
            bench "channelSend/reliable" $
              nf (channelSend payload64 (MonoTime 1000000)) ch,
          env (pure (newChannel (ChannelId 0) unreliableConfig)) $ \ch ->
            bench "channelSend/unreliable" $
              nf (channelSend payload64 (MonoTime 1000000)) ch,
          env (pure (newChannel (ChannelId 0) defaultChannelConfig)) $ \ch ->
            bench "onMessageReceived/ordered" $
              nf
                ( \c ->
                    foldl'
                      (\acc i -> onMessageReceived (SequenceNum i) payload64 (MonoTime 1000000) acc)
                      c
                      [0 .. 49]
                )
                ch,
          env (pure (buildChannelWithPending 50, MonoTime 10000000000)) $ \ ~(ch, now) ->
            bench "getRetransmitMessages/50-pending" $
              nf (\(c, t) -> getRetransmitMessages t 100.0 c) (ch, now)
        ],
      -- Group 4: Packet Header
      bgroup
        "packetheader"
        [ env (pure sampleHeader) $ \hdr ->
            bench "serialize" $ nf serializeHeader hdr,
          env (pure headerBytes) $ \bs ->
            bench "deserialize" $ nf deserializeHeader bs
        ],
      -- Group 5: Fragment Header
      bgroup
        "fragmentheader"
        [ env (pure sampleFragmentHeader) $ \hdr ->
            bench "serialize" $ nf serializeFragmentHeader hdr,
          env (pure fragmentHeaderBytes) $ \bs ->
            bench "deserialize" $ nf deserializeFragmentHeader bs
        ],
      -- Group 6: Storable serialization (flat and nested)
      bgroup
        "storable"
        [ env (pure (Vec3S 1.0 (-2.5) 100.0)) $ \v ->
            bench "vec3/serialize" $ nf serialize v,
          env (pure (serialize (Vec3S 1.0 (-2.5) 100.0))) $ \bs ->
            bench "vec3/deserialize" $ nf (deserialize :: BS.ByteString -> Either String Vec3S) bs,
          env (pure (Transform (Vec3S 1.0 2.0 3.0) 45.0)) $ \t ->
            bench "transform/serialize" $ nf serialize t,
          env (pure (serialize (Transform (Vec3S 1.0 2.0 3.0) 45.0))) $ \bs ->
            bench "transform/deserialize" $ nf (deserialize :: BS.ByteString -> Either String Transform) bs
        ],
      -- Group 7: Connection operations
      bgroup
        "connection"
        [ env (pure buildConnection) $ \conn ->
            bench "sendMessage/64B" $
              nf (sendMessage (ChannelId 0) payload64 (MonoTime 1000000)) conn,
          env (pure buildConnection) $ \conn ->
            bench "drainSendQueue" $
              nf drainSendQueue conn
        ],
      -- Group 8: Fragmentation
      bgroup
        "fragment"
        [ env (pure payload1k) $ \payload ->
            bench "fragmentMessage/1KB" $
              nf (fragmentMessage (MessageId 1) payload) benchMtu,
          env (pure (newFragmentAssembler 5000.0 256)) $ \assembler ->
            bench "processFragment/single" $
              nf (processFragment payload64 (MonoTime 1000000)) assembler
        ],
      -- Group 9: Security (CRC32C)
      bgroup
        "security"
        [ env (pure payload64) $ \payload ->
            bench "crc32c/append/64B" $ nf appendCrc32 payload,
          env (pure payload1k) $ \payload ->
            bench "crc32c/append/1KB" $ nf appendCrc32 payload,
          env (pure payload64WithCrc) $ \payload ->
            bench "crc32c/validate/64B" $ nf validateAndStripCrc32 payload
        ],
      -- Group 10: Congestion control
      bgroup
        "congestion"
        [ env (pure (newCongestionController 60.0 0.1 250.0 10000.0)) $ \cc ->
            bench "ccUpdate/good" $
              nf (ccUpdate 0.02 50.0 (MonoTime 1000000000)) cc,
          env (pure (newCongestionController 60.0 0.1 250.0 10000.0)) $ \cc ->
            bench "ccUpdate/bad" $
              nf (ccUpdate 0.15 300.0 (MonoTime 1000000000)) cc,
          env (pure (newCongestionController 60.0 0.1 250.0 10000.0)) $ \cc ->
            bench "ccRefillBudget" $
              nf (ccRefillBudget 1200) cc,
          env (pure (newCongestionWindow 1200)) $ \cw ->
            bench "cwOnAck/1200B" $
              nf (cwOnAck 1200) cw,
          env (pure (newCongestionWindow 1200)) $ \cw ->
            bench "cwOnLoss" $
              nf cwOnLoss cw
        ],
      -- Group 11: Delta replication
      bgroup
        "delta"
        [ env (pure (newDeltaTracker 64 :: DeltaTracker Vec3S)) $ \dt ->
            bench "deltaEncode" $
              nf (deltaEncode 0 (Vec3S 1.0 2.0 3.0)) dt
        ],
      -- Group 12: Interest manager
      bgroup
        "interest"
        [ env (pure (newRadiusInterest 100.0)) $ \interest ->
            bench "relevant/inRange" $
              nf (\i -> relevant i (50.0, 50.0, 0.0) (100.0, 100.0, 0.0)) interest,
          env (pure (newRadiusInterest 100.0)) $ \interest ->
            bench "relevant/outOfRange" $
              nf (\i -> relevant i (0.0, 0.0, 0.0) (500.0, 500.0, 0.0)) interest
        ],
      -- Group 13: Message batching
      bgroup
        "batching"
        [ env (pure (replicate 10 payload64)) $ \msgs ->
            bench "batchMessages/10x64B" $
              nf (`batchMessages` 1200) msgs,
          env (pure (batchMessages (replicate 10 payload64) 1200)) $ \batches ->
            bench "unbatchMessages/10x64B" $
              nf (map unbatchMessages) batches
        ],
      -- Group 14: AEAD encryption (ChaCha20-Poly1305)
      bgroup
        "crypto"
        [ env (pure payload64) $ \payload ->
            bench "encrypt/64B" $
              nf (encrypt benchKey (NonceCounter 0) benchProtocolId) payload,
          env (pure payload1k) $ \payload ->
            bench "encrypt/1KB" $
              nf (encrypt benchKey (NonceCounter 0) benchProtocolId) payload,
          env (pure (forceRight (encrypt benchKey (NonceCounter 0) benchProtocolId payload64))) $ \ct ->
            bench "decrypt/64B" $
              nf (decrypt benchKey benchProtocolId) ct,
          env (pure (forceRight (encrypt benchKey (NonceCounter 0) benchProtocolId payload1k))) $ \ct ->
            bench "decrypt/1KB" $
              nf (decrypt benchKey benchProtocolId) ct
        ]
    ]

-- | Extract Right value, erroring on Left. Used only in benchmark setup.
forceRight :: Either a b -> b
forceRight (Right x) = x
forceRight (Left _) = error "forceRight: Left"
