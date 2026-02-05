{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq (NFData (..), rwhnf)
import Criterion.Main
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word16, Word32, Word64)
import GBNet.Channel
  ( Channel (..),
    ChannelConfig (..),
    ChannelError (..),
    ChannelMessage (..),
    DeliveryMode (..),
    channelSend,
    defaultChannelConfig,
    getRetransmitMessages,
    newChannel,
    onMessageReceived,
    unreliableConfig,
  )
import GBNet.Class (MonoTime (..))
import GBNet.Fragment
  ( FragmentHeader (..),
    deserializeFragmentHeader,
    serializeFragmentHeader,
  )
import GBNet.Packet
  ( PacketHeader (..),
    PacketType (..),
    deserializeHeader,
    serializeHeader,
  )
import GBNet.Types (ChannelId (..), MessageId (..), SequenceNum (..))
import GBNet.Reliability
  ( ReliableEndpoint (..),
    SentPacketRecord (..),
    SequenceBuffer (..),
    newReliableEndpoint,
    newSequenceBuffer,
    onPacketReceived,
    onPacketSent,
    processAcks,
    sbExists,
    sbInsert,
    updateRtt,
  )
import GBNet.Serialize.BitBuffer
  ( BitBuffer,
    BitReader,
    ReadResult (..),
    empty,
    fromBytes,
    readBits,
    toBytes,
    writeBits,
  )
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..), deserializeM, runDeserialize)
import GBNet.Serialize.TH (deriveNetworkSerialize)
import GBNet.Serialize.FastTH (deriveStorable)
import GBNet.Serialize.FastSupport (Storable, serialize, plusPtr, castPtr)

--------------------------------------------------------------------------------
-- TH-derived benchmark type (must precede use due to staging restriction)
--------------------------------------------------------------------------------

-- BitBuffer-based (old approach)
data Vec3 = Vec3
  { vecX :: !Float,
    vecY :: !Float,
    vecZ :: !Float
  }
  deriving (Eq, Show)

deriveNetworkSerialize ''Vec3

-- Storable-based (new approach) - flat type
data Vec3S = Vec3S !Float !Float !Float
  deriving (Eq, Show)

deriveStorable ''Vec3S

-- Storable-based nested type - demonstrates composition
data Transform = Transform !Vec3S !Float  -- position + rotation angle
  deriving (Eq, Show)

deriveStorable ''Transform

--------------------------------------------------------------------------------
-- NFData instances for criterion (force full evaluation)
--------------------------------------------------------------------------------

-- Primitives / newtypes
instance NFData SequenceNum where rnf (SequenceNum w) = rnf w

instance NFData ChannelId where rnf (ChannelId w) = rnf w

instance NFData MessageId where rnf (MessageId w) = rnf w

instance NFData MonoTime where rnf (MonoTime w) = rnf w

instance NFData FragmentHeader where
  rnf (FragmentHeader mid idx cnt) = rnf mid `seq` rnf idx `seq` rnf cnt

instance NFData PacketType where rnf = rwhnf

instance NFData DeliveryMode where rnf = rwhnf

instance NFData ChannelError where rnf = rwhnf

-- Serialization
instance NFData BitBuffer where rnf buf = rnf (toBytes buf)

instance NFData Vec3 where rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Vec3S where rnf (Vec3S x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Transform where rnf (Transform v r) = rnf v `seq` rnf r

instance (NFData a) => NFData (ReadResult a) where
  rnf (ReadResult val buf) = rnf val `seq` rnf buf

-- Packet
instance NFData PacketHeader where
  rnf (PacketHeader pt sn ak abf) = rnf pt `seq` rnf sn `seq` rnf ak `seq` rnf abf

-- Reliability
instance NFData SentPacketRecord where
  rnf (SentPacketRecord ci cs st sz nc) =
    rnf ci `seq` rnf cs `seq` rnf st `seq` rnf sz `seq` rnf nc

instance (NFData a) => NFData (SequenceBuffer a) where
  rnf sb = rnf (sbEntries sb) `seq` rnf (sbSequence sb) `seq` rnf (sbSize sb)

instance NFData ReliableEndpoint where
  rnf (ReliableEndpoint ls rs ab sp rp msd mif srtt rv rto hrs lw lwi lwc ts ta tl pe bs ba) =
    rnf ls `seq`
      rnf rs `seq`
        rnf ab `seq`
          rnf sp `seq`
            rnf rp `seq`
              rnf msd `seq`
                rnf mif `seq`
                  rnf srtt `seq`
                    rnf rv `seq`
                      rnf rto `seq`
                        rnf hrs `seq`
                          rnf lw `seq`
                            rnf lwi `seq`
                              rnf lwc `seq`
                                rnf ts `seq`
                                  rnf ta `seq`
                                    rnf tl `seq`
                                      rnf pe `seq`
                                        rnf bs `seq`
                                          rnf ba

-- Channel
instance NFData ChannelConfig where
  rnf (ChannelConfig dm mms mbs bof obt mobs mrr p) =
    rnf dm `seq`
      rnf mms `seq`
        rnf mbs `seq`
          rnf bof `seq`
            rnf obt `seq`
              rnf mobs `seq`
                rnf mrr `seq`
                  rnf p

instance NFData ChannelMessage where
  rnf (ChannelMessage s d t a r rel) =
    rnf s `seq` rnf d `seq` rnf t `seq` rnf a `seq` rnf r `seq` rnf rel

instance NFData Channel where
  rnf (Channel cfg ci ls rs sb rb pa orb oe ts tr td trt) =
    rnf cfg `seq`
      rnf ci `seq`
        rnf ls `seq`
          rnf rs `seq`
            rnf sb `seq`
              rnf rb `seq`
                rnf pa `seq`
                  rnf orb `seq`
                    rnf oe `seq`
                      rnf ts `seq`
                        rnf tr `seq`
                          rnf td `seq`
                            rnf trt

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

-- | A sample Vec3 for benchmarking.
sampleVec3 :: Vec3
sampleVec3 = Vec3 1.0 (-2.5) 100.0

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

-- | Pre-serialized Vec3 buffer.
vec3Buffer :: BitBuffer
vec3Buffer = bitSerialize sampleVec3 empty

-- | Vec3 as ByteString.
vec3Bytes :: BS.ByteString
vec3Bytes = toBytes vec3Buffer

-- | 64-byte payload for channel benchmarks.
payload64 :: BS.ByteString
payload64 = BS.replicate 64 0xAB

-- | Build a ReliableEndpoint with N in-flight sent packets.
buildEndpointWithInFlight :: Int -> ReliableEndpoint
buildEndpointWithInFlight n =
  foldl' sendOne (newReliableEndpoint 256) [0 .. fromIntegral (n - 1)]
  where
    sendOne ep i =
      let seqNum = SequenceNum i
          sendTime = MonoTime (fromIntegral i * 1000000) -- 1ms apart
       in onPacketSent seqNum sendTime (ChannelId 0) seqNum 64 ep

-- | Build a SequenceBuffer with N sequential entries.
buildSequenceBuffer :: Int -> SequenceBuffer ()
buildSequenceBuffer n =
  foldl' (\buf i -> sbInsert (SequenceNum (fromIntegral i)) () buf) (newSequenceBuffer 256) [0 .. n - 1]

-- | Build a ReliableEndpoint that has received N sequential packets.
buildEndpointWithReceived :: Int -> ReliableEndpoint
buildEndpointWithReceived n =
  foldl' (\ep i -> onPacketReceived (SequenceNum (fromIntegral i)) ep) (newReliableEndpoint 256) [0 .. n - 1]

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
    [ -- Group 1: BitBuffer (serialize/deserialize)
      bgroup
        "bitbuffer"
        [ env (pure empty) $ \buf ->
            bench "writeBits/aligned-64" $ nf (writeBits 0xDEADBEEFCAFEBABE 64) buf,
          env (pure empty) $ \buf ->
            bench "writeBits/unaligned-7" $ nf (writeBits 42 7) buf,
          env (pure (sampleHeader, empty)) $ \ ~(hdr, buf) ->
            bench "header/serialize" $ nf (\h -> toBytes (bitSerialize h buf)) hdr,
          env (pure headerBytes) $ \bs ->
            bench "header/deserialize" $ nf deserializeHeader bs,
          env (pure (sampleVec3, empty)) $ \ ~(v, buf) ->
            bench "record/serialize" $ nf (\x -> toBytes (bitSerialize x buf)) v,
          env (pure vec3Bytes) $ \bs ->
            bench "record/deserialize" $
              nf
                ( \b ->
                    let buf = fromBytes b
                     in runDeserialize (deserializeM :: BitReader Vec3) buf
                )
                bs
        ],
      -- Group 2: Reliability (ACK processing)
      bgroup
        "reliability"
        [ env (pure (newReliableEndpoint 256)) $ \ep ->
            bench "onPacketReceived/sequential" $
              nf
                ( \e ->
                    foldl'
                      (\acc i -> onPacketReceived (SequenceNum i) acc)
                      e
                      [0 .. 99]
                )
                ep,
          env (pure (newReliableEndpoint 256)) $ \ep ->
            bench "onPacketReceived/gaps" $
              nf
                ( \e ->
                    foldl'
                      (\acc i -> onPacketReceived (SequenceNum (i * 3)) acc)
                      e
                      [0 .. 99]
                )
                ep,
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
          env (pure (newReliableEndpoint 256)) $ \ep ->
            bench "updateRtt" $
              nf (updateRtt 25.0 . updateRtt 30.0) ep
        ],
      -- Group 3: SequenceBuffer
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
          env (pure (buildSequenceBuffer 100)) $ \buf ->
            bench "sbExists/hit" $
              whnf (sbExists (SequenceNum 50)) buf,
          env (pure (buildSequenceBuffer 100)) $ \buf ->
            bench "sbExists/miss" $
              whnf (sbExists (SequenceNum 200)) buf
        ],
      -- Group 4: Channel (message pipeline)
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
      -- Group 5: Packet Header (optimized poke-based serialization)
      bgroup
        "packetheader"
        [ env (pure sampleHeader) $ \hdr ->
            bench "serialize" $ nf serializeHeader hdr,
          env (pure headerBytes) $ \bs ->
            bench "deserialize" $ nf deserializeHeader bs
        ],
      -- Group 6: Fragment Header (optimized poke-based serialization)
      bgroup
        "fragmentheader"
        [ env (pure sampleFragmentHeader) $ \hdr ->
            bench "serialize" $ nf serializeFragmentHeader hdr,
          env (pure fragmentHeaderBytes) $ \bs ->
            bench "deserialize" $ nf deserializeFragmentHeader bs
        ],
      -- Group 7: Storable serialization (flat and nested)
      bgroup
        "storable"
        [ env (pure (sampleVec3, empty)) $ \ ~(v, buf) ->
            bench "vec3/bitbuffer" $ nf (\x -> toBytes (bitSerialize x buf)) v,
          env (pure (Vec3S 1.0 (-2.5) 100.0)) $ \v ->
            bench "vec3/storable" $ nf serialize v,
          env (pure (Transform (Vec3S 1.0 2.0 3.0) 45.0)) $ \t ->
            bench "transform/nested" $ nf serialize t
        ]
    ]
