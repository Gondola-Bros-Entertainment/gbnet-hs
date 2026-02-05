{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq (NFData (..), rwhnf)
import Criterion.Main
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word16, Word32, Word64)
import Foreign.Storable (Storable (..))
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
import GBNet.Config (defaultNetworkConfig)
import GBNet.Connection
  ( Connection (..),
    drainSendQueue,
    newConnection,
    sendMessage,
  )
import qualified GBNet.Connection
import GBNet.Fragment
  ( FragmentAssembler (..),
    FragmentHeader (..),
    deserializeFragmentHeader,
    fragmentMessage,
    newFragmentAssembler,
    processFragment,
    serializeFragmentHeader,
  )
import qualified GBNet.Fragment
import GBNet.Packet
  ( PacketHeader (..),
    PacketType (..),
    deserializeHeader,
    serializeHeader,
  )
import GBNet.Reliability
  ( ReliableEndpoint (..),
    SentPacketRecord (..),
    SequenceBuffer (..),
    newReliableEndpoint,
    newSequenceBuffer,
    onPacketSent,
    onPacketsReceived,
    processAcks,
    sbExists,
    sbInsert,
    updateRtt,
  )
import GBNet.Security (appendCrc32, validateAndStripCrc32)
import GBNet.Serialize.FastSupport (castPtr, plusPtr, serialize)
import GBNet.Serialize.FastTH (deriveStorable)
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
-- NFData instances for criterion (force full evaluation)
--------------------------------------------------------------------------------

-- Note: SequenceNum, ChannelId have NFData derived in GBNet.Types

instance NFData MessageId where rnf (MessageId w) = rnf w

-- Note: MonoTime NFData is derived in GBNet.Class

instance NFData FragmentHeader where
  rnf (FragmentHeader mid idx cnt) = rnf mid `seq` rnf idx `seq` rnf cnt

instance NFData PacketType where rnf = rwhnf

instance NFData DeliveryMode where rnf = rwhnf

instance NFData ChannelError where rnf = rwhnf

instance NFData Vec3S where rnf (Vec3S x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData Transform where rnf (Transform v r) = rnf v `seq` rnf r

-- Packet
instance NFData PacketHeader where
  rnf (PacketHeader pt sn ak abf) = rnf pt `seq` rnf sn `seq` rnf ak `seq` rnf abf

-- Reliability
-- Note: SentPacketRecord NFData is defined in GBNet.Reliability

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

-- Connection (use rwhnf - deep eval is expensive)
instance NFData Connection where rnf = rwhnf

instance NFData GBNet.Connection.ConnectionError where rnf = rwhnf

instance NFData GBNet.Connection.OutgoingPacket where rnf = rwhnf

-- FragmentAssembler
instance NFData FragmentAssembler where rnf = rwhnf

instance NFData GBNet.Fragment.FragmentError where rnf = rwhnf

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

-- | Build a fresh Connection for benchmarking.
buildConnection :: Connection
buildConnection = newConnection defaultNetworkConfig 0x12345678 (MonoTime 0)

-- | MTU for fragmentation benchmarks.
benchMtu :: Int
benchMtu = 1200

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
        [ env (pure (newReliableEndpoint 256, map SequenceNum [0 .. 99])) $ \ ~(ep, seqs) ->
            bench "onPacketsReceived/100-sequential" $
              nf (onPacketsReceived seqs) ep,
          env (pure (newReliableEndpoint 256, map (\i -> SequenceNum (i * 3)) [0 .. 99])) $ \ ~(ep, seqs) ->
            bench "onPacketsReceived/100-gaps" $
              nf (onPacketsReceived seqs) ep,
          env (pure (newReliableEndpoint 256)) $ \ep ->
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
          env (pure (newReliableEndpoint 256)) $ \ep ->
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
            bench "vec3" $ nf serialize v,
          env (pure (Transform (Vec3S 1.0 2.0 3.0) 45.0)) $ \t ->
            bench "transform/nested" $ nf serialize t
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
        ]
    ]
