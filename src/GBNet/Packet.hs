-- |
-- Module      : GBNet.Packet
-- Description : Packet types and header definitions for the wire protocol
--
-- Defines 'PacketType' and 'PacketHeader' for the gbnet wire format.
-- Header is 68 bits: 4-bit type + 16-bit sequence + 16-bit ack + 32-bit ack bitfield.
module GBNet.Packet
  ( -- * Types
    PacketType (..),
    PacketHeader (..),
    Packet (..),

    -- * Constants
    packetHeaderBitSize,
    packetHeaderByteSize,

    -- * Serialization
    serializePacket,
    deserializePacket,
    serializeHeader,
    deserializeHeader,
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word16, Word32)
import GBNet.Serialize.BitBuffer
  ( BitReader (..),
    ReadResult (..),
    empty,
    fromBytes,
    readBitsM,
    toBytes,
    writeBits,
  )
import GBNet.Serialize.Class
  ( BitDeserialize (..),
    BitSerialize (..),
    deserializeM,
    packetTypeBitWidth,
    runDeserialize,
  )

-- | Header size in bits (4 + 16 + 16 + 32 = 68).
packetHeaderBitSize :: Int
packetHeaderBitSize = 68

-- | Packet type tag (4 bits on wire).
-- Additional fields are serialized after the header.
data PacketType
  = -- | 0: Client initiates connection
    ConnectionRequest
  | -- | 1: Server accepts
    ConnectionAccepted
  | -- | 2: Server rejects (+ 8-bit reason in payload)
    ConnectionDenied
  | -- | 3: Normal game data (+ 3-bit channel, 1-bit is_fragment in payload)
    Payload
  | -- | 4: Graceful disconnect (+ 8-bit reason in payload)
    Disconnect
  | -- | 5: Keep connection alive
    Keepalive
  | -- | 6: Server challenge (+ 64-bit server_salt in payload)
    ConnectionChallenge
  | -- | 7: Client response (+ 64-bit client_salt in payload)
    ConnectionResponse
  deriving (Eq, Show, Enum, Bounded)

instance BitSerialize PacketType where
  bitSerialize pt = writeBits (fromIntegral (fromEnum pt)) packetTypeBitWidth

instance BitDeserialize PacketType where
  bitDeserialize = runDeserialize $ do
    val <- readBitsM packetTypeBitWidth
    let tag = fromIntegral val :: Int
    if tag > fromEnum (maxBound :: PacketType)
      then BitReader $ \_ -> Left $ "PacketType: invalid tag " ++ show tag
      else pure $ toEnum tag

-- | Packet header (68 bits on wire).
data PacketHeader = PacketHeader
  { -- | 4 bits
    packetType :: !PacketType,
    -- | 16 bits
    sequenceNum :: !Word16,
    -- | 16 bits - most recent received sequence
    ack :: !Word16,
    -- | 32 bits - preceding 32 acks
    ackBitfield :: !Word32
  }
  deriving (Eq, Show)

instance BitSerialize PacketHeader where
  bitSerialize hdr =
    bitSerialize (ackBitfield hdr)
      . bitSerialize (ack hdr)
      . bitSerialize (sequenceNum hdr)
      . bitSerialize (packetType hdr)

instance BitDeserialize PacketHeader where
  bitDeserialize = runDeserialize $ do
    pt <- deserializeM
    sn <- deserializeM
    ak <- deserializeM
    abf <- deserializeM
    pure
      PacketHeader
        { packetType = pt,
          sequenceNum = sn,
          ack = ak,
          ackBitfield = abf
        }

-- | Header size in bytes (68 bits = 9 bytes, rounded up).
packetHeaderByteSize :: Int
packetHeaderByteSize = (packetHeaderBitSize + 7) `div` 8

-- | A complete packet with header and payload.
data Packet = Packet
  { pktHeader :: !PacketHeader,
    pktPayload :: !BS.ByteString
  }
  deriving (Eq, Show)

-- | Serialize a packet header to bytes.
serializeHeader :: PacketHeader -> BS.ByteString
serializeHeader hdr = toBytes $ bitSerialize hdr empty

-- | Deserialize a packet header from bytes.
deserializeHeader :: BS.ByteString -> Either String PacketHeader
deserializeHeader bs
  | BS.length bs < packetHeaderByteSize = Left "Header too short"
  | otherwise =
      case bitDeserialize (fromBytes bs) of
        Left err -> Left err
        Right result -> Right (readValue result)

-- | Serialize a complete packet (header + payload) to bytes.
serializePacket :: Packet -> BS.ByteString
serializePacket pkt =
  let headerBytes = serializeHeader (pktHeader pkt)
   in headerBytes <> pktPayload pkt

-- | Deserialize a complete packet from bytes.
deserializePacket :: BS.ByteString -> Either String Packet
deserializePacket bs = do
  header <- deserializeHeader bs
  let payload = BS.drop packetHeaderByteSize bs
  Right Packet {pktHeader = header, pktPayload = payload}
