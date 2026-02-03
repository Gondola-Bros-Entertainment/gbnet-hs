-- |
-- Module      : GBNet.Packet
-- Description : Packet types and header definitions for the wire protocol
--
-- Defines 'PacketType' and 'PacketHeader' for the gbnet wire format.
-- Header is 68 bits: 4-bit type + 16-bit sequence + 16-bit ack + 32-bit ack bitfield.

module GBNet.Packet
  ( PacketType(..)
  , PacketHeader(..)
  , packetHeaderBitSize
  ) where

import Data.Word (Word16, Word32)
import GBNet.Serialize.BitBuffer (BitReader(..), readBitsM, writeBits)
import GBNet.Serialize.Class (BitSerialize(..), BitDeserialize(..),
                               packetTypeBitWidth, runDeserialize, deserializeM)

-- | Header size in bits (4 + 16 + 16 + 32 = 68).
packetHeaderBitSize :: Int
packetHeaderBitSize = 68

-- | Packet type tag (4 bits on wire).
data PacketType
  = ConnectionRequest   -- ^ 0: Client initiates connection
  | ConnectionAccepted  -- ^ 1: Server accepts
  | ConnectionDenied    -- ^ 2: Server rejects
  | Payload             -- ^ 3: Normal game data
  | Disconnect          -- ^ 4: Graceful disconnect
  | Keepalive           -- ^ 5: Keep connection alive
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
  { packetType   :: !PacketType  -- ^ 4 bits
  , sequenceNum  :: !Word16      -- ^ 16 bits
  , ack          :: !Word16      -- ^ 16 bits - most recent received sequence
  , ackBitfield  :: !Word32      -- ^ 32 bits - preceding 32 acks
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
    pt  <- deserializeM
    sn  <- deserializeM
    ak  <- deserializeM
    abf <- deserializeM
    pure PacketHeader
      { packetType  = pt
      , sequenceNum = sn
      , ack         = ak
      , ackBitfield = abf
      }
