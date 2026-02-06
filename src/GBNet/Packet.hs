{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word16, Word32, Word8)
import Foreign.Storable (pokeByteOff)
import GBNet.Types (SequenceNum (..))
import Optics.TH (makeFieldLabelsNoPrefix)

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

instance NFData PacketType where rnf = rwhnf

-- | Packet header (68 bits on wire).
data PacketHeader = PacketHeader
  { -- | 4 bits
    packetType :: !PacketType,
    -- | 16 bits
    sequenceNum :: !SequenceNum,
    -- | 16 bits - most recent received sequence
    ack :: !SequenceNum,
    -- | 32 bits - preceding 32 acks
    ackBitfield :: !Word32
  }
  deriving (Eq, Show)

instance NFData PacketHeader where
  rnf (PacketHeader pt sn ak abf) = rnf pt `seq` rnf sn `seq` rnf ak `seq` rnf abf

-- | Header size in bytes (68 bits = 9 bytes, rounded up).
packetHeaderByteSize :: Int
packetHeaderByteSize = (packetHeaderBitSize + 7) `div` 8

-- | A complete packet with header and payload.
data Packet = Packet
  { pktHeader :: !PacketHeader,
    pktPayload :: !BS.ByteString
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''PacketHeader
makeFieldLabelsNoPrefix ''Packet

-- | Bit shift for packet type in first byte.
packetTypeBitShift :: Int
packetTypeBitShift = 4

-- | Serialize a packet header to bytes.
-- Uses optimized direct memory writes.
--
-- Wire format (68 bits, MSB-first):
--   Byte 0:     [type:4][seq_hi:4]
--   Byte 1:     [seq_mid:8]
--   Byte 2:     [seq_lo:4][ack_hi:4]
--   Byte 3:     [ack_mid:8]
--   Byte 4:     [ack_lo:4][abf_hi:4]
--   Bytes 5-7:  [abf:24]
--   Byte 8:     [abf_lo:4][pad:4]
serializeHeader :: PacketHeader -> BS.ByteString
serializeHeader !hdr = unsafeCreate packetHeaderByteSize $ \ptr -> do
  let !pt = fromIntegral (fromEnum (packetType hdr)) :: Word8
      !(SequenceNum !sn) = sequenceNum hdr
      !(SequenceNum !ak) = ack hdr
      !abf = ackBitfield hdr
      !byte0 = (pt `shiftL` packetTypeBitShift) .|. fromIntegral (sn `shiftR` 12)
      !byte1 = fromIntegral (sn `shiftR` 4) :: Word8
      !byte2 = ((fromIntegral (sn .&. 0x0F) :: Word8) `shiftL` 4) .|. fromIntegral (ak `shiftR` 12)
      !byte3 = fromIntegral (ak `shiftR` 4) :: Word8
      !byte4 = ((fromIntegral (ak .&. 0x0F) :: Word8) `shiftL` 4) .|. fromIntegral (abf `shiftR` 28)
      !byte5 = fromIntegral (abf `shiftR` 20) :: Word8
      !byte6 = fromIntegral (abf `shiftR` 12) :: Word8
      !byte7 = fromIntegral (abf `shiftR` 4) :: Word8
      !byte8 = fromIntegral (abf .&. 0x0F) `shiftL` 4 :: Word8
  pokeByteOff ptr 0 byte0
  pokeByteOff ptr 1 byte1
  pokeByteOff ptr 2 byte2
  pokeByteOff ptr 3 byte3
  pokeByteOff ptr 4 byte4
  pokeByteOff ptr 5 byte5
  pokeByteOff ptr 6 byte6
  pokeByteOff ptr 7 byte7
  pokeByteOff ptr 8 byte8
{-# INLINE serializeHeader #-}

-- | Deserialize a packet header from bytes.
-- Uses optimized direct memory access.
deserializeHeader :: BS.ByteString -> Either String PacketHeader
deserializeHeader !bs
  | BS.length bs < packetHeaderByteSize = Left "Header too short"
  | otherwise =
      let !b0 = BSU.unsafeIndex bs 0
          !b1 = BSU.unsafeIndex bs 1
          !b2 = BSU.unsafeIndex bs 2
          !b3 = BSU.unsafeIndex bs 3
          !b4 = BSU.unsafeIndex bs 4
          !b5 = BSU.unsafeIndex bs 5
          !b6 = BSU.unsafeIndex bs 6
          !b7 = BSU.unsafeIndex bs 7
          !b8 = BSU.unsafeIndex bs 8
          !ptVal = fromIntegral (b0 `shiftR` packetTypeBitShift) :: Int
          !snHi = fromIntegral (b0 .&. 0x0F) :: Word16
          !snMid = fromIntegral b1 :: Word16
          !snLo = fromIntegral (b2 `shiftR` 4) :: Word16
          !sn = (snHi `shiftL` 12) .|. (snMid `shiftL` 4) .|. snLo
          !akHi = fromIntegral (b2 .&. 0x0F) :: Word16
          !akMid = fromIntegral b3 :: Word16
          !akLo = fromIntegral (b4 `shiftR` 4) :: Word16
          !ak = (akHi `shiftL` 12) .|. (akMid `shiftL` 4) .|. akLo
          !abf0 = fromIntegral (b4 .&. 0x0F) :: Word32
          !abf1 = fromIntegral b5 :: Word32
          !abf2 = fromIntegral b6 :: Word32
          !abf3 = fromIntegral b7 :: Word32
          !abf4 = fromIntegral (b8 `shiftR` 4) :: Word32
          !abf = (abf0 `shiftL` 28) .|. (abf1 `shiftL` 20) .|. (abf2 `shiftL` 12) .|. (abf3 `shiftL` 4) .|. abf4
       in if ptVal > fromEnum (maxBound :: PacketType)
            then Left $ "Invalid packet type: " ++ show ptVal
            else
              Right
                PacketHeader
                  { packetType = toEnum ptVal,
                    sequenceNum = SequenceNum sn,
                    ack = SequenceNum ak,
                    ackBitfield = abf
                  }
{-# INLINE deserializeHeader #-}

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
