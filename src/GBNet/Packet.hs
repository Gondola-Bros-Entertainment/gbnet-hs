-- |
-- Module      : GBNet.Packet
-- Description : Packet types and header definitions for the wire protocol
--
-- Haskell equivalent of @gbnet::packet@ from the Rust library. Defines the
-- 'PacketType' enumeration and 'PacketHeader' structure that form the
-- foundation of the gbnet wire format.
--
-- == Wire Format
--
-- Every gbnet packet starts with a fixed-size header:
--
-- @
--   ┌──────────────────────────────────────────────────────────────┐
--   │  Bits 0-3     │ PacketType (4 bits)                         │
--   │  Bits 4-19    │ Sequence number (16 bits)                   │
--   │  Bits 20-35   │ Ack number (16 bits)                        │
--   │  Bits 36-67   │ Ack bitfield (32 bits)                      │
--   └──────────────────────────────────────────────────────────────┘
--   Total: 68 bits (9 bytes on the wire, with 4 padding bits)
-- @
--
-- This matches the Rust @PacketHeader@ struct in @packet.rs@.
--
-- == Packet Types
--
-- The protocol uses 6 packet types, encoded as a 4-bit integer (0–5):
--
--   * 'ConnectionRequest' (0) — Client initiates a connection
--   * 'ConnectionAccepted' (1) — Server accepts the connection
--   * 'ConnectionDenied' (2) — Server rejects (full, bad token, etc.)
--   * 'Payload' (3) — Normal game data
--   * 'Disconnect' (4) — Graceful disconnect
--   * 'Keepalive' (5) — Empty packet to maintain the connection
--
-- == Ack Bitfield
--
-- The @ack@ and @ackBitfield@ fields implement the acknowledgment
-- scheme used for reliable delivery. @ack@ is the most recently received
-- sequence number; @ackBitfield@ has one bit for each of the 32
-- preceding sequence numbers. If bit @i@ is set, then sequence number
-- @ack - 1 - i@ has been received. This allows acknowledging up to 33
-- packets in a single header with zero additional overhead.
--
-- In the Rust library, this is implemented in @reliability.rs@ and
-- @connection.rs@. The same scheme is used by Gaffer On Games' "reliable
-- UDP" protocol and ENet.

module GBNet.Packet
  ( -- * Packet type enumeration
    PacketType(..)

    -- * Packet header
  , PacketHeader(..)

    -- * Constants
  , packetHeaderBitSize
  ) where

import Data.Word (Word16, Word32)
import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class

-- | Number of bits in a serialized 'PacketHeader'.
--
-- 4 (type) + 16 (sequence) + 16 (ack) + 32 (ack bitfield) = 68 bits.
-- Named constant avoids magic numbers in tests and serialization code.
packetHeaderBitSize :: Int
packetHeaderBitSize = 68

-- | Packet type tag, encoded as a 4-bit unsigned integer on the wire.
--
-- Uses Haskell's algebraic data type (ADT) syntax — the Haskell
-- equivalent of Rust's @enum@. Each constructor is a distinct value
-- with no associated data (a "C-style" enum).
--
-- 'Eq' lets us compare with @==@; 'Show' provides debug printing;
-- 'Enum' gives us 'fromEnum' / 'toEnum' for converting to/from
-- integers; 'Bounded' provides 'minBound' / 'maxBound'.
data PacketType
  = ConnectionRequest   -- ^ 0: Client -> Server. Initiates connection handshake.
  | ConnectionAccepted  -- ^ 1: Server -> Client. Connection approved.
  | ConnectionDenied    -- ^ 2: Server -> Client. Connection rejected.
  | Payload             -- ^ 3: Bidirectional. Normal game data.
  | Disconnect          -- ^ 4: Bidirectional. Graceful shutdown.
  | Keepalive           -- ^ 5: Bidirectional. Prevents timeout during idle.
  deriving (Eq, Show, Enum, Bounded)
  -- ^ 'deriving (Enum)' auto-generates:
  --     @fromEnum ConnectionRequest == 0@
  --     @fromEnum Keepalive == 5@
  --     @toEnum 3 == Payload@
  --   This relies on the constructors being listed in order 0..5.

-- | Serialization: write the packet type as 4 bits.
--
-- 'fromEnum' converts the constructor to its ordinal position (0–5),
-- then 'fromIntegral' widens to 'Word64' for 'writeBits'.
-- 4 bits can encode values 0–15, so 6 packet types fit comfortably.
instance BitSerialize PacketType where
  bitSerialize pt = writeBits (fromIntegral (fromEnum pt)) packetTypeBitWidth

-- | Deserialization: read 4 bits and convert to a 'PacketType'.
--
-- We validate that the tag is within the valid range (0–5) before
-- calling 'toEnum'. Without this check, an invalid tag would cause
-- a runtime exception from 'toEnum'.
instance BitDeserialize PacketType where
  bitDeserialize buf =
    case readBits packetTypeBitWidth buf of
      Left err -> Left err
      Right (ReadResult val buf') ->
        let tag = fromIntegral val :: Int
        in if tag > fromEnum (maxBound :: PacketType)
           then Left $ "bitDeserialize PacketType: invalid tag " ++ show tag
           else Right $ ReadResult
                  { readValue  = toEnum tag
                  , readBuffer = buf'
                  }

-- | Packet header — the first thing serialized in every gbnet packet.
--
-- Matches the Rust @PacketHeader@ struct:
--
-- @
--   pub struct PacketHeader {
--       pub packet_type: PacketType,
--       pub sequence: u16,
--       pub ack: u16,
--       pub ack_bitfield: u32,
--   }
-- @
--
-- The Haskell version uses record syntax, which auto-generates accessor
-- functions (e.g., @packetType :: PacketHeader -> PacketType@). The @!@
-- annotations make all fields strict to avoid space leaks — important
-- for a networking library processing many packets per frame.
data PacketHeader = PacketHeader
  { packetType   :: !PacketType
    -- ^ What kind of packet this is (4 bits on the wire).
  , sequenceNum  :: !Word16
    -- ^ Monotonically increasing per-connection counter (16 bits).
    -- Wraps around at 65535 -> 0. Used for ordering, dedup, and acks.
  , ack          :: !Word16
    -- ^ The most recent remote sequence number we've received (16 bits).
  , ackBitfield  :: !Word32
    -- ^ Bitfield of 32 preceding acks (32 bits). Bit @i@ set means
    -- we've received sequence number @ack - 1 - i@.
  }
  deriving (Eq, Show)

-- | Serialize a 'PacketHeader' by writing each field in wire order.
--
-- Fields are written in the order they appear in the wire format diagram
-- at the top of this module. The @$@ chains compose right-to-left
-- (innermost first), so the last 'bitSerialize' call writes first.
-- This means we write: type, sequence, ack, ack bitfield.
instance BitSerialize PacketHeader where
  bitSerialize hdr =
      bitSerialize (ackBitfield hdr)
    . bitSerialize (ack hdr)
    . bitSerialize (sequenceNum hdr)
    . bitSerialize (packetType hdr)
    -- ^ Function composition with @.@ — read bottom-to-top:
    --   1. Serialize packetType
    --   2. Serialize sequenceNum
    --   3. Serialize ack
    --   4. Serialize ackBitfield
    -- This is equivalent to the Rust version's sequential writes.

-- | Deserialize a 'PacketHeader' by reading fields in wire order.
--
-- Each @case@ reads one field and threads the updated buffer to the
-- next read. This is the "nested case" pattern that the 'BitReader'
-- monad (from "GBNet.Serialize.Reader") was designed to eliminate.
-- We use the explicit style here for clarity and to avoid a circular
-- dependency (Packet doesn't import Reader).
instance BitDeserialize PacketHeader where
  bitDeserialize buf =
    case bitDeserialize buf of
      Left err -> Left err
      Right (ReadResult pt buf1) ->
        case bitDeserialize buf1 of
          Left err -> Left err
          Right (ReadResult sn buf2) ->
            case bitDeserialize buf2 of
              Left err -> Left err
              Right (ReadResult ak buf3) ->
                case bitDeserialize buf3 of
                  Left err -> Left err
                  Right (ReadResult abf buf4) ->
                    Right $ ReadResult
                      { readValue = PacketHeader
                          { packetType  = pt
                          , sequenceNum = sn
                          , ack         = ak
                          , ackBitfield = abf
                          }
                      , readBuffer = buf4
                      }
