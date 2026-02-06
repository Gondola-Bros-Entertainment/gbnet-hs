-- |
-- Module      : GBNet.Peer.Protocol
-- Description : Pure encoding/decoding utilities for Peer protocol
--
-- Salt encoding, deny reasons, payload header decoding, channel sequence
-- decoding, and FNV-1a hashing. No dependencies on other Peer sub-modules.
module GBNet.Peer.Protocol
  ( -- * Salt encoding
    encodeSalt,
    decodeSalt,

    -- * Deny reasons
    DenyReason (..),
    encodeDenyReason,
    decodeDenyReason,
    denyToDisconnectReason,

    -- * Payload header
    decodePayloadHeader,
    payloadFragmentFlag,
    payloadChannelMask,
    minPayloadSize,

    -- * Channel sequence
    decodeChannelSeq,

    -- * FNV-1a hashing
    sockAddrToKey,
    fnvMix,
    fnvOffsetBasis,
    fnvPrime,
  )
where

import Data.Bits (shiftL, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64, Word8)
import GBNet.Connection (DisconnectReason (..))
import GBNet.Serialize (deserialize, serialize)
import GBNet.Types (ChannelId (..), SequenceNum (..))
import Network.Socket (PortNumber, SockAddr (..))

-- | Encode a Word64 salt to bytes.
encodeSalt :: Word64 -> BS.ByteString
encodeSalt = serialize
{-# INLINE encodeSalt #-}

-- | Decode a Word64 salt from bytes.
decodeSalt :: BS.ByteString -> Maybe Word64
decodeSalt bs = case deserialize bs of
  Left _ -> Nothing
  Right v -> Just v
{-# INLINE decodeSalt #-}

-- | Deny reason codes sent during handshake rejection.
data DenyReason
  = DenyServerFull
  | DenyInvalidChallenge
  | DenyUnknown !Word8
  deriving (Eq, Show)

-- | Encode a deny reason to bytes.
encodeDenyReason :: DenyReason -> BS.ByteString
encodeDenyReason DenyServerFull = BS.singleton 1
encodeDenyReason DenyInvalidChallenge = BS.singleton 2
encodeDenyReason (DenyUnknown code) = BS.singleton code

-- | Decode a deny reason from bytes.
decodeDenyReason :: BS.ByteString -> DenyReason
decodeDenyReason bs = case BS.uncons bs of
  Just (1, _) -> DenyServerFull
  Just (2, _) -> DenyInvalidChallenge
  Just (code, _) -> DenyUnknown code
  Nothing -> DenyUnknown 0

-- | Convert a deny reason to a disconnect reason for event reporting.
denyToDisconnectReason :: DenyReason -> DisconnectReason
denyToDisconnectReason DenyServerFull = ReasonServerFull
denyToDisconnectReason DenyInvalidChallenge = ReasonProtocolMismatch
denyToDisconnectReason (DenyUnknown code) = ReasonUnknown code

-- | Payload header: channel (3 bits) + is_fragment (1 bit) + reserved (4 bits)
-- Encoded in first byte: [is_fragment:1][reserved:4][channel:3]
-- Channel is low 3 bits, is_fragment is high bit (0x80)

-- | Fragment flag bit in payload header.
payloadFragmentFlag :: Word8
payloadFragmentFlag = 0x80

-- | Channel mask in payload header.
payloadChannelMask :: Word8
payloadChannelMask = 0x07

-- | Decode payload header byte.
decodePayloadHeader :: Word8 -> (ChannelId, Bool)
decodePayloadHeader b =
  let channel = ChannelId (b .&. payloadChannelMask)
      isFragment = (b .&. payloadFragmentFlag) /= 0
   in (channel, isFragment)
{-# INLINE decodePayloadHeader #-}

-- | Minimum size of a non-fragment payload: headerByte + 2 bytes channel sequence.
minPayloadSize :: Int
minPayloadSize = 3

-- | Decode channel sequence from payload bytes.
-- Returns (channelSeq, remaining data) or Nothing if too short.
decodeChannelSeq :: BS.ByteString -> Maybe (SequenceNum, BS.ByteString)
decodeChannelSeq bs
  | BS.length bs < 2 = Nothing
  | otherwise =
      let chSeq =
            SequenceNum $
              (fromIntegral (BS.index bs 0) `shiftL` 8)
                .|. fromIntegral (BS.index bs 1)
       in Just (chSeq, BS.drop 2 bs)
{-# INLINE decodeChannelSeq #-}

-- | FNV-1a hash seed.
fnvOffsetBasis :: Word64
fnvOffsetBasis = 14695981039346656037

-- | FNV-1a hash prime.
fnvPrime :: Word64
fnvPrime = 1099511628211

-- | Convert SockAddr to a key for rate limiting using FNV-1a hash.
-- Hashes the raw address/port words directly, avoiding 'show'.
sockAddrToKey :: SockAddr -> Word64
sockAddrToKey addr = case addr of
  SockAddrInet port host ->
    fnvMix (fnvMix fnvOffsetBasis (fromIntegral host)) (fromIntegral (portToWord port))
  SockAddrInet6 port _ (h1, h2, h3, h4) _ ->
    foldl'
      fnvMix
      fnvOffsetBasis
      [ fromIntegral (portToWord port),
        fromIntegral h1,
        fromIntegral h2,
        fromIntegral h3,
        fromIntegral h4
      ]
  _ -> fnvMix fnvOffsetBasis 0
  where
    portToWord :: PortNumber -> Word64
    portToWord = fromIntegral
    {-# INLINE portToWord #-}

-- | FNV-1a mix step: XOR then multiply.
fnvMix :: Word64 -> Word64 -> Word64
fnvMix h val = (h `xor` val) * fnvPrime
{-# INLINE fnvMix #-}
