{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : GBNet.Crypto
-- Description : ChaCha20-Poly1305 AEAD encryption for packet payloads
--
-- Provides authenticated encryption for post-handshake packets using
-- ChaCha20-Poly1305 (RFC 8439). Pre-shared key model: both sides must
-- configure the same 'EncryptionKey' before connecting.
--
-- Wire format for encrypted packets:
--
-- @
-- [header:9 plaintext][nonce:8 BE][ciphertext:N][auth_tag:16][CRC32C:4]
-- @
--
-- Nonce construction: @[counter:8 BE][protocol_id:4 BE]@ = 12 bytes
-- (ChaCha20 requirement).
module GBNet.Crypto
  ( -- * Types
    EncryptionKey (..),
    NonceCounter (..),
    CryptoError (..),

    -- * Constants
    encryptionKeySize,
    nonceSize,
    authTagSize,
    encryptionOverhead,

    -- * Operations
    encrypt,
    decrypt,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import qualified Crypto.Cipher.ChaChaPoly1305 as CCP
import Crypto.Error (CryptoFailable (..))
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word32, Word64, Word8)
import Foreign.Storable (pokeByteOff)

-- | 32-byte symmetric encryption key for ChaCha20-Poly1305.
newtype EncryptionKey = EncryptionKey {unEncryptionKey :: BS.ByteString}
  deriving (Eq, Show)

instance NFData EncryptionKey where
  rnf (EncryptionKey k) = rnf k

-- | Monotonically increasing nonce counter (one per connection direction).
newtype NonceCounter = NonceCounter {unNonceCounter :: Word64}
  deriving (Eq, Show, Ord)

instance NFData NonceCounter where
  rnf (NonceCounter n) = rnf n

-- | Encryption/decryption errors.
data CryptoError
  = CryptoInitError !String
  | CryptoAuthError
  | CryptoNonceReplay
  deriving (Eq, Show)

instance NFData CryptoError where rnf = rwhnf

-- | Encryption key size in bytes (256 bits).
encryptionKeySize :: Int
encryptionKeySize = 32

-- | Nonce size on wire in bytes (8-byte counter, big-endian).
nonceSize :: Int
nonceSize = 8

-- | Authentication tag size in bytes (Poly1305).
authTagSize :: Int
authTagSize = 16

-- | Total overhead added by encryption: nonce (8) + auth tag (16) = 24 bytes.
-- (CRC32C is added separately by the send path.)
encryptionOverhead :: Int
encryptionOverhead = nonceSize + authTagSize

-- | Encrypt a plaintext payload.
--
-- Returns nonce counter prepended to ciphertext + auth tag:
-- @[nonce:8 BE][ciphertext:N][auth_tag:16]@
--
-- The caller is responsible for incrementing the nonce counter after calling.
encrypt ::
  EncryptionKey ->
  NonceCounter ->
  Word32 ->
  BS.ByteString ->
  Either CryptoError BS.ByteString
encrypt (EncryptionKey key) (NonceCounter counter) protocolId plaintext =
  let !nonceBytes = buildNonce counter protocolId
   in case CCP.nonce12 nonceBytes of
        CryptoFailed err -> Left (CryptoInitError (show err))
        CryptoPassed nonce -> case CCP.initialize key nonce of
          CryptoFailed err -> Left (CryptoInitError (show err))
          CryptoPassed st0 ->
            let !st1 = CCP.finalizeAAD st0
                (!ciphertext, !st2) = CCP.encrypt plaintext st1
                !authTag = CCP.finalize st2
                !tagBytes = BA.convert authTag :: BS.ByteString
                !counterBytes = word64ToBE counter
             in Right (counterBytes <> ciphertext <> tagBytes)

-- | Decrypt a ciphertext payload.
--
-- Input format: @[nonce:8 BE][ciphertext:N][auth_tag:16]@
--
-- Returns the decrypted plaintext and the nonce counter from the packet,
-- or an error if authentication fails or the packet is too short.
--
-- Anti-replay: the caller should check that the returned 'NonceCounter'
-- is greater than the previously seen maximum.
decrypt ::
  EncryptionKey ->
  Word32 ->
  BS.ByteString ->
  Either CryptoError (BS.ByteString, NonceCounter)
decrypt (EncryptionKey key) protocolId dat
  | BS.length dat < nonceSize + authTagSize = Left CryptoAuthError
  | otherwise =
      let !counterBytes = BS.take nonceSize dat
          !counter = word64FromBE counterBytes
          !rest = BS.drop nonceSize dat
          !ciphertextLen = BS.length rest - authTagSize
          !ciphertext = BS.take ciphertextLen rest
          !tagBytes = BS.drop ciphertextLen rest
          !nonceBytes = buildNonce counter protocolId
       in case CCP.nonce12 nonceBytes of
            CryptoFailed err -> Left (CryptoInitError (show err))
            CryptoPassed nonce -> case CCP.initialize key nonce of
              CryptoFailed err -> Left (CryptoInitError (show err))
              CryptoPassed st0 ->
                let !st1 = CCP.finalizeAAD st0
                    (!plaintext, !st2) = CCP.decrypt ciphertext st1
                    !computedTag = CCP.finalize st2
                    !computedBytes = BA.convert computedTag :: BS.ByteString
                 in if BA.constEq computedBytes tagBytes
                      then Right (plaintext, NonceCounter counter)
                      else Left CryptoAuthError

-- | Build a 12-byte nonce from counter (8 bytes BE) and protocol ID (4 bytes BE).
buildNonce :: Word64 -> Word32 -> BS.ByteString
buildNonce counter protocolId =
  word64ToBE counter <> word32ToBE protocolId
{-# INLINE buildNonce #-}

-- | Encode a Word64 as 8 bytes big-endian.
word64ToBE :: Word64 -> BS.ByteString
word64ToBE !w = unsafeCreate nonceSize $ \ptr -> do
  pokeByteOff ptr 0 (fromIntegral (w `shiftR` 56) :: Word8)
  pokeByteOff ptr 1 (fromIntegral (w `shiftR` 48) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (w `shiftR` 40) :: Word8)
  pokeByteOff ptr 3 (fromIntegral (w `shiftR` 32) :: Word8)
  pokeByteOff ptr 4 (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff ptr 5 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff ptr 6 (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff ptr 7 (fromIntegral w :: Word8)
{-# INLINE word64ToBE #-}

-- | Decode 8 bytes big-endian to Word64.
word64FromBE :: BS.ByteString -> Word64
word64FromBE !bs =
  let !b0 = fromIntegral (BSU.unsafeIndex bs 0) :: Word64
      !b1 = fromIntegral (BSU.unsafeIndex bs 1) :: Word64
      !b2 = fromIntegral (BSU.unsafeIndex bs 2) :: Word64
      !b3 = fromIntegral (BSU.unsafeIndex bs 3) :: Word64
      !b4 = fromIntegral (BSU.unsafeIndex bs 4) :: Word64
      !b5 = fromIntegral (BSU.unsafeIndex bs 5) :: Word64
      !b6 = fromIntegral (BSU.unsafeIndex bs 6) :: Word64
      !b7 = fromIntegral (BSU.unsafeIndex bs 7) :: Word64
   in (b0 `shiftL` 56)
        .|. (b1 `shiftL` 48)
        .|. (b2 `shiftL` 40)
        .|. (b3 `shiftL` 32)
        .|. (b4 `shiftL` 24)
        .|. (b5 `shiftL` 16)
        .|. (b6 `shiftL` 8)
        .|. b7
{-# INLINE word64FromBE #-}

-- | Encode a Word32 as 4 bytes big-endian.
word32ToBE :: Word32 -> BS.ByteString
word32ToBE !w = unsafeCreate 4 $ \ptr -> do
  pokeByteOff ptr 0 (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff ptr 1 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff ptr 3 (fromIntegral w :: Word8)
{-# INLINE word32ToBE #-}
