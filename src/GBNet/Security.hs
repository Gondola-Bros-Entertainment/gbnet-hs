{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : GBNet.Security
-- Description : CRC32C integrity, rate limiting, and connect tokens
--
-- Packet integrity via CRC32C (Castagnoli), connection rate limiting,
-- and netcode.io-style connect token validation.
module GBNet.Security
  ( -- * CRC32C
    crc32c,
    appendCrc32,
    validateAndStripCrc32,
    crc32Size,

    -- * Rate limiting
    RateLimiter (..),
    newRateLimiter,
    rateLimiterAllow,

    -- * Connect tokens
    ConnectToken (..),
    newConnectToken,
    isTokenExpired,
    TokenError (..),
    TokenValidator (..),
    newTokenValidator,
    validateToken,
  )
where

import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Digest.CRC32C as CRC
import Data.Word (Word8, Word32, Word64)
import Foreign.Storable (pokeByteOff)
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import GBNet.Reliability (MonoTime, elapsedMs)

-- | CRC32 checksum size in bytes.
crc32Size :: Int
crc32Size = 4

-- | Compute CRC32C checksum (hardware-accelerated via SSE4.2\/ARMv8 CRC).
crc32c :: BS.ByteString -> Word32
crc32c = CRC.crc32c
{-# INLINE crc32c #-}

-- | Append CRC32C checksum to data (little-endian).
appendCrc32 :: BS.ByteString -> BS.ByteString
appendCrc32 dat =
  let crc = crc32c dat
   in dat <> word32ToLEBytes crc

-- | Validate and strip CRC32C. Returns Nothing if corrupt.
validateAndStripCrc32 :: BS.ByteString -> Maybe BS.ByteString
validateAndStripCrc32 dat
  | BS.length dat < crc32Size = Nothing
  | otherwise =
      let payloadLen = BS.length dat - crc32Size
          payload = BS.take payloadLen dat
          crcBytes = BS.drop payloadLen dat
          expected = word32FromLEBytes crcBytes
          actual = crc32c payload
       in if actual == expected
            then Just payload
            else Nothing

-- | Convert Word32 to little-endian bytes.
-- Uses zero-allocation direct memory writes.
word32ToLEBytes :: Word32 -> BS.ByteString
word32ToLEBytes !w = unsafeCreate crc32Size $ \ptr -> do
  pokeByteOff ptr 0 (fromIntegral w :: Word8)
  pokeByteOff ptr 1 (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff ptr 3 (fromIntegral (w `shiftR` 24) :: Word8)
{-# INLINE word32ToLEBytes #-}

-- | Convert little-endian bytes to Word32.
-- Uses direct memory access for speed.
-- Caller must ensure at least 4 bytes; 'validateAndStripCrc32' guarantees this.
word32FromLEBytes :: BS.ByteString -> Word32
word32FromLEBytes !bs =
  let !b0 = fromIntegral (BSU.unsafeIndex bs 0) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs 1) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs 2) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs 3) :: Word32
   in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
{-# INLINE word32FromLEBytes #-}

-- | Cleanup interval — sweep stale entries every 5 seconds.
cleanupIntervalMs :: Double
cleanupIntervalMs = 5000.0

-- | Rate limiter for connection requests per source.
-- Self-cleaning: stale entries are pruned automatically during 'rateLimiterAllow'.
data RateLimiter = RateLimiter
  { rlRequests :: !(Map Word64 [MonoTime]),
    rlMaxRequestsPerSecond :: !Int,
    rlWindowMs :: !Double,
    rlLastCleanup :: !MonoTime
  }
  deriving (Show)

-- | Create a new rate limiter.
newRateLimiter :: Int -> MonoTime -> RateLimiter
newRateLimiter maxReqs now =
  RateLimiter
    { rlRequests = Map.empty,
      rlMaxRequestsPerSecond = maxReqs,
      rlWindowMs = 1000.0,
      rlLastCleanup = now
    }

-- | Check if a request should be allowed. Returns (allowed, updatedLimiter).
-- Automatically prunes stale entries when the cleanup interval has elapsed.
rateLimiterAllow :: Word64 -> MonoTime -> RateLimiter -> (Bool, RateLimiter)
rateLimiterAllow addrKey now rl
  | recentCount >= rlMaxRequestsPerSecond rl' = (False, rl' {rlRequests = Map.insert addrKey recent (rlRequests rl')})
  | otherwise = (True, rl' {rlRequests = Map.insert addrKey (now : recent) (rlRequests rl')})
  where
    rl' = maybeCleanup now rl
    window = rlWindowMs rl'
    timestamps = Map.findWithDefault [] addrKey (rlRequests rl')
    -- Filter and count in single pass
    (recentCount, recent) = foldr countRecent (0, []) timestamps
    countRecent t (n, acc)
      | elapsedMs t now < window = (n + 1, t : acc)
      | otherwise = (n, acc)

-- | Sweep stale entries if enough time has passed since last cleanup.
maybeCleanup :: MonoTime -> RateLimiter -> RateLimiter
maybeCleanup now rl
  | elapsedMs (rlLastCleanup rl) now < cleanupIntervalMs = rl
  | otherwise =
      let window = rlWindowMs rl
          cleanup = filter (\t -> elapsedMs t now < window)
          cleaned = Map.map cleanup (rlRequests rl)
          nonEmpty = Map.filter (not . null) cleaned
       in rl {rlRequests = nonEmpty, rlLastCleanup = now}

-- | Connect token for authentication.
data ConnectToken = ConnectToken
  { ctClientId :: !Word64,
    ctCreateTime :: !MonoTime,
    ctExpireDurationMs :: !Double,
    ctUserData :: !BS.ByteString
  }
  deriving (Show)

-- | Create a new connect token.
newConnectToken :: Word64 -> Double -> BS.ByteString -> MonoTime -> ConnectToken
newConnectToken clientId expireMs userData now =
  ConnectToken
    { ctClientId = clientId,
      ctCreateTime = now,
      ctExpireDurationMs = expireMs,
      ctUserData = userData
    }

-- | Check if token is expired.
isTokenExpired :: MonoTime -> ConnectToken -> Bool
isTokenExpired now token = elapsedMs (ctCreateTime token) now > ctExpireDurationMs token

-- | Token validation errors.
data TokenError
  = TokenExpired
  | TokenReplayed
  | TokenInvalid
  deriving (Eq, Show)

-- | Server-side token validator.
data TokenValidator = TokenValidator
  { tvUsedTokens :: !(Map Word64 MonoTime),
    tvTokenLifetimeMs :: !Double,
    tvMaxTrackedTokens :: !Int,
    tvTokensEvicted :: !Int
  }
  deriving (Show)

-- | Create a new token validator.
newTokenValidator :: Double -> Int -> TokenValidator
newTokenValidator lifetimeMs maxTracked =
  TokenValidator
    { tvUsedTokens = Map.empty,
      tvTokenLifetimeMs = lifetimeMs,
      tvMaxTrackedTokens = maxTracked,
      tvTokensEvicted = 0
    }

-- | Validate a connect token. Returns (Result, updatedValidator).
validateToken :: ConnectToken -> MonoTime -> TokenValidator -> (Either TokenError Word64, TokenValidator)
validateToken token now tv
  | isTokenExpired now token = (Left TokenExpired, tv)
  | Map.member (ctClientId token) (tvUsedTokens tv) = (Left TokenReplayed, tv)
  | otherwise =
      let tv' =
            tv
              { tvUsedTokens = Map.insert (ctClientId token) now (tvUsedTokens tv)
              }
          tv'' = enforceLimit now tv'
       in (Right (ctClientId token), tv'')

-- | Enforce maximum tracked tokens limit.
enforceLimit :: MonoTime -> TokenValidator -> TokenValidator
enforceLimit now tv
  | Map.size (tvUsedTokens tv) <= tvMaxTrackedTokens tv = tv
  | otherwise =
      -- First try cleanup
      let cleaned = cleanupExpired now tv
       in if Map.size (tvUsedTokens cleaned) <= tvMaxTrackedTokens cleaned
            then cleaned
            else evictOldest cleaned

-- | Remove expired tokens.
cleanupExpired :: MonoTime -> TokenValidator -> TokenValidator
cleanupExpired now tv =
  let lifetime = tvTokenLifetimeMs tv
      keep (_clientId, created) = elapsedMs created now < lifetime
      kept = Map.filterWithKey (curry keep) (tvUsedTokens tv)
   in tv {tvUsedTokens = kept}

-- | Evict oldest token.
evictOldest :: TokenValidator -> TokenValidator
evictOldest tv =
  case findOldest (Map.toList (tvUsedTokens tv)) of
    Nothing -> tv
    Just (oldestId, _) ->
      tv
        { tvUsedTokens = Map.delete oldestId (tvUsedTokens tv),
          tvTokensEvicted = tvTokensEvicted tv + 1
        }
  where
    findOldest [] = Nothing
    findOldest xs = Just $ minimumBy (comparing snd) xs
