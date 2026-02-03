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
    crc32cPolynomial,
    crc32Size,

    -- * Rate limiting
    RateLimiter (..),
    newRateLimiter,
    rateLimiterAllow,
    rateLimiterCleanup,

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

import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64, Word8)
import GBNet.Reliability (MonoTime, elapsedMs)

-- CRC32C Constants

-- | CRC-32C (Castagnoli) polynomial (iSCSI standard).
crc32cPolynomial :: Word32
crc32cPolynomial = 0x82F63B78

-- | CRC32 checksum size in bytes.
crc32Size :: Int
crc32Size = 4

-- | Compute CRC32C checksum.
crc32c :: BS.ByteString -> Word32
crc32c dat = complement $ BS.foldl' processByte 0xFFFFFFFF dat
  where
    processByte :: Word32 -> Word8 -> Word32
    processByte crc byte =
      let crc' = crc `xor` fromIntegral byte
       in foldl' processBit crc' [0 .. 7 :: Int]

    processBit :: Word32 -> Int -> Word32
    processBit crc _ =
      if crc .&. 1 /= 0
        then (crc `shiftR` 1) `xor` crc32cPolynomial
        else crc `shiftR` 1

    foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' _ z [] = z
    foldl' f z (x : xs) = let z' = f z x in z' `seq` foldl' f z' xs

    complement :: Word32 -> Word32
    complement x = x `xor` 0xFFFFFFFF

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
word32ToLEBytes :: Word32 -> BS.ByteString
word32ToLEBytes w =
  BS.pack
    [ fromIntegral (w .&. 0xFF),
      fromIntegral ((w `shiftR` 8) .&. 0xFF),
      fromIntegral ((w `shiftR` 16) .&. 0xFF),
      fromIntegral ((w `shiftR` 24) .&. 0xFF)
    ]

-- | Convert little-endian bytes to Word32.
word32FromLEBytes :: BS.ByteString -> Word32
word32FromLEBytes bs
  | BS.length bs < 4 = 0
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Word32
          b1 = fromIntegral (BS.index bs 1) :: Word32
          b2 = fromIntegral (BS.index bs 2) :: Word32
          b3 = fromIntegral (BS.index bs 3) :: Word32
       in b0 + b1 * 256 + b2 * 65536 + b3 * 16777216

-- | Rate limiter for connection requests per source.
data RateLimiter = RateLimiter
  { rlRequests :: !(Map Word64 [MonoTime]), -- Key is hashed address
    rlMaxRequestsPerSecond :: !Int,
    rlWindowMs :: !Double
  }
  deriving (Show)

-- | Create a new rate limiter.
newRateLimiter :: Int -> RateLimiter
newRateLimiter maxReqs =
  RateLimiter
    { rlRequests = Map.empty,
      rlMaxRequestsPerSecond = maxReqs,
      rlWindowMs = 1000.0
    }

-- | Check if a request should be allowed. Returns (allowed, updatedLimiter).
rateLimiterAllow :: Word64 -> MonoTime -> RateLimiter -> (Bool, RateLimiter)
rateLimiterAllow addrKey now rl =
  let window = rlWindowMs rl
      timestamps = Map.findWithDefault [] addrKey (rlRequests rl)
      -- Filter to recent timestamps
      recent = filter (\t -> elapsedMs t now < window) timestamps
   in if length recent >= rlMaxRequestsPerSecond rl
        then (False, rl {rlRequests = Map.insert addrKey recent (rlRequests rl)})
        else
          ( True,
            rl {rlRequests = Map.insert addrKey (now : recent) (rlRequests rl)}
          )

-- | Clean up old entries.
rateLimiterCleanup :: MonoTime -> RateLimiter -> RateLimiter
rateLimiterCleanup now rl =
  let window = rlWindowMs rl
      cleanup = filter (\t -> elapsedMs t now < window)
      cleaned = Map.map cleanup (rlRequests rl)
      -- Remove empty entries
      nonEmpty = Map.filter (not . null) cleaned
   in rl {rlRequests = nonEmpty}

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
    findOldest xs = Just $ foldr1 (\a b -> if snd a < snd b then a else b) xs
