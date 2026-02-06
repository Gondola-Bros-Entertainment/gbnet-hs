{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Fragment
-- Description : Message fragmentation, reassembly, and MTU discovery
--
-- Splits large messages into fragments that fit within MTU, reassembles
-- incoming fragments, and discovers path MTU via binary search probing.
module GBNet.Fragment
  ( -- * Constants
    fragmentHeaderSize,
    maxFragmentCount,
    defaultProbeTimeoutMs,
    defaultMaxProbeAttempts,
    minMtu,
    maxMtu,
    mtuConvergenceThreshold,

    -- * Fragment header
    FragmentHeader (..),
    serializeFragmentHeader,
    deserializeFragmentHeader,

    -- * Fragmentation
    FragmentError (..),
    fragmentMessage,

    -- * Reassembly
    FragmentAssembler (..),
    newFragmentAssembler,
    processFragment,
    cleanupFragments,

    -- * MTU discovery
    MtuDiscovery (..),
    newMtuDiscovery,
    defaultMtuDiscovery,
    nextProbe,
    onProbeSuccess,
    onProbeTimeout,
    checkProbeTimeout,
    discoveredMtu,
    mtuIsComplete,
  )
where

import Control.Monad (when)
import Control.Monad.State.Strict (modify', runState)
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import qualified Data.ByteString.Unsafe as BSU
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Word (Word32, Word8)
import Foreign.Storable (pokeByteOff)
import GBNet.Reliability (MonoTime, elapsedMs)
import GBNet.Types (MessageId (..))
import Optics ((%~), (&), (.~), (?~))
import Optics.State (use)
import Optics.State.Operators ((%=))
import Optics.TH (makeFieldLabelsNoPrefix)

-- Constants

-- | Fragment header size: message_id (4) + fragment_index (1) + fragment_count (1) = 6 bytes.
fragmentHeaderSize :: Int
fragmentHeaderSize = 6

-- | Maximum fragments per message.
maxFragmentCount :: Int
maxFragmentCount = 255

-- | Default probe timeout in milliseconds.
defaultProbeTimeoutMs :: Double
defaultProbeTimeoutMs = 500.0

-- | Default maximum probe attempts.
defaultMaxProbeAttempts :: Int
defaultMaxProbeAttempts = 10

-- | Minimum safe MTU.
minMtu :: Int
minMtu = 576

-- | Maximum typical MTU.
maxMtu :: Int
maxMtu = 1500

-- | MTU convergence threshold (stop when range is this small).
mtuConvergenceThreshold :: Int
mtuConvergenceThreshold = 1

-- | Fragment header.
data FragmentHeader = FragmentHeader
  { fhMessageId :: !MessageId,
    fhFragmentIndex :: !Word8,
    fhFragmentCount :: !Word8
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''FragmentHeader

-- | Serialize fragment header to bytes.
-- Uses zero-allocation direct memory writes.
serializeFragmentHeader :: FragmentHeader -> BS.ByteString
serializeFragmentHeader !hdr = unsafeCreate fragmentHeaderSize $ \ptr -> do
  let !msgId = unMessageId (fhMessageId hdr)
  pokeByteOff ptr 0 (fromIntegral (msgId `shiftR` 24) :: Word8)
  pokeByteOff ptr 1 (fromIntegral (msgId `shiftR` 16) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (msgId `shiftR` 8) :: Word8)
  pokeByteOff ptr 3 (fromIntegral msgId :: Word8)
  pokeByteOff ptr 4 (fhFragmentIndex hdr)
  pokeByteOff ptr 5 (fhFragmentCount hdr)
{-# INLINE serializeFragmentHeader #-}

-- | Deserialize fragment header from bytes.
-- Uses direct memory access for speed.
deserializeFragmentHeader :: BS.ByteString -> Maybe FragmentHeader
deserializeFragmentHeader !bs
  | BS.length bs < fragmentHeaderSize = Nothing
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs 0) :: Word32
          !b1 = fromIntegral (BSU.unsafeIndex bs 1) :: Word32
          !b2 = fromIntegral (BSU.unsafeIndex bs 2) :: Word32
          !b3 = fromIntegral (BSU.unsafeIndex bs 3) :: Word32
          !msgId = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
       in Just
            FragmentHeader
              { fhMessageId = MessageId msgId,
                fhFragmentIndex = BSU.unsafeIndex bs 4,
                fhFragmentCount = BSU.unsafeIndex bs 5
              }
{-# INLINE deserializeFragmentHeader #-}

-- | Fragmentation errors.
data FragmentError
  = TooManyFragments
  deriving (Eq, Show)

-- | Split a message into fragments.
fragmentMessage :: MessageId -> BS.ByteString -> Int -> Either FragmentError [BS.ByteString]
fragmentMessage messageId dat maxFragmentPayload
  | BS.null dat || maxFragmentPayload <= 0 = Right []
  | fragCount > maxFragmentCount = Left TooManyFragments
  | otherwise = Right $ map makeFragment [0 .. fragCount - 1]
  where
    fragCount = (BS.length dat + maxFragmentPayload - 1) `div` maxFragmentPayload

    makeFragment :: Int -> BS.ByteString
    makeFragment i =
      let start = i * maxFragmentPayload
          end = min ((i + 1) * maxFragmentPayload) (BS.length dat)
          header =
            FragmentHeader
              { fhMessageId = messageId,
                fhFragmentIndex = fromIntegral i,
                fhFragmentCount = fromIntegral fragCount
              }
       in serializeFragmentHeader header <> BS.take (end - start) (BS.drop start dat)

-- | Buffer for reassembling a single fragmented message.
data FragmentBuffer = FragmentBuffer
  { fbFragments :: !(Map Word8 BS.ByteString),
    fbFragmentCount :: !Word8,
    fbCreatedAt :: !MonoTime,
    fbTotalSize :: !Int
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''FragmentBuffer

-- | Create a new fragment buffer.
newFragmentBuffer :: Word8 -> MonoTime -> FragmentBuffer
newFragmentBuffer count now =
  FragmentBuffer
    { fbFragments = Map.empty,
      fbFragmentCount = count,
      fbCreatedAt = now,
      fbTotalSize = 0
    }

-- | Insert a fragment. Returns (isComplete, updatedBuffer).
insertFragment :: Word8 -> BS.ByteString -> FragmentBuffer -> (Bool, FragmentBuffer)
insertFragment idx dat buf
  | idx >= fbFragmentCount buf = (False, buf)
  | Map.member idx (fbFragments buf) = (isComplete buf, buf) -- Already have this fragment
  | otherwise =
      let buf' =
            buf
              & #fbFragments
              %~ Map.insert idx dat
              & #fbTotalSize
              %~ (+ BS.length dat)
       in (isComplete buf', buf')

-- | Check if all fragments received.
isComplete :: FragmentBuffer -> Bool
isComplete buf = Map.size (fbFragments buf) == fromIntegral (fbFragmentCount buf)

-- | Assemble complete message from fragments.
assembleFragments :: FragmentBuffer -> Maybe BS.ByteString
assembleFragments buf
  | not (isComplete buf) = Nothing
  | otherwise =
      let indices = [0 .. fbFragmentCount buf - 1]
          fragments = mapM (`Map.lookup` fbFragments buf) indices
       in BS.concat <$> fragments

-- | Fragment reassembler managing multiple in-progress messages.
data FragmentAssembler = FragmentAssembler
  { faBuffers :: !(Map MessageId FragmentBuffer),
    faTimeoutMs :: !Double,
    faMaxBufferSize :: !Int,
    faCurrentBufferSize :: !Int
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''FragmentAssembler

-- | Create a new fragment assembler.
newFragmentAssembler :: Double -> Int -> FragmentAssembler
newFragmentAssembler timeoutMs maxSize =
  FragmentAssembler
    { faBuffers = Map.empty,
      faTimeoutMs = timeoutMs,
      faMaxBufferSize = maxSize,
      faCurrentBufferSize = 0
    }

-- | Process an incoming fragment. Returns reassembled message if complete.
processFragment :: BS.ByteString -> MonoTime -> FragmentAssembler -> (Maybe BS.ByteString, FragmentAssembler)
processFragment dat now = runState $ do
  modify' (cleanupFragments now)
  case deserializeFragmentHeader dat of
    Nothing -> pure Nothing
    Just header -> do
      let fragData = BS.drop fragmentHeaderSize dat
          fragSize = BS.length fragData
          msgId = fhMessageId header
      -- Check if existing buffer has mismatched count
      buffers <- use #faBuffers
      case Map.lookup msgId buffers of
        Just existing
          | fbFragmentCount existing /= fhFragmentCount header ->
              pure Nothing
        _ -> do
          -- Enforce memory limit
          currentSize <- use #faCurrentBufferSize
          maxSize <- use #faMaxBufferSize
          when (currentSize + fragSize > maxSize) $
            modify' expireOldest
          -- Get or create buffer
          bufs <- use #faBuffers
          let buf = case Map.lookup msgId bufs of
                Just b -> b
                Nothing -> newFragmentBuffer (fhFragmentCount header) now
          -- Insert fragment
          let (complete, buf') = insertFragment (fhFragmentIndex header) fragData buf
          #faBuffers %= Map.insert msgId buf'
          #faCurrentBufferSize %= (+ fragSize)
          if complete
            then do
              let result = assembleFragments buf'
              #faBuffers %= Map.delete msgId
              #faCurrentBufferSize %= subtract (fbTotalSize buf')
              pure result
            else pure Nothing

-- | Remove expired incomplete fragment buffers.
cleanupFragments :: MonoTime -> FragmentAssembler -> FragmentAssembler
cleanupFragments now asm =
  let timeout = faTimeoutMs asm
      (expired, kept) = Map.partition (\buf -> elapsedMs (fbCreatedAt buf) now >= timeout) (faBuffers asm)
      removedSize = sum $ map fbTotalSize $ Map.elems expired
   in asm
        & #faBuffers
        .~ kept
        & #faCurrentBufferSize
        %~ subtract removedSize

-- | Expire the oldest buffer to make room.
expireOldest :: FragmentAssembler -> FragmentAssembler
expireOldest asm =
  case findOldest (faBuffers asm) of
    Nothing -> asm
    Just (oldestId, oldestBuf) ->
      asm
        & #faBuffers
        %~ Map.delete oldestId
        & #faCurrentBufferSize
        %~ subtract (fbTotalSize oldestBuf)
  where
    findOldest :: Map MessageId FragmentBuffer -> Maybe (MessageId, FragmentBuffer)
    findOldest m =
      case Map.toList m of
        [] -> Nothing
        xs -> Just $ minimumBy (comparing (fbCreatedAt . snd)) xs

-- | MTU discovery state.
data MtuState
  = MtuProbing
  | MtuComplete
  deriving (Eq, Show)

-- | MTU discovery via binary search.
data MtuDiscovery = MtuDiscovery
  { mdMinMtu :: !Int,
    mdMaxMtu :: !Int,
    mdCurrentProbe :: !Int,
    mdDiscoveredMtu :: !Int,
    mdState :: !MtuState,
    mdProbeTimeoutMs :: !Double,
    mdLastProbeTime :: !(Maybe MonoTime),
    mdAttempts :: !Int,
    mdMaxAttempts :: !Int
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''MtuDiscovery

-- | Create MTU discovery with custom bounds.
newMtuDiscovery :: Int -> Int -> MtuDiscovery
newMtuDiscovery minM maxM =
  let initialProbe = (minM + maxM) `div` 2
   in MtuDiscovery
        { mdMinMtu = minM,
          mdMaxMtu = maxM,
          mdCurrentProbe = initialProbe,
          mdDiscoveredMtu = minM, -- Start with safe default
          mdState = MtuProbing,
          mdProbeTimeoutMs = defaultProbeTimeoutMs,
          mdLastProbeTime = Nothing,
          mdAttempts = 0,
          mdMaxAttempts = defaultMaxProbeAttempts
        }

-- | Create MTU discovery with default bounds.
defaultMtuDiscovery :: MtuDiscovery
defaultMtuDiscovery = newMtuDiscovery minMtu maxMtu

-- | Get next probe size, or Nothing if complete.
nextProbe :: MonoTime -> MtuDiscovery -> (Maybe Int, MtuDiscovery)
nextProbe now md
  | mdState md == MtuComplete || mdAttempts md >= mdMaxAttempts md =
      (Nothing, md & #mdState .~ MtuComplete)
  | mdMaxMtu md - mdMinMtu md <= mtuConvergenceThreshold =
      (Nothing, md & #mdState .~ MtuComplete)
  | otherwise =
      case mdLastProbeTime md of
        Just lastTime
          | elapsedMs lastTime now < mdProbeTimeoutMs md ->
              (Nothing, md)
        _ ->
          let probe = (mdMinMtu md + mdMaxMtu md) `div` 2
              md' =
                md
                  & #mdCurrentProbe
                  .~ probe
                  & #mdLastProbeTime
                  ?~ now
                  & #mdAttempts
                  %~ (+ 1)
           in (Just probe, md')

-- | Called when probe succeeded (ack received).
onProbeSuccess :: Int -> MtuDiscovery -> MtuDiscovery
onProbeSuccess size md
  | size >= mdMinMtu md =
      md
        & #mdDiscoveredMtu
        .~ size
        & #mdMinMtu
        .~ size
  | otherwise = md

-- | Called when probe timed out (too large).
onProbeTimeout :: MtuDiscovery -> MtuDiscovery
onProbeTimeout md = md & #mdMaxMtu .~ mdCurrentProbe md

-- | Check if current probe timed out.
checkProbeTimeout :: MonoTime -> MtuDiscovery -> MtuDiscovery
checkProbeTimeout now md
  | mdState md == MtuComplete = md
  | otherwise =
      case mdLastProbeTime md of
        Just lastTime
          | elapsedMs lastTime now >= mdProbeTimeoutMs md ->
              onProbeTimeout md
        _ -> md

-- | Get discovered MTU.
discoveredMtu :: MtuDiscovery -> Int
discoveredMtu = mdDiscoveredMtu

-- | Check if MTU discovery is complete.
mtuIsComplete :: MtuDiscovery -> Bool
mtuIsComplete md = mdState md == MtuComplete
