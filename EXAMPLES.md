# Examples

Practical examples showcasing the gbnet-hs API for game networking.

---

## Networking Examples

These examples use the Peer API with the MTL-based network monad.

```haskell
import GBNet
import GBNet.Net (newNetState)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
```

### Basic Server

```haskell
-- A simple server that echoes messages back
runServer :: IO ()
runServer = do
  let addr = anyAddr 7777  -- Port 7777, all interfaces
  let config = defaultNetworkConfig { ncMaxClients = 16 }

  now <- getMonoTimeIO
  result <- newPeer addr config now
  case result of
    Left err -> putStrLn $ "Failed to bind: " ++ show err
    Right (peer, sock) -> do
      netState <- newNetState sock addr
      evalNetT (serverLoop peer) netState

serverLoop :: NetPeer -> NetT IO ()
serverLoop peer = do
  -- Process network (receive packets, handle connections, send queued)
  (events, peer') <- peerTick [] peer  -- [] = no outgoing messages this tick

  -- Handle events
  peer'' <- foldM handleEvent peer' events

  -- Tick at ~60hz
  liftIO $ threadDelay 16667
  serverLoop peer''

handleEvent :: NetPeer -> PeerEvent -> NetT IO NetPeer
handleEvent peer event = do
  now <- getMonoTime
  case event of
    PeerConnected pid dir -> do
      liftIO $ putStrLn ("Connected: " ++ show pid ++ " " ++ show dir)
      pure peer

    PeerDisconnected pid reason -> do
      liftIO $ putStrLn ("Disconnected: " ++ show pid ++ " " ++ show reason)
      pure peer

    PeerMessage pid channel msg -> do
      liftIO $ putStrLn $ "Received " ++ show (BS.length msg) ++ " bytes on channel " ++ show channel
      -- Echo back on same channel
      case peerSend pid channel msg now peer of
        Left err -> do
          liftIO $ putStrLn ("Send error: " ++ show err)
          pure peer
        Right peer' -> pure peer'

    PeerMigrated oldPid newPid -> do
      liftIO $ putStrLn ("Peer migrated: " ++ show oldPid ++ " -> " ++ show newPid)
      pure peer
```

### Basic Client

```haskell
-- A client that connects and sends a message
runClient :: IO ()
runClient = do
  let localAddr = anyAddr 0  -- Ephemeral port
  let serverAddr = localhost 7777

  now <- getMonoTimeIO
  result <- newPeer localAddr defaultNetworkConfig now
  case result of
    Left err -> putStrLn $ "Failed to create peer: " ++ show err
    Right (peer, sock) -> do
      -- Initiate connection
      let peer' = peerConnect (peerIdFromAddr serverAddr) now peer
      netState <- newNetState sock localAddr
      evalNetT (clientLoop peer' False) netState

clientLoop :: NetPeer -> Bool -> NetT IO ()
clientLoop peer connected = do
  (events, peer') <- peerTick [] peer

  -- Check for connection event
  (peer'', connected') <- foldM handleClientEvent (peer', connected) events

  -- Tick
  liftIO $ threadDelay 16667
  clientLoop peer'' connected'

handleClientEvent :: (NetPeer, Bool) -> PeerEvent -> NetT IO (NetPeer, Bool)
handleClientEvent (peer, connected) event = do
  now <- getMonoTime
  case event of
    PeerConnected pid _ -> do
      liftIO $ putStrLn "Connected to server!"
      -- Send a message on channel 0
      let msg = BS.pack [72, 101, 108, 108, 111]  -- "Hello"
      case peerSend pid (ChannelId 0) msg now peer of
        Left err -> do
          liftIO $ putStrLn ("Send error: " ++ show err)
          pure (peer, True)
        Right peer' -> pure (peer', True)

    PeerMessage _ _ msg -> do
      liftIO $ putStrLn $ "Server replied: " ++ show msg
      pure (peer, connected)

    PeerDisconnected _ reason -> do
      liftIO $ putStrLn $ "Disconnected: " ++ show reason
      pure (peer, False)

    _ -> pure (peer, connected)
```

### P2P Mesh

```haskell
-- A peer that both listens and connects (P2P mode)
runP2PPeer :: Int -> [SockAddr] -> IO ()
runP2PPeer port remotes = do
  let addr = anyAddr (fromIntegral port)
  let config = defaultNetworkConfig
        { ncMaxClients = 64
        , ncEnableConnectionMigration = True
        }

  now <- getMonoTimeIO
  result <- newPeer addr config now
  case result of
    Left err -> putStrLn $ "Failed: " ++ show err
    Right (peer, sock) -> do
      -- Connect to all known peers
      let peer' = foldl (\p remote -> peerConnect (peerIdFromAddr remote) now p) peer remotes
      netState <- newNetState sock addr
      evalNetT (p2pLoop peer') netState

p2pLoop :: NetPeer -> NetT IO ()
p2pLoop peer = do
  (events, peer') <- peerTick [] peer

  -- Broadcast any received message to all other peers
  now <- getMonoTime
  let peer'' = foldl (broadcastMessage now) peer' events

  liftIO $ threadDelay 16667
  p2pLoop peer''

broadcastMessage :: MonoTime -> NetPeer -> PeerEvent -> NetPeer
broadcastMessage now peer event = case event of
  PeerMessage sender channel msg ->
    -- Broadcast to everyone except sender
    peerBroadcast channel msg (Just sender) now peer
  _ -> peer
```

### Channel Configuration

```haskell
-- Configure different delivery modes per channel
let config = defaultNetworkConfig
      { ncChannelConfigs =
          [ -- Channel 0: Position updates (unreliable, latest-only)
            defaultChannelConfig
              { ccDeliveryMode = Unreliable
              , ccPriority = 200  -- High priority
              }
          , -- Channel 1: Player actions (reliable, ordered)
            defaultChannelConfig
              { ccDeliveryMode = ReliableOrdered
              , ccPriority = 150
              }
          , -- Channel 2: Chat (reliable, but order doesn't matter)
            defaultChannelConfig
              { ccDeliveryMode = ReliableSequenced
              , ccPriority = 50  -- Low priority
              }
          ]
      }
```

---

## Serialization Examples

All serialization examples assume these imports:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.Reader
import GBNet.Serialize.TH
import GBNet.Packet

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text as T
```

---

## 1. FPS Player Snapshot

A compact player state using `BitWidth` for tight packing: 7-bit health
(0–127), 4-bit weapon ID (0–15), 10-bit X/Y coordinates (0–1023), and a
1-bit alive flag.

```haskell
data PlayerSnapshot = PlayerSnapshot
  { psAlive    :: Bool
  , psHealth   :: BitWidth 7 Word8
  , psWeaponId :: BitWidth 4 Word8
  , psX        :: BitWidth 10 Word16
  , psY        :: BitWidth 10 Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''PlayerSnapshot
```

Serialize, deserialize, and measure size:

```haskell
let player = PlayerSnapshot
      { psAlive    = True
      , psHealth   = BitWidth 100
      , psWeaponId = BitWidth 3
      , psX        = BitWidth 512
      , psY        = BitWidth 768
      }

-- Serialize
let buf = bitSerialize player empty
-- 1 + 7 + 4 + 10 + 10 = 32 bits (4 bytes)

-- Measure size
serializedSizeBits (bitSerialize player)   -- 32
serializedSizeBytes (bitSerialize player)  -- 4

-- Deserialize
case runBitReader deserializeM buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (snapshot, _) -> print snapshot
  -- PlayerSnapshot {psAlive = True, psHealth = BitWidth {unBitWidth = 100}, ...}
```

Compare to a naive approach using full-width types: `Bool` (8 bits as
`Word8`) + `Word8` + `Word8` + `Word16` + `Word16` = 56 bits. The
bitpacked version uses 32 bits — a **43% reduction**.

---

## 2. Chat System

A sum type with payloads representing different chat message kinds:

```haskell
data ChatMessage
  = SystemMessage T.Text
  | PlayerChat Word8 T.Text
  | TeamChat Word8 T.Text
  deriving (Eq, Show)

deriveNetworkSerialize ''ChatMessage
```

`Text` handles full Unicode, including emoji:

```haskell
let msgs =
      [ SystemMessage "Server restarting"
      , PlayerChat 7 "gg wp"
      , TeamChat 3 "push left \x1F680"  -- rocket emoji
      ]

-- Serialize all messages as a list
let buf = bitSerialize msgs empty

-- Deserialize
case runBitReader (deserializeM :: BitReader [ChatMessage]) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
  -- SystemMessage "Server restarting"
  -- PlayerChat 7 "gg wp"
  -- TeamChat 3 "push left 🚀"
```

The enum tag uses `ceilLog2(3) = 2` bits to distinguish the three
constructors. `Text` is serialized as a 16-bit byte-length prefix
followed by raw UTF-8 bytes — wire-compatible with the Rust `String`
serialization.

---

## 3. Game Event Stream

Multiple event types serialized as a list:

```haskell
data GameEvent
  = PlayerSpawned Word8 Word16 Word16    -- player ID, x, y
  | DamageDealt Word8 Word8 Word16       -- attacker, target, amount
  | ItemPickup Word8 Word16              -- player ID, item ID
  | PlayerDied Word8 Word8               -- player ID, killer ID
  deriving (Eq, Show)

deriveNetworkSerialize ''GameEvent
```

Deserialize with the `BitReader` monad using do-notation:

```haskell
let events =
      [ PlayerSpawned 1 100 200
      , DamageDealt 1 2 50
      , ItemPickup 2 1001
      , PlayerDied 2 1
      ]

let buf = bitSerialize events empty

-- The BitReader monad threads buffer state and error propagation
-- automatically — no manual case-matching on ReadResult needed.
let reader :: BitReader [GameEvent]
    reader = do
      count <- readBitsM 16  -- list length prefix
      sequence [ deserializeM | _ <- [1 .. count] ]

case runBitReader reader buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
```

Or simply use the `[a]` instance which handles the length prefix
automatically:

```haskell
case runBitReader (deserializeM :: BitReader [GameEvent]) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
```

---

## 4. Inventory System

`Maybe` for optional equipment slots. `Nothing` costs only 1 bit on the
wire; `Just x` costs 1 bit + the payload.

```haskell
-- Equipment slots: each is optional
type ItemId = BitWidth 8 Word8

let helmet = Just (BitWidth 12 :: ItemId)
let weapon = Just (BitWidth 3 :: ItemId)
let armor  = Nothing :: Maybe ItemId

-- Serialize a 3-tuple of optional slots
let loadout = (helmet, weapon, armor)
let buf = bitSerialize loadout empty

-- helmet: 1 (present) + 8 (item ID) = 9 bits
-- weapon: 1 (present) + 8 (item ID) = 9 bits
-- armor:  1 (absent)                 = 1 bit
-- Total: 19 bits

serializedSizeBits (bitSerialize loadout)  -- 19

case runBitReader (deserializeM :: BitReader (Maybe ItemId, Maybe ItemId, Maybe ItemId)) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> print result
  -- (Just (BitWidth {unBitWidth = 12}), Just (BitWidth {unBitWidth = 3}), Nothing)
```

Nested optional: `Maybe (Maybe Bool)` works out of the box:

```haskell
let nested = Just (Just True) :: Maybe (Maybe Bool)
let buf = bitSerialize nested empty
-- 1 (outer present) + 1 (inner present) + 1 (Bool) = 3 bits
serializedSizeBits (bitSerialize nested)  -- 3
```

---

## 5. Raw Bit Packing

Manual `writeBits` / `readBits` for a custom wire format. Pack a
movement input: 2-bit direction, 1-bit jump, 1-bit shoot, 4-bit
sequence number.

```haskell
let direction = 2 :: Word64  -- 0=up, 1=right, 2=down, 3=left
let jump      = True
let shoot     = False
let seqNum    = 13 :: Word64

-- Pack manually (writes are composed right-to-left)
let buf = writeBits seqNum 4
        $ writeBit shoot
        $ writeBit jump
        $ writeBits direction 2
          empty

-- 2 + 1 + 1 + 4 = 8 bits (exactly 1 byte)
bitPosition buf     -- 8
toBitString buf     -- "10101101"
                    -- ^^        direction = 2 (10)
                    --   ^       jump = 1
                    --    ^      shoot = 0
                    --     ^^^^  seqNum = 13 (1101)

-- Read back with the BitReader monad
let reader = do
      dir <- readBitsM 2
      j   <- readBitM
      s   <- readBitM
      sq  <- readBitsM 4
      pure (dir, j, s, sq)

case runBitReader reader buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right ((dir, j, s, sq), _) ->
    putStrLn $ "dir=" ++ show dir
            ++ " jump=" ++ show j
            ++ " shoot=" ++ show s
            ++ " seq=" ++ show sq
    -- dir=2 jump=True shoot=False seq=13
```

Use `serializedSizeBits` to verify the wire size of any serialization:

```haskell
serializedSizeBits (writeBits 0 2 . writeBit False . writeBit False . writeBits 0 4)
-- 8
```

---

## 6. Packet Header + Payload

Build a complete packet: `PacketHeader` followed by a payload type, then
convert to bytes for the wire and back.

```haskell
data MovePayload = MovePayload
  { moveX :: Word16
  , moveY :: Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''MovePayload

let header = PacketHeader
      { packetType  = Payload
      , sequenceNum = 42
      , ack         = 40
      , ackBitfield = 0x00000003  -- ack-1 and ack-2 received
      }

let payload = MovePayload { moveX = 300, moveY = 750 }

-- Serialize header + payload into a single buffer
let buf = bitSerialize payload $ bitSerialize header empty

-- Convert to ByteString for sending over UDP
let bytes = toBytes buf
-- 68 bits (header) + 32 bits (payload) = 100 bits = 13 bytes

-- On the receiving end: fromBytes and deserialize
let rxBuf = fromBytes bytes

let reader = do
      hdr <- deserializeM :: BitReader PacketHeader
      msg <- deserializeM :: BitReader MovePayload
      pure (hdr, msg)

case runBitReader reader rxBuf of
  Left err -> putStrLn $ "Error: " ++ err
  Right ((hdr, msg), _) -> do
    putStrLn $ "Type: " ++ show (packetType hdr)
    putStrLn $ "Seq:  " ++ show (sequenceNum hdr)
    putStrLn $ "Move: " ++ show (moveX msg) ++ ", " ++ show (moveY msg)
    -- Type: Payload
    -- Seq:  42
    -- Move: 300, 750
```

End-to-end: serialize → `toBytes` → (network) → `fromBytes` → deserialize.
The wire format is identical to the Rust gbnet library, so a Haskell
client can communicate with a Rust server (and vice versa) with zero
conversion overhead.

---

## Replication Examples

### 7. Delta Compression

Send only changed fields to minimize bandwidth:

```haskell
import GBNet.Replication.Delta
import Data.Maybe (fromMaybe)

-- Your game state
data PlayerState = PlayerState
  { psPos :: (Float, Float)
  , psHealth :: Int
  , psAmmo :: Int
  } deriving (Eq, Show)

-- Delta contains Maybe for each field
data PlayerDelta = PlayerDelta
  { dPos :: Maybe (Float, Float)
  , dHealth :: Maybe Int
  , dAmmo :: Maybe Int
  } deriving (Eq, Show)

instance NetworkDelta PlayerState where
  type Delta PlayerState = PlayerDelta

  diff new old = PlayerDelta
    { dPos = if psPos new /= psPos old then Just (psPos new) else Nothing
    , dHealth = if psHealth new /= psHealth old then Just (psHealth new) else Nothing
    , dAmmo = if psAmmo new /= psAmmo old then Just (psAmmo new) else Nothing
    }

  apply state delta = PlayerState
    { psPos = fromMaybe (psPos state) (dPos delta)
    , psHealth = fromMaybe (psHealth state) (dHealth delta)
    , psAmmo = fromMaybe (psAmmo state) (dAmmo delta)
    }

-- Server: track and encode deltas
serverTick :: BaselineSeq -> PlayerState -> DeltaTracker PlayerState
           -> (BS.ByteString, DeltaTracker PlayerState)
serverTick seq currentState tracker = deltaEncode seq currentState tracker

-- Server: when client ACKs a packet
onClientAck :: BaselineSeq -> DeltaTracker PlayerState -> DeltaTracker PlayerState
onClientAck ackSeq tracker = deltaOnAck ackSeq tracker

-- Client: decode received delta
clientReceive :: BS.ByteString -> BaselineManager PlayerState
              -> Either String PlayerState
clientReceive encoded baselines = deltaDecode encoded baselines
```

---

### 8. Interest Management

Only replicate entities within area-of-interest:

```haskell
import GBNet.Replication.Interest

-- Radius-based AOI (sphere)
radiusExample :: IO ()
radiusExample = do
  let interest = newRadiusInterest 500.0  -- 500 unit radius

  let playerPos = (100.0, 50.0, 0.0)
  let npc1Pos = (150.0, 60.0, 0.0)   -- close
  let npc2Pos = (1000.0, 0.0, 0.0)   -- far

  -- Check relevance
  print $ relevant interest npc1Pos playerPos  -- True (within 500)
  print $ relevant interest npc2Pos playerPos  -- False (too far)

  -- Get priority modifier (closer = higher)
  print $ priorityMod interest npc1Pos playerPos  -- ~0.9 (very close)
  print $ priorityMod interest npc2Pos playerPos  -- 0.0 (out of range)

-- Grid-based AOI (spatial partitioning)
gridExample :: IO ()
gridExample = do
  let interest = newGridInterest 100.0  -- 100 unit cells

  let playerPos = (150.0, 150.0, 0.0)  -- cell (1, 1)
  let npc1Pos = (180.0, 120.0, 0.0)    -- cell (1, 1) - same cell
  let npc2Pos = (250.0, 150.0, 0.0)    -- cell (2, 1) - neighbor
  let npc3Pos = (500.0, 500.0, 0.0)    -- cell (5, 5) - far

  print $ relevant interest npc1Pos playerPos  -- True (same cell)
  print $ relevant interest npc2Pos playerPos  -- True (neighbor)
  print $ relevant interest npc3Pos playerPos  -- False (too far)
```

---

### 9. Priority-Based Replication

Fair bandwidth allocation when you can't send everything:

```haskell
import GBNet.Replication.Priority

replicationExample :: IO ()
replicationExample = do
  -- Register entities with base priorities (units per second)
  let acc = register "tree2" 1.0      -- scenery: low priority
          $ register "tree1" 1.0
          $ register "npc2" 5.0       -- NPCs: medium priority
          $ register "npc1" 5.0
          $ register "player2" 20.0   -- players: high priority
          $ register "player1" 20.0
            newPriorityAccumulator

  -- Simulate 100ms tick - priorities accumulate
  let acc' = accumulate 0.1 acc

  -- Check accumulated priorities
  print $ getPriority "player1" acc'  -- Just 2.0 (20 * 0.1)
  print $ getPriority "tree1" acc'    -- Just 0.1 (1 * 0.1)

  -- Drain what fits in 500 byte budget
  -- Assume each entity is ~100 bytes
  let entitySize _ = 100
  let (selected, acc'') = drainTop 500 entitySize acc'

  -- selected will prioritize players, then NPCs
  print selected  -- ["player1", "player2", "npc1", "npc2", "tree1"]
                  -- (stops when budget exceeded)

  -- Selected entities have priority reset to 0
  -- Unselected entities keep accumulating
  print $ getPriority "tree2" acc''  -- Still has accumulated priority
```

---

### 10. Snapshot Interpolation

Smooth rendering despite network jitter:

```haskell
import GBNet.Replication.Interpolation

-- Your state must implement Interpolatable
data Transform = Transform
  { tX :: Float
  , tY :: Float
  } deriving (Eq, Show)

instance Interpolatable Transform where
  lerp a b t = Transform
    { tX = tX a + (tX b - tX a) * t
    , tY = tY a + (tY b - tY a) * t
    }

interpolationExample :: IO ()
interpolationExample = do
  -- Create buffer with 100ms playback delay
  let buffer = newSnapshotBufferWithConfig 3 100.0

  -- Server sends snapshots at t=0, t=50, t=100, t=150
  let buffer1 = pushSnapshot 0.0 (Transform 0.0 0.0) buffer
  let buffer2 = pushSnapshot 50.0 (Transform 10.0 5.0) buffer1
  let buffer3 = pushSnapshot 100.0 (Transform 20.0 10.0) buffer2
  let buffer4 = pushSnapshot 150.0 (Transform 30.0 15.0) buffer3

  -- Check if ready for interpolation
  print $ snapshotReady buffer4  -- True (3+ snapshots)

  -- At render time 175ms, we sample at 175-100 = 75ms
  -- This interpolates between t=50 and t=100
  case sampleSnapshot 175.0 buffer4 of
    Nothing -> putStrLn "Not ready"
    Just t -> print t  -- Transform 15.0 7.5 (halfway between)

  -- At render time 225ms, we sample at 125ms
  -- Interpolates between t=100 and t=150
  case sampleSnapshot 225.0 buffer4 of
    Nothing -> putStrLn "Not ready"
    Just t -> print t  -- Transform 25.0 12.5
```
