# Examples

Practical examples showcasing the gbnet-hs API for game networking.

---

## Networking Examples

These examples use the Peer API with the MTL-based network monad.

```haskell
import GBNet
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
  (events, ticked) <- peerTick [] peer  -- [] = no outgoing messages this tick

  -- Handle events
  handled <- foldM handleEvent ticked events

  -- Tick at ~60hz
  liftIO $ threadDelay tickIntervalUs
  serverLoop handled
  where
    tickIntervalUs = 16667

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
        Right echoed -> pure echoed

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
      let connecting = peerConnect (peerIdFromAddr serverAddr) now peer
      netState <- newNetState sock localAddr
      evalNetT (clientLoop connecting False) netState

clientLoop :: NetPeer -> Bool -> NetT IO ()
clientLoop peer connected = do
  (events, ticked) <- peerTick [] peer

  -- Check for connection event
  (handled, isConnected) <- foldM handleClientEvent (ticked, connected) events

  -- Tick
  liftIO $ threadDelay tickIntervalUs
  clientLoop handled isConnected
  where
    tickIntervalUs = 16667

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
        Right sent -> pure (sent, True)

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
      let connecting = foldl (\p remote -> peerConnect (peerIdFromAddr remote) now p) peer remotes
      netState <- newNetState sock addr
      evalNetT (p2pLoop connecting) netState

p2pLoop :: NetPeer -> NetT IO ()
p2pLoop peer = do
  (events, ticked) <- peerTick [] peer

  -- Broadcast any received message to all other peers
  now <- getMonoTime
  let broadcasted = foldl (broadcastMessage now) ticked events

  liftIO $ threadDelay tickIntervalUs
  p2pLoop broadcasted
  where
    tickIntervalUs = 16667

broadcastMessage :: MonoTime -> NetPeer -> PeerEvent -> NetPeer
broadcastMessage now peer event = case event of
  PeerMessage sender channel msg ->
    -- Broadcast to everyone except sender
    peerBroadcast channel msg (Just sender) now peer
  _ -> peer
```

### Channel Configuration (with Optics)

All gbnet-hs types use [optics](https://hackage.haskell.org/package/optics) labels
via `OverloadedLabels`. Config construction, stats queries, and deep updates compose
cleanly without prime variables or manual record update syntax:

```haskell
{-# LANGUAGE OverloadedLabels #-}
import Optics ((&), (.~), (?~), (%), view)

-- Configure different delivery modes per channel
let positionChannel = defaultChannelConfig
      & #ccDeliveryMode .~ Unreliable
      & #ccPriority     .~ 200  -- High priority

    actionChannel = defaultChannelConfig
      & #ccDeliveryMode .~ ReliableOrdered
      & #ccPriority     .~ 150

    chatChannel = defaultChannelConfig
      & #ccDeliveryMode .~ ReliableSequenced
      & #ccPriority     .~ 50   -- Low priority

    config = defaultNetworkConfig
      & #ncMaxClients              .~ 32
      & #ncConnectionTimeoutMs     .~ 10000.0
      & #ncEnableConnectionMigration .~ True
      & #ncChannelConfigs          .~ [positionChannel, actionChannel, chatChannel]
      & #ncEncryptionKey           ?~ EncryptionKey myKey

-- Query stats with composed optics
case peerStats peerId peer of
  Just stats -> do
    let bytesSent = view #nsBytesSent stats
    let congestion = view #nsCongestionLevel stats
    putStrLn $ "Sent: " ++ show bytesSent ++ " bytes, congestion: " ++ show congestion
  Nothing -> pure ()

-- TestNet config: simulate harsh network conditions
let harshNet = defaultTestNetConfig
      & #tncLatencyNs       .~ 50000000   -- 50ms one-way
      & #tncJitterNs        .~ 10000000   -- 10ms jitter
      & #tncLossRate        .~ 0.05       -- 5% packet loss
      & #tncDuplicateChance .~ 0.02       -- 2% duplicates
      & #tncOutOfOrderChance .~ 0.1       -- 10% reordering
```

---

## Serialization Examples

All serialization uses the high-performance Storable API:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import GBNet.Serialize.TH (deriveStorable)
import GBNet.Serialize (serialize, deserialize)
import GBNet.Packet

import Data.Word (Word8, Word16, Word32)
import Foreign.Storable (sizeOf)
```

---

## FPS Player Snapshot

A player state using Storable for C-level serialization speed (~14ns):

```haskell
data PlayerSnapshot = PlayerSnapshot
  { psHealth   :: !Word8   -- 0-255
  , psWeaponId :: !Word8   -- 0-255
  , psX        :: !Word16  -- 0-65535
  , psY        :: !Word16  -- 0-65535
  } deriving (Eq, Show)

deriveStorable ''PlayerSnapshot
```

Serialize, deserialize, and measure size:

```haskell
let player = PlayerSnapshot
      { psHealth   = 100
      , psWeaponId = 3
      , psX        = 512
      , psY        = 768
      }

-- Serialize (pure, ~14ns)
let bytes = serialize player
-- 1 + 1 + 2 + 2 = 6 bytes

-- Size
sizeOf player  -- 6

-- Deserialize (pure)
case deserialize bytes of
  Left err -> putStrLn $ "Invalid data: " ++ err
  Right snapshot -> print snapshot
```

The Storable approach gives C-level performance (~14ns) with automatic
memory layout. For custom game types, use `deriveStorable`:

```haskell
data GameEvent
  = PlayerSpawned !Word8 !Word16 !Word16   -- player ID, x, y
  | DamageDealt !Word8 !Word8 !Word16      -- attacker, target, amount
  deriving (Eq, Show)

-- For sum types, you'll need manual Storable instance or use tagged unions
```

For complex sum types, consider using a tagged union pattern with a
Word8 discriminator followed by payload bytes.

## Packet Header

The built-in `PacketHeader` uses Storable for fast serialization:

```haskell
import GBNet.Packet (PacketHeader(..), PacketType(..), serializeHeader, deserializeHeader)

let header = PacketHeader
      { packetType  = Payload
      , sequenceNum = 42
      , ack         = 40
      , ackBitfield = 0x00000003
      }

-- Serialize (~17ns)
let bytes = serializeHeader header

-- Deserialize (~16ns)
case deserializeHeader bytes of
  Left err -> putStrLn $ "Invalid header: " ++ err
  Right hdr -> print (sequenceNum hdr)  -- 42
```

---

## Replication Examples

### Delta Compression

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

### Interest Management

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

### Priority-Based Replication

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
  let accumulated = accumulate tickDeltaSec acc

  -- Check accumulated priorities
  print $ getPriority "player1" accumulated  -- Just 2.0 (20 * 0.1)
  print $ getPriority "tree1" accumulated    -- Just 0.1 (1 * 0.1)

  -- Drain what fits in 500 byte budget
  -- Assume each entity is ~100 bytes
  let entitySize _ = entityByteCost
  let (selected, drained) = drainTop budgetBytes entitySize accumulated

  -- selected will prioritize players, then NPCs
  print selected  -- ["player1", "player2", "npc1", "npc2", "tree1"]
                  -- (stops when budget exceeded)

  -- Selected entities have priority reset to 0
  -- Unselected entities keep accumulating
  print $ getPriority "tree2" drained  -- Still has accumulated priority
  where
    tickDeltaSec = 0.1
    budgetBytes = 500
    entityByteCost = 100
```

---

### Snapshot Interpolation

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
