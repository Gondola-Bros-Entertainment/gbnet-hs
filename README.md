<div align="center">
<h1>gbnet-hs</h1>
<p><strong>Game Networking for Haskell</strong></p>
<p>Bitpacked serialization. Reliable UDP transport. Effect-abstracted design for pure testing.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#networking">Networking</a> · <a href="#serialization">Serialization</a> · <a href="#testing">Testing</a> · <a href="#architecture">Architecture</a></p>
<p>

[![CI](https://github.com/gondola-bros-entertainment/gbnet-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/gondola-bros-entertainment/gbnet-hs/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

## What is gbnet-hs?

Haskell port of [GB-Net](https://github.com/gondola-bros-entertainment/gbnet), a transport-level game networking library providing:

- **Bitpacked serialization** — Sub-byte encoding for bandwidth efficiency
- **Reliable UDP** — Connection-oriented with ACKs, retransmits, and ordering
- **Unified Peer API** — Same code for client, server, or P2P mesh
- **Effect abstraction** — `MonadNetwork` typeclass enables pure deterministic testing
- **Connection migration** — Seamless IP address change handling

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends:
    gbnet-hs
```

### Simple Game Loop

```haskell
import GBNet
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  -- Initialize network
  let bindAddr = anyAddr 7777
  Right netState <- initNetState bindAddr

  -- Create peer
  now <- getMonoTimeIO
  Right (peer, _) <- newPeer bindAddr defaultNetworkConfig now

  -- Run game loop inside NetT IO
  evalNetT (gameLoop peer) netState

gameLoop :: NetPeer -> NetT IO ()
gameLoop peer = do
  -- Single call: receive, process, broadcast, send
  let outgoing = [(0, encodeMyState myState)]
  (events, peer') <- peerTick outgoing peer

  -- Handle events
  liftIO $ mapM_ handleEvent events

  gameLoop peer'

handleEvent :: PeerEvent -> IO ()
handleEvent = \case
  PeerConnected pid dir   -> putStrLn $ "Connected: " ++ show pid
  PeerDisconnected pid _  -> putStrLn $ "Disconnected: " ++ show pid
  PeerMessage pid ch msg  -> handleMessage pid ch msg
  PeerMigrated old new    -> putStrLn "Peer address changed"
```

### Connecting to a Remote Peer

```haskell
-- Initiate connection (handshake happens automatically)
let peer' = peerConnect (peerIdFromAddr remoteAddr) now peer

-- The PeerConnected event fires when handshake completes
```

---

## Networking

### The `peerTick` Function

The recommended API for game loops — handles receive, process, and send in one call:

```haskell
peerTick
  :: MonadNetwork m
  => [(Word8, ByteString)]    -- Messages to broadcast (channel, data)
  -> NetPeer                   -- Current peer state
  -> m ([PeerEvent], NetPeer)  -- Events and updated state
```

### Peer Events

```haskell
data PeerEvent
  = PeerConnected !PeerId !ConnectionDirection  -- Inbound or Outbound
  | PeerDisconnected !PeerId !DisconnectReason
  | PeerMessage !PeerId !Word8 !ByteString      -- channel, data
  | PeerMigrated !PeerId !PeerId                -- old address, new address
```

### Channel Reliability Modes

```haskell
import GBNet

-- Unreliable: fire-and-forget (position updates)
let unreliable = defaultChannelConfig { ccDeliveryMode = Unreliable }

-- Reliable ordered: guaranteed delivery, in-order (chat, RPC)
let reliable = defaultChannelConfig { ccDeliveryMode = ReliableOrdered }

-- Reliable sequenced: latest-only, drops stale (state sync)
let sequenced = defaultChannelConfig { ccDeliveryMode = ReliableSequenced }
```

### Configuration

```haskell
let config = defaultNetworkConfig
      { ncMaxClients = 32
      , ncConnectionTimeoutMs = 10000.0
      , ncKeepaliveIntervalMs = 1000.0
      , ncMtu = 1200
      , ncEnableConnectionMigration = True
      , ncChannelConfigs = [unreliableChannel, reliableChannel]
      }
```

---

## Serialization

### Bit-Level Encoding

```haskell
import GBNet

-- Write 28 bits: 1-bit flag, 7-bit health, two 10-bit coords
let buf = bitSerialize (1 :: Word8)     -- 1 bit (flag)
        $ bitSerialize (100 :: Word8)   -- 8 bits
        $ bitSerialize (512 :: Word16)  -- 16 bits
        $ empty

let bytes = toBytes buf  -- Compact wire format
```

### Template Haskell Derive

```haskell
{-# LANGUAGE TemplateHaskell #-}
import GBNet.Serialize.TH

data PlayerState = PlayerState
  { health :: Word8
  , x      :: Word16
  , y      :: Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''PlayerState

-- Now PlayerState has BitSerialize/BitDeserialize instances
let encoded = toBytes (bitSerialize myPlayer empty)
```

### Custom Bit Widths

```haskell
{-# LANGUAGE DataKinds #-}
import GBNet.Serialize.Class (BitWidth(..))

data CompactPlayer = CompactPlayer
  { health    :: BitWidth 7 Word8   -- 7 bits instead of 8
  , direction :: BitWidth 4 Word8   -- 4 bits instead of 8
  }
-- Only 11 bits on the wire instead of 16
```

---

## Testing

### Pure Deterministic Testing with TestNet

The `MonadNetwork` typeclass allows swapping real sockets for a pure test implementation:

```haskell
import GBNet
import GBNet.TestNet

-- Run peer logic purely — no actual network IO
testHandshake :: ((), TestNetState)
testHandshake = runTestNet action (initialTestNetState myAddr)
  where
    action = do
      -- Simulate sending
      netSend remoteAddr someData
      -- Advance simulated time
      advanceTime 100
      -- Check what would be received
      result <- netRecv
      pure ()
```

### Multi-Peer World Simulation

```haskell
import GBNet.TestNet

-- Create a world with multiple peers
let world = newTestWorld

-- Run actions for each peer
let (result1, world') = runPeerInWorld addr1 action1 world
let (result2, world'') = runPeerInWorld addr2 action2 world'

-- Deliver packets between peers
let world''' = worldAdvanceTime 100 world''  -- Advances time and delivers
```

### Simulating Network Conditions

```haskell
-- Add 50ms latency
simulateLatency 50

-- 10% packet loss
simulateLoss 0.1
```

---

## Architecture

```
┌─────────────────────────────────────────┐
│           User Application              │
├─────────────────────────────────────────┤
│  GBNet (top-level re-exports)           │
│  import GBNet -- gets everything        │
├─────────────────────────────────────────┤
│  GBNet.Peer                             │
│  peerTick, peerConnect, PeerEvent       │
├─────────────────────────────────────────┤
│  GBNet.Net (NetT transformer)           │
│  Carries socket state for IO            │
├──────────────┬──────────────────────────┤
│  NetT IO     │  TestNet                 │
│  (real UDP)  │  (pure, deterministic)   │
├──────────────┴──────────────────────────┤
│  GBNet.Class                            │
│  MonadTime, MonadNetwork typeclasses    │
└─────────────────────────────────────────┘
```

### Module Overview

| Module | Purpose |
|--------|---------|
| `GBNet` | Top-level facade — import this for convenience |
| `GBNet.Class` | `MonadTime`, `MonadNetwork` typeclasses |
| `GBNet.Net` | `NetT` monad transformer, `MonadNetwork (NetT IO)` |
| `GBNet.Net.IO` | `initNetState` — create real UDP socket |
| `GBNet.Peer` | `NetPeer`, `peerTick`, connection management |
| `GBNet.TestNet` | Pure test network, `TestWorld` for multi-peer |
| `GBNet.Serialize.*` | Bit-level serialization, TH derivation |

### Explicit Imports (for larger codebases)

```haskell
-- Instead of `import GBNet`, be explicit:
import GBNet.Class (MonadNetwork, MonadTime, MonoTime)
import GBNet.Net (NetT, runNetT, evalNetT)
import GBNet.Net.IO (initNetState)
import GBNet.Peer (NetPeer, peerTick, PeerEvent(..))
import GBNet.Config (NetworkConfig(..), defaultNetworkConfig)
```

---

## Replication Helpers

### Delta Compression

Only send changed fields:

```haskell
import GBNet.Delta

instance NetworkDelta PlayerState where
  type Delta PlayerState = PlayerDelta
  diff new old = PlayerDelta { ... }
  apply state delta = state { ... }
```

### Interest Management

Filter by area-of-interest:

```haskell
import GBNet.Interest

let interest = newRadiusInterest 100.0
if relevant interest entityPos observerPos
  then sendEntity entity
  else skip
```

### Priority Accumulator

Fair bandwidth allocation:

```haskell
import GBNet.Priority

let acc = newPriorityAccumulator
        & register playerId 10.0
        & register npcId 2.0
let (selected, acc') = drainTop 1200 entitySize acc
```

### Snapshot Interpolation

Smooth client-side rendering:

```haskell
import GBNet.Interpolation

let buffer' = pushSnapshot serverTime state buffer
case sampleSnapshot renderTime buffer' of
  Nothing -> waitForMoreSnapshots
  Just interpolated -> render interpolated
```

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.6.

```bash
cabal build                              # Build library
cabal test                               # Run all tests
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal haddock                            # Generate docs
```

---

## Performance

Optimized for game networking:

- **Strict fields** with bang patterns throughout
- **GHC flags**: `-O2 -fspecialise-aggressively -fexpose-all-unfoldings`
- **INLINE pragmas** on hot paths
- **Byte-aligned fast paths** when bit position allows

---

## Features

### Core Transport
- [x] Bitpacked serialization with sub-byte encoding
- [x] Template Haskell derive for records and enums
- [x] Custom bit widths via `BitWidth n a`
- [x] Reliable/unreliable/sequenced delivery modes
- [x] RTT estimation and adaptive retransmit
- [x] Large message fragmentation
- [x] Connection migration
- [x] CRC32 packet validation
- [x] Rate limiting

### Effect Abstraction
- [x] `MonadNetwork` typeclass
- [x] `NetT` monad transformer for IO
- [x] `TestNet` pure deterministic network
- [x] `TestWorld` multi-peer simulation

### Replication Helpers
- [x] Delta compression
- [x] Interest management
- [x] Priority accumulator
- [x] Snapshot interpolation

---

## Contributing

```bash
cabal test && cabal build --ghc-options="-Werror"
```

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/gondola-bros-entertainment">Gondola Bros Entertainment</a></sub>
</p>
