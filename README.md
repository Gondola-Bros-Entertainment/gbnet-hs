<div align="center">
<h1>gbnet-hs</h1>
<p><strong>Game Networking for Haskell</strong></p>
<p>Bitpacked serialization. Reliable UDP transport. Haskell port of <a href="https://github.com/gondola-bros-entertainment/gbnet">GB-Net</a>.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#networking">Networking</a> · <a href="#serialization">Serialization</a> · <a href="#architecture">Architecture</a></p>
<p>

[![CI](https://github.com/gondola-bros-entertainment/gbnet-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/gondola-bros-entertainment/gbnet-hs/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

> **Note:** This library is undergoing a major architecture overhaul to support idiomatic Haskell patterns (effect abstraction, MonadNetwork typeclass, layered design). See [docs/ARCHITECTURE_OVERHAUL.md](docs/ARCHITECTURE_OVERHAUL.md) for the roadmap.

---

## What is GB-Net-HS?

Complete Haskell port of [GB-Net](https://github.com/gondola-bros-entertainment/gbnet), a transport-level game networking library providing:

- **Bitpacked serialization** - Sub-byte encoding for bandwidth efficiency
- **Reliable UDP** - Connection-oriented with ACKs, retransmits, and ordering
- **Unified Peer API** - Same code for client, server, or P2P mesh
- **Connection migration** - Seamless IP address change handling
- **Congestion control** - Adaptive send rates based on network conditions

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends:
    gbnet-hs
```

### Creating a Peer

```haskell
import GBNet.Peer
import GBNet.Config (defaultNetworkConfig)
import Network.Socket (SockAddr(..))

main :: IO ()
main = do
  let addr = SockAddrInet 7777 0  -- Bind to port 7777
  now <- getCurrentTime           -- Your monotonic time source

  result <- newPeer addr defaultNetworkConfig now
  case result of
    Left err -> print err
    Right peer -> gameLoop peer now

gameLoop :: NetPeer -> MonoTime -> IO ()
gameLoop peer now = do
  -- Process incoming packets and get events
  (events, peer') <- peerUpdate now peer

  -- Handle events
  forM_ events $ \case
    PeerConnected pid dir -> putStrLn $ "Connected: " ++ show pid
    PeerDisconnected pid reason -> putStrLn $ "Disconnected: " ++ show reason
    PeerMessage pid channel msg -> handleMessage pid channel msg
    PeerMigrated oldPid newPid -> putStrLn "Peer address changed"

  -- Send messages
  let peer'' = case peerSend somePeerId 0 myData now peer' of
        Left err -> peer'  -- Handle error
        Right p -> p

  gameLoop peer'' (now + tickDelta)
```

### Connecting to a Remote Peer

```haskell
-- Initiate connection (handshake happens automatically)
peer' <- peerConnect (peerIdFromAddr remoteAddr) now peer

-- The PeerConnected event will fire when handshake completes
```

### Sending Messages

```haskell
-- Send on channel 0 (reliability determined by channel config)
case peerSend peerId 0 myData now peer of
  Left ErrNotConnected -> handleNotConnected
  Left ErrChannelFull -> handleBackpressure
  Right peer' -> continue peer'

-- Broadcast to all connected peers
let peer' = peerBroadcast 0 myData Nothing now peer  -- Nothing = no exclusions
```

---

## Networking

### Peer Events

```haskell
data PeerEvent
  = PeerConnected !PeerId !ConnectionDirection  -- Inbound or Outbound
  | PeerDisconnected !PeerId !DisconnectReason
  | PeerMessage !PeerId !Word8 !BS.ByteString   -- channel, data
  | PeerMigrated !PeerId !PeerId                -- old address, new address
```

### Channel Reliability Modes

Configure per-channel delivery guarantees:

```haskell
import GBNet.Channel

-- Unreliable: fire-and-forget (position updates)
let unreliable = defaultChannelConfig { ccReliabilityMode = Unreliable }

-- Reliable ordered: guaranteed delivery, in-order (chat, RPC)
let reliable = defaultChannelConfig { ccReliabilityMode = ReliableOrdered }

-- Reliable sequenced: latest-only, drops stale (state sync)
let sequenced = defaultChannelConfig { ccReliabilityMode = ReliableSequenced }
```

### Configuration

```haskell
import GBNet.Config

let config = defaultNetworkConfig
      { ncMaxClients = 32
      , ncConnectionTimeoutMs = 10000.0
      , ncKeepaliveIntervalMs = 1000.0
      , ncMtu = 1200
      , ncEnableConnectionMigration = True
      }

-- Validate before use
case validateConfig config of
  Left err -> handleConfigError err
  Right () -> proceed
```

### Connection Migration

When a peer's IP address changes (mobile roaming, NAT rebind), gbnet-hs automatically migrates the connection:

```haskell
-- Enable in config
let config = defaultNetworkConfig { ncEnableConnectionMigration = True }

-- Handle migration event
case event of
  PeerMigrated oldPid newPid ->
    -- Update your peer ID mappings
    updatePlayerAddress playerId newPid
```

---

## Replication Helpers

### Delta Compression

Only send changed fields to save bandwidth:

```haskell
import GBNet.Delta

-- Implement NetworkDelta for your state type
instance NetworkDelta PlayerState where
  type Delta PlayerState = PlayerDelta
  diff new old = PlayerDelta
    { dPos = if pos new /= pos old then Just (pos new) else Nothing
    , dHealth = if health new /= health old then Just (health new) else Nothing
    }
  apply state delta = state
    { pos = fromMaybe (pos state) (dPos delta)
    , health = fromMaybe (health state) (dHealth delta)
    }

-- Sender: encode against acknowledged baseline
let (encoded, tracker') = deltaEncode seqNum currentState tracker

-- Receiver: decode using baseline manager
case deltaDecode encoded baselines of
  Left err -> handleError err
  Right state -> use state
```

### Interest Management

Filter entities by area-of-interest:

```haskell
import GBNet.Interest

-- Radius-based: entities within 100 units
let interest = newRadiusInterest 100.0

if relevant interest entityPos observerPos
  then sendEntity entity
  else skip  -- too far away

-- Priority modifier: closer = higher priority
let modifier = priorityMod interest entityPos observerPos
```

### Priority Accumulator

Fair bandwidth allocation across entities:

```haskell
import GBNet.Priority

let acc = newPriorityAccumulator
        & register playerId 10.0   -- high priority
        & register npcId 2.0       -- low priority

-- Each tick: accumulate based on elapsed time
let acc' = accumulate 0.016 acc

-- Drain entities that fit in budget (1200 bytes)
let (selected, acc'') = drainTop 1200 entitySize acc'
-- selected = entities to replicate this tick
```

### Snapshot Interpolation

Smooth client-side rendering:

```haskell
import GBNet.Interpolation

-- Push server snapshots as they arrive
let buffer' = pushSnapshot serverTime state buffer

-- Sample interpolated state at render time
case sampleSnapshot renderTime buffer' of
  Nothing -> waitForMoreSnapshots
  Just interpolated -> render interpolated
```

---

## Serialization

### Writing & Reading Bits

```haskell
import GBNet.Serialize.BitBuffer

-- Write 28 bits: 1-bit flag, 7-bit health, two 10-bit coords
let buf = writeBits 512 10
        $ writeBits 768 10
        $ writeBits 100 7
        $ writeBits 1   1
        $ empty

bitPosition buf  -- 28 (3.5 bytes on wire)
```

### Typeclass Serialization

```haskell
import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class

-- Serialize multiple typed values
let buf = bitSerialize (99 :: Word8)
        $ bitSerialize (5000 :: Word16)
        $ bitSerialize True
        $ empty
```

### Monadic Deserialization

```haskell
import GBNet.Serialize.Reader

let reader = do
      flag   <- deserializeM :: BitReader Bool
      health <- deserializeM :: BitReader Word16
      id_    <- deserializeM :: BitReader Word8
      pure (flag, health, id_)

case runBitReader reader buf of
  Left err -> handleError err
  Right (result, buf') -> use result
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

data GameEvent
  = PlayerJoin Word8
  | PlayerLeave Word8
  | ChatMessage Word8 Word16
  deriving (Eq, Show)

deriveNetworkSerialize ''GameEvent
```

### Custom Bit Widths

```haskell
{-# LANGUAGE DataKinds #-}
import GBNet.Serialize.Class (BitWidth(..))

-- Use exactly 7 bits for health, 4 bits for direction
data CompactPlayer = CompactPlayer
  { health    :: BitWidth 7 Word8
  , direction :: BitWidth 4 Word8
  } deriving (Eq, Show)

deriveNetworkSerialize ''CompactPlayer

-- Only 11 bits on the wire instead of 16
let player = CompactPlayer (BitWidth 100) (BitWidth 12)
let buf = bitSerialize player empty
bitPosition buf  -- 11
```

### Primitives Reference

| Type | Bits | Example |
|------|------|---------|
| `Bool` | 1 | `bitSerialize True` |
| `Word8` | 8 | `bitSerialize (42 :: Word8)` |
| `Word16` | 16 | `bitSerialize (1234 :: Word16)` |
| `Word32` | 32 | `bitSerialize (n :: Word32)` |
| `Word64` | 64 | `bitSerialize (n :: Word64)` |
| `Int8`-`Int64` | 8-64 | Signed integers |
| `Float` | 32 | `bitSerialize (3.14 :: Float)` |
| `Double` | 64 | `bitSerialize (n :: Double)` |
| `Maybe a` | 1 + payload | `bitSerialize (Just 42)` |
| `[a]` | 16 + elements | Max 65535 elements |
| `Text` | 16 + UTF-8 | Full Unicode support |
| `BitWidth n a` | n | Custom bit width |

---

## Architecture

```
gbnet-hs/
├── src/GBNet/
│   ├── Peer.hs               # Unified networking API
│   ├── Connection.hs         # Connection state machine
│   ├── Packet.hs             # Wire protocol (68-bit header)
│   ├── Reliability.hs        # RTT, ACKs, retransmit
│   ├── Channel.hs            # Delivery modes
│   ├── Fragment.hs           # Large message fragmentation
│   ├── Congestion.hs         # Congestion control
│   ├── Config.hs             # Configuration & validation
│   ├── Security.hs           # CRC32, rate limiting
│   ├── Simulator.hs          # Network condition simulation
│   ├── Socket.hs             # UDP socket wrapper
│   ├── Stats.hs              # Network statistics
│   ├── Util.hs               # Sequence utilities
│   ├── Delta.hs              # Delta compression for state sync
│   ├── Interest.hs           # Area-of-interest filtering
│   ├── Priority.hs           # Bandwidth-fair entity selection
│   ├── Interpolation.hs      # Client-side snapshot smoothing
│   └── Serialize/
│       ├── BitBuffer.hs      # Bit-level read/write
│       ├── Class.hs          # BitSerialize typeclass
│       ├── Reader.hs         # BitReader monad
│       └── TH.hs             # Template Haskell derive
├── test/Main.hs              # 126+ round-trip tests
└── gbnet-hs.cabal
```

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.6.

```bash
cabal build                              # Build library
cabal test                               # Run all tests
cabal build --ghc-options="-Werror"      # Warnings as errors
hlint src/                               # Lint check
cabal haddock                            # Generate docs
```

---

## Performance

The library is optimized for game networking workloads:

- **Strict fields** with bang patterns throughout
- **GHC optimization flags**: `-O2 -fspecialise-aggressively -fexpose-all-unfoldings`
- **INLINE pragmas** on hot paths (serialization, packet handling)
- **Byte-aligned fast paths** when bit position is aligned

---

## Features

### Core Transport
- [x] Bitpacked serialization with sub-byte encoding
- [x] Template Haskell derive for records and enums
- [x] Custom bit widths via `BitWidth n a`
- [x] Reliable/unreliable/sequenced delivery modes
- [x] RTT estimation and adaptive retransmit
- [x] Large message fragmentation and reassembly
- [x] Connection migration (IP address changes)
- [x] CRC32 packet validation
- [x] Rate limiting for DoS protection
- [x] Network condition simulation (loss, latency, jitter)
- [x] Congestion control (binary and window-based)
- [x] Full Unicode text support via `Text`

### High-Level Replication
- [x] Delta compression (only send changed fields)
- [x] Interest management (radius/grid area-of-interest)
- [x] Priority accumulator (fair bandwidth allocation)
- [x] Snapshot interpolation (smooth client-side rendering)

---

## Contributing

Contributions welcome. Run before submitting PRs:

```bash
cabal test && cabal build --ghc-options="-Werror" && hlint src/
```

---

<p align="center">
  <sub>MIT License · Built by <a href="https://github.com/gondola-bros-entertainment">Gondola Bros Entertainment</a></sub>
</p>
