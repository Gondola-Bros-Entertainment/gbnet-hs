# GB-Net Haskell Architecture Overhaul

## Vision

A robust, idiomatic Haskell networking library that serves:
- **High-performance games** - Low latency, deterministic, game-loop friendly
- **General-purpose serialization** - Compact binary encoding for any protocol
- **Reliable UDP communications** - Fast protocols, IoT, real-time apps

## Package Structure

```
gbnet-hs/
├── gbnet-serialize/        # Standalone serialization
├── gbnet-transport/        # Reliable UDP layer
└── gbnet-peer/             # High-level P2P networking
```

### 1. gbnet-serialize (Ready - minor polish)

Bit-level binary serialization with Template Haskell derivation.

**Features:**
- Bit-packed encoding (not byte-aligned)
- TH auto-derive for records and enums
- Zero-copy where possible
- BitWidth for compact fields

**Use cases:**
- Network protocols
- File formats
- WebSocket binary payloads
- Embedded/IoT

**Modules:**
```
GBNet.Serialize
├── BitBuffer        # Core bit-level read/write
├── Class            # BitSerialize/BitDeserialize typeclasses
├── TH               # Template Haskell derivation
└── Instances        # Standard type instances
```

### 2. gbnet-transport (Needs refactor)

Connection-oriented reliable UDP with congestion control.

**Features:**
- Connection state machine
- Reliable & unreliable channels
- Packet sequencing & ACKs
- Congestion control (CWND-based)
- RTT estimation
- Packet loss detection

**Use cases:**
- Game netcode
- Voice/video streaming
- IoT telemetry
- Custom fast protocols

**Modules:**
```
GBNet.Transport
├── Connection       # Connection state machine
├── Channel          # Reliable/unreliable channels
├── Reliability      # Sequence numbers, ACKs, retransmit
├── Congestion       # Congestion control algorithms
├── Packet           # Packet format, CRC
└── Socket           # UDP socket wrapper
```

### 3. gbnet-peer (Needs major refactor)

High-level peer-to-peer networking abstraction.

**Features:**
- Symmetric peer model (client/server/P2P)
- Connection management
- Message broadcasting
- Peer discovery (future)
- NAT traversal (future)

**Modules:**
```
GBNet.Peer
├── Core             # Pure peer state machine
├── Class            # MonadNetwork effect abstraction
├── IO               # Real socket implementation
├── Mock             # Test implementation
└── Simple           # Easy game-focused API
```

---

## Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    User Application                          │
├─────────────────────────────────────────────────────────────┤
│  GBNet.Peer.Simple     │  GBNet.Peer.PeerT                  │
│  (peerTick - easy)     │  (MonadPeer - composable)          │
├─────────────────────────────────────────────────────────────┤
│                 GBNet.Peer.Class (MonadNetwork)             │
│                 Effect abstraction layer                     │
├─────────────────────────────────────────────────────────────┤
│                 GBNet.Peer.Core (Pure)                       │
│                 Deterministic state machine                  │
├─────────────────────────────────────────────────────────────┤
│                 GBNet.Transport.*                            │
│                 Connection, reliability, congestion          │
├─────────────────────────────────────────────────────────────┤
│                 GBNet.Serialize.*                            │
│                 Bit-level serialization                      │
└─────────────────────────────────────────────────────────────┘
```

---

## Effect Abstraction Design

### MonadNetwork Typeclass

```haskell
-- | Abstract over network IO for testing and flexibility
class Monad m => MonadNetwork m where
  -- | Send raw bytes to an address
  netSend :: SockAddr -> BS.ByteString -> m (Either NetError ())

  -- | Receive bytes (non-blocking, returns Nothing if no data)
  netRecv :: m (Maybe (BS.ByteString, SockAddr))

  -- | Get current monotonic time in milliseconds
  netTime :: m MonoTime

-- | Real IO implementation
instance MonadNetwork IO where
  netSend addr bs = ...
  netRecv = ...
  netTime = ...

-- | Pure test implementation with deterministic "network"
newtype TestNet a = TestNet (State TestNetState a)

instance MonadNetwork TestNet where
  netSend addr bs = TestNet $ modify (queuePacket addr bs)
  netRecv = TestNet $ state dequeuePacket
  netTime = TestNet $ gets testTime
```

### MonadPeer Typeclass

```haskell
-- | High-level peer operations
class MonadNetwork m => MonadPeer m where
  -- | Send to specific peer
  peerSendTo :: PeerId -> Channel -> BS.ByteString -> m ()

  -- | Broadcast to all connected peers
  peerBroadcast :: Channel -> BS.ByteString -> m ()

  -- | Get pending events
  peerEvents :: m [PeerEvent]

  -- | Get connected peer IDs
  peerConnections :: m [PeerId]
```

### PeerT Monad Transformer

```haskell
-- | Peer monad transformer
newtype PeerT m a = PeerT { unPeerT :: StateT PeerState m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadNetwork m => MonadNetwork (PeerT m) where
  netSend = lift . netSend
  netRecv = lift netRecv
  netTime = lift netTime

instance MonadNetwork m => MonadPeer (PeerT m) where
  peerSendTo pid ch bs = PeerT $ do
    now <- lift netTime
    modify $ peerSendPure pid ch bs now
  ...

-- | Run peer computation
runPeerT :: MonadNetwork m => PeerT m a -> PeerState -> m (a, PeerState)
runPeerT (PeerT m) = runStateT m
```

---

## Simple API (for games)

```haskell
-- | All-in-one tick function for game loops
-- Handles: recv -> process -> broadcast -> send
peerTick
  :: MonoTime                           -- Current time
  -> [(Channel, BS.ByteString)]         -- Messages to broadcast
  -> NetPeer                            -- Current state
  -> IO ([PeerEvent], NetPeer)          -- Events and new state

-- | Game loop example
gameLoop :: NetPeer -> GameState -> IO ()
gameLoop peer game = do
  now <- getMonoTime

  -- One simple call does everything
  let outgoing = [(0, encodeState (playerPos game))]
  (events, peer') <- peerTick now outgoing peer

  -- Handle events
  game' <- foldM handleEvent game events

  -- Render, repeat
  render game'
  gameLoop peer' game'
```

---

## Known Bugs to Fix

### Critical

1. **connState not set to Connected** (Peer.hs lines 576, 597)
   - `newConnection` creates with `Disconnected` state
   - Need to set `connState = Connected` after handshake complete

2. **Server never establishes connection** (Peer.hs)
   - Server (7777) shows `conns=0` always
   - Need to investigate inbound connection handling

### Medium

3. **peerBroadcast silently fails** (Peer.hs line 872)
   - Uses `fromRight p` which swallows errors
   - Should log or accumulate errors

4. **No keepalive packets being sent**
   - Connections may timeout without activity

### Low

5. **Unused imports warnings** (Demo)
6. **Debug output still in demo** (needs cleanup)

---

## Implementation Plan

### Phase 1: Bug Fixes (Now)
- [ ] Fix connState = Connected bug
- [ ] Fix server connection handling
- [ ] Clean up demo debug output
- [ ] Verify P2P works end-to-end

### Phase 2: Effect Abstraction
- [ ] Create GBNet.Transport.Class module
- [ ] Define MonadNetwork typeclass
- [ ] Create TestNet implementation
- [ ] Write property tests for connection state machine

### Phase 3: Peer Layer Refactor
- [ ] Create GBNet.Peer.Core (pure state machine)
- [ ] Create GBNet.Peer.Class (MonadPeer)
- [ ] Create GBNet.Peer.IO (real implementation)
- [ ] Create GBNet.Peer.Simple (peerTick API)

### Phase 4: Package Split
- [ ] Extract gbnet-serialize as standalone package
- [ ] Extract gbnet-transport as standalone package
- [ ] gbnet-peer depends on both
- [ ] Update cabal files and CI

### Phase 5: Documentation & Polish
- [ ] Haddock all public APIs
- [ ] Tutorial/cookbook documentation
- [ ] Example projects (game, chat, IoT)
- [ ] Hackage release

---

## Testing Strategy

### Unit Tests
- Serialization round-trips (existing)
- Sequence number wraparound (existing)
- Connection state transitions

### Property Tests
```haskell
-- Connection state machine properties
prop_connectionNeverSkipsStates :: [Packet] -> Property
prop_connectionAlwaysResponds :: Packet -> Connection -> Property
prop_reliableDeliveryEventually :: [Message] -> Property
```

### Integration Tests
```haskell
-- Using TestNet (deterministic)
test_twoClientsConnect :: TestNet ()
test_broadcastReachesAll :: TestNet ()
test_disconnectCleanup :: TestNet ()
```

### Simulation Tests
- Packet loss simulation
- Latency simulation
- Reordering simulation
- Congestion scenarios

---

## Compatibility

### Rust GB-Net Interop
- Packet format must match Rust implementation
- Serialization must be bit-compatible
- Protocol version negotiation

### Minimum GHC Version
- GHC 9.2+ (for reliable records, etc.)
- Consider GHC 9.6+ only for simplicity

### Dependencies
- Keep minimal for wide compatibility
- Core: bytestring, containers, time
- Optional: network, stm

---

## Timeline Estimate

| Phase | Effort |
|-------|--------|
| Phase 1: Bug Fixes | 1-2 hours |
| Phase 2: Effect Abstraction | 4-6 hours |
| Phase 3: Peer Refactor | 6-8 hours |
| Phase 4: Package Split | 2-3 hours |
| Phase 5: Docs & Polish | 4-6 hours |

**Total: ~20-25 hours of focused work**

---

## Open Questions

1. Should we support both MTL-style and effect systems (polysemy, effectful)?
2. NAT traversal - build in or separate package?
3. Encryption - optional TLS/DTLS layer?
4. WebRTC data channels as alternate transport?

---

## References

- [Rust GB-Net](https://github.com/Gondola-Bros-Entertainment/gbnet) - Original implementation
- [Gaffer On Games](https://gafferongames.com/) - Networking articles
- [Laminar](https://github.com/amethyst/laminar) - Rust reliable UDP
- [ENet](http://enet.bespin.org/) - C reliable UDP library
