# Changelog

## 0.2.5.0

### Bug Fixes

- **Retransmissions now respect congestion control**: Retransmitted reliable messages previously bypassed all flow control (`ccCanSend`/`ccDeductBudget` and CWND), allowing unbounded retransmit traffic during congestion. Now gated by the same congestion budget as new sends.

### Internal

- Extract `encodeChannelWire` and `enqueuePayload` shared helpers, eliminating duplicated wire encoding between `processChannelMessages` and `processRetransmissions`.

## 0.2.4.0

### Bug Fixes

- **Wire reliable channel retransmission**: `Channel.getRetransmitMessages` was implemented and exported but never called from the connection tick loop. Reliable messages that were lost on the wire were never retransmitted, causing silent data loss on any packet drop. Now called from `updateConnectedPure` after processing outgoing messages — expired unacked messages are re-queued as new Payload packets with fresh sequence numbers.

## 0.2.3.0

### Bug Fixes

- **Fix keepalive regression**: `drainAllConnectionQueues` unconditionally called `recordBytesSent` even when zero bytes were sent, resetting `connLastSendTime` every tick. This prevented the keepalive timer from ever triggering, causing connections to silently die on any single packet loss (no ACK feedback, no retransmission). Now only records when actual bytes are sent.

## 0.2.2.0

### Bug Fixes

- **Bandwidth tracking now functional**: `recordBytesSent` and `recordBytesReceived` were implemented in `Connection` but never called. Wired into `peerTick`: outgoing bytes recorded after encryption in `drainAllConnectionQueues`, incoming bytes recorded per-packet in `handlePacket`. `BandwidthTracker` and `NetworkStats` byte counters now report real values.
- **Fix O(n^2) list appending** in `encryptOutgoing` and `processPacketsPure`: replaced `acc ++ [x]` with reverse-accumulator pattern.
- **Fix migration cooldowns leak**: `npMigrationCooldowns` map now swept of stale entries in `updateConnections`, preventing unbounded growth on long-running servers.
- **Total function compliance**: replaced partial `Seq.index` with `Seq.lookup` in delta encoding/baseline management.

### Internal

- Codebase-wide elimination of prime-mark variable naming (`x'`, `x''`) in favour of descriptive names and optics composition across all 31 source files, tests, and benchmarks.
- README and EXAMPLES: removed all prime-mark variables, added optics-based configuration examples, fixed section numbering, added `LambdaCase` pragma.

## 0.2.1.0

### Bug Fixes

- **Congestion bypass for small reliable packets**: Small reliable messages (<= 64 bytes) now bypass the congestion window, preventing upgrade/command stalls when heavy unreliable traffic fills the cwnd. Binary rate limit still applies to all traffic.

### New

- `smallReliableThreshold` constant exported from `GBNet.Config`

### Internal

- Refactored `processChannelMessages` from nested if/else to guards + Maybe-monadic pipeline with named where-helpers

## 0.2.0.0

### Breaking Changes

- `NetworkConfig` gains `ncEncryptionKey :: !(Maybe EncryptionKey)` field (defaults to `Nothing`)
- `NetworkStats` gains `nsDecryptionFailures :: !Word64` field
- `TestNetConfig` gains `tncDuplicateChance` and `tncOutOfOrderChance` fields
- `Connection` gains `connEncryptionKey`, `connSendNonce`, `connRecvNonceMax` fields

### New Features

- **ChaCha20-Poly1305 AEAD encryption** (`GBNet.Crypto` module): pre-shared key encryption for post-handshake packets with anti-replay nonce tracking
- **IPv6 socket support**: `newUdpSocket` now detects address family from `SockAddr`; new helpers `localhost6`, `ipv6`, `anyAddr6`
- **TestNet simulation**: packet duplication (`tncDuplicateChance`) and out-of-order delivery (`tncOutOfOrderChance`)

### Internal

- New dependency: `crypton` (ChaCha20-Poly1305), `memory` (ByteArray conversion)

## 0.1.1.0

- Drop `-O2` from benchmark ghc-options (users set optimization in their own cabal.project)
- Add automated Hackage publish to CI pipeline (tag-gated with version validation)
- Add Hackage badge to README

## 0.1.0.0

Initial release.

- Reliable and unreliable UDP channels
- Dual-layer congestion control (binary mode + TCP New Reno CWND)
- Jacobson/Karels RTT estimation with adaptive RTO and fast retransmit
- Fragment reassembly with MTU discovery
- Effect-abstracted design via `MonadNetwork` / `MonadTime` typeclasses
- Pure deterministic testing with `TestNet`
- Template Haskell serialization derive
- Connection lifecycle: handshake, keepalive, migration, timeout
- Rate limiting, connect tokens, CRC32C packet integrity
- Priority accumulator, delta compression, interest management
- Network condition simulator (latency, jitter, packet loss, reordering)
