# Changelog

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
