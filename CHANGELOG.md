# Changelog

## Unreleased

- **Optics migration** — all record updates use `optics` composable lenses (`&`, `.~`, `%~`, `use`, `.=`)
- **Zero-copy linear types** — `ZeroCopy` module with mutable thaw/freeze for `SequenceBuffer`
- **SequenceBuffer rewrite** — vector-based ring buffer replacing `IntMap`, O(1) insert/lookup
- **Sub-microsecond packet processing** — INLINE pragmas, bang patterns, strict fields throughout
- **Peer module split** — `GBNet.Peer` split into `Internal`, `Protocol`, `Handshake`, `Migration` sub-modules (public API unchanged)
- Benchmark suite: connection, fragment, security, and reliability groups

## 0.2.0.0

- **Storable serialization** — replaced `BitBuffer`/`BitSerialize` with zero-copy Storable-based `serialize`/`deserialize` (14ns, 70M ops/sec)
- **`deriveStorable` TH** — generates `Storable` instances for product types with little-endian wire format
- **23x faster packet header serialization** — zero-allocation `poke`-based writes (380ns → 17ns)
- **4.6x faster deserialization** — direct memory access via `unsafeIndex` (73ns → 18ns)
- Type-safe domain newtypes: `ChannelId`, `SequenceNum`, `MonoTime`, `MessageId`
- `GBNet.Types` module as shared home for domain newtypes
- Modules renamed: `GBNet.Serialize.FastTH` → `GBNet.Serialize.TH`, `GBNet.Serialize.FastSupport` → `GBNet.Serialize`
- Idiomatic cleanup: Either do-notation, function composition, `fmap`/`toList`
- Extracted focused helpers from monolithic functions (Simulator, Connection)
- Comprehensive test coverage: channel modes, fragment edge cases, security, config validation, delta compression, simulator

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
