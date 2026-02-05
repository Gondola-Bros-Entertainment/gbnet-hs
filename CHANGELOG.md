# Changelog

## 0.2.0.0

- **23x faster packet header serialization** — zero-allocation `poke`-based writes (380ns → 17ns)
- **4.6x faster deserialization** — direct memory access via `unsafeIndex` (73ns → 18ns)
- Type-safe domain newtypes: `ChannelId`, `SequenceNum`, `MonoTime`, `MessageId`
- `GBNet.Types` module as shared home for domain newtypes
- Idiomatic cleanup: Either do-notation, function composition, `fmap`/`toList`
- Extracted focused helpers from monolithic functions (Simulator, Connection)

## 0.1.0.0

Initial release.

- Reliable and unreliable UDP channels with bitpacked serialization
- Dual-layer congestion control (binary mode + TCP New Reno CWND)
- Jacobson/Karels RTT estimation with adaptive RTO and fast retransmit
- Fragment reassembly with MTU discovery
- Effect-abstracted design via `MonadNetwork` / `MonadTime` typeclasses
- Pure deterministic testing with `TestNet`
- Template Haskell derive for `BitSerialize` instances
- Connection lifecycle: handshake, keepalive, migration, timeout
- Rate limiting, connect tokens, CRC32C packet integrity
- Priority accumulator, delta compression, interest management
- Network condition simulator (latency, jitter, packet loss, reordering)
