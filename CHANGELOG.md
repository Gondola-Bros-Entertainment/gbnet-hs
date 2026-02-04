# Changelog

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
