<div align="center">
<h1>gbnet-hs</h1>
<p><strong>Game Networking for Haskell</strong></p>
<p>Bitpacked serialization. Reliable UDP transport. Haskell port of <a href="https://github.com/aoinoikaz/gbnet">GB-Net</a>.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#serialization">Serialization</a> · <a href="#architecture">Architecture</a></p>
<p>

[![CI](https://github.com/aoinoikaz/gbnet-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/aoinoikaz/gbnet-hs/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

## What is GB-Net-HS?

Haskell port of [GB-Net](https://github.com/aoinoikaz/gbnet), a transport-level game networking library. Currently implements the bitpacked serialization layer with a pure functional API.

**Status:** Serialization complete. Transport layer in progress.

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends:
    gbnet-hs
```

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
import Data.Word (Word8, Word16)

-- Serialize multiple typed values into one buffer
let buf = bitSerialize (99 :: Word8)
        $ bitSerialize (5000 :: Word16)
        $ bitSerialize True
        $ empty
```

### Monadic Deserialization

```haskell
import GBNet.Serialize.Reader

-- Clean sequential reads with automatic error propagation
let reader = do
      flag   <- deserializeM :: BitReader Bool
      health <- deserializeM :: BitReader Word16
      id_    <- deserializeM :: BitReader Word8
      pure (flag, health, id_)

case runBitReader reader buf of
  Left err            -> handleError err
  Right (result, buf') -> use result
```

---

## Serialization

| Type | Bits | Haskell | Rust Equivalent |
|------|------|---------|-----------------|
| `Bool` | 1 | `bitSerialize True` | `bit_serialize(&true)` |
| `Word8` | 8 | `bitSerialize (42 :: Word8)` | `bit_serialize(&42u8)` |
| `Word16` | 16 | `bitSerialize (1234 :: Word16)` | `bit_serialize(&1234u16)` |
| `Word32` | 32 | `bitSerialize (n :: Word32)` | `bit_serialize(&n)` |
| `Word64` | 64 | `bitSerialize (n :: Word64)` | `bit_serialize(&n)` |
| `Int8` | 8 | `bitSerialize (-1 :: Int8)` | `bit_serialize(&-1i8)` |
| `Int16` | 16 | `bitSerialize (n :: Int16)` | `bit_serialize(&n)` |
| `Int32` | 32 | `bitSerialize (n :: Int32)` | `bit_serialize(&n)` |
| `Int64` | 64 | `bitSerialize (n :: Int64)` | `bit_serialize(&n)` |
| `Float` | 32 | `bitSerialize (3.14 :: Float)` | `bit_serialize(&3.14f32)` |
| `Double` | 64 | `bitSerialize (n :: Double)` | `bit_serialize(&n)` |
| Custom N | N | `writeBits value n` | `write_bits(value, n)` |

---

## Architecture

```
gbnet-hs/
├── src/
│   └── GBNet/
│       ├── Packet.hs              # Packet types & header wire format
│       └── Serialize/
│           ├── BitBuffer.hs       # Bit-level read/write buffer
│           ├── Class.hs           # BitSerialize / BitDeserialize typeclasses
│           └── Reader.hs          # BitReader monad for clean deserialization
├── test/
│   └── Main.hs                   # Serialization round-trip tests
├── .github/
│   └── workflows/
│       └── ci.yml                # GitHub Actions CI
└── gbnet-hs.cabal               # Package manifest
```

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.6.

```bash
cabal build              # Build library
cabal test               # Run all tests
cabal build -Werror      # Warnings as errors
cabal haddock            # Generate docs
```

---

## Roadmap

- [x] Bitpacked serialization (BitBuffer)
- [x] Typeclass-based serialize/deserialize
- [x] Monadic reader (BitReader)
- [x] Packet header / wire format
- [ ] Reliability layer (RTT, ACK, retransmit)
- [ ] Channel system (delivery modes)
- [ ] Connection state machine
- [ ] UDP transport (NetServer / NetClient)

---

## Contributing

Contributions welcome. Run `cabal test && cabal build --ghc-options="-Werror"` before submitting PRs.

---

<p align="center">
  <sub>MIT License · Built by <a href="https://github.com/aoinoikaz">Gondola Bros Entertainment</a></sub>
</p>
