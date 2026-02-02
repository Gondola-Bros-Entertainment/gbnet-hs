# CLAUDE.md

## Rules

- **Only commit and push when explicitly instructed.** Never amend commits. Never add `Co-Authored-By` headers.
- This is a public open-source project. All code must be clean, production-quality, and free of secrets or credentials.
- Prefer generic, abstracted, clean code. No hacky fixes (e.g. prefixing unused variables with `_` instead of implementing properly).
- Always optimize for performance. This is a game networking library — speed matters.
- Run `cabal test` and `cabal build --ghc-options="-Werror"` before considering any change complete.
- No magic numbers or hardcoded strings. Use named constants.

## Project Overview

gbnet-hs is the Haskell port of [GB-Net](https://github.com/Gondola-Bros-Entertainment/gb-net) — a transport-level game networking library providing reliable UDP with bitpacked serialization. The Rust version is the golden copy.

## Structure

- `src/GBNet/Serialize/BitBuffer.hs` - Bit-level read/write buffer, MSB-first encoding
- `src/GBNet/Serialize/Class.hs` - BitSerialize/BitDeserialize typeclasses, primitive instances
- `src/GBNet/Serialize/Reader.hs` - BitReader monad for clean sequential deserialization
- `test/Main.hs` - Serialization round-trip tests
- `.github/workflows/ci.yml` - GitHub Actions CI

## Build & Test

```
cabal build                         # Build library
cabal test                          # Run all tests
cabal build --ghc-options="-Werror" # Warnings as errors
```
