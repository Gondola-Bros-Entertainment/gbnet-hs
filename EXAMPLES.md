# Examples

Practical game networking serialization examples showcasing the gbnet-hs API.

All examples assume these imports:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import GBNet.Serialize.BitBuffer
import GBNet.Serialize.Class
import GBNet.Serialize.Reader
import GBNet.Serialize.TH
import GBNet.Packet

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text as T
```

---

## 1. FPS Player Snapshot

A compact player state using `BitWidth` for tight packing: 7-bit health
(0–127), 4-bit weapon ID (0–15), 10-bit X/Y coordinates (0–1023), and a
1-bit alive flag.

```haskell
data PlayerSnapshot = PlayerSnapshot
  { psAlive    :: Bool
  , psHealth   :: BitWidth 7 Word8
  , psWeaponId :: BitWidth 4 Word8
  , psX        :: BitWidth 10 Word16
  , psY        :: BitWidth 10 Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''PlayerSnapshot
```

Serialize, deserialize, and measure size:

```haskell
let player = PlayerSnapshot
      { psAlive    = True
      , psHealth   = BitWidth 100
      , psWeaponId = BitWidth 3
      , psX        = BitWidth 512
      , psY        = BitWidth 768
      }

-- Serialize
let buf = bitSerialize player empty
-- 1 + 7 + 4 + 10 + 10 = 32 bits (4 bytes)

-- Measure size
serializedSizeBits (bitSerialize player)   -- 32
serializedSizeBytes (bitSerialize player)  -- 4

-- Deserialize
case runBitReader deserializeM buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (snapshot, _) -> print snapshot
  -- PlayerSnapshot {psAlive = True, psHealth = BitWidth {unBitWidth = 100}, ...}
```

Compare to a naive approach using full-width types: `Bool` (8 bits as
`Word8`) + `Word8` + `Word8` + `Word16` + `Word16` = 56 bits. The
bitpacked version uses 32 bits — a **43% reduction**.

---

## 2. Chat System

A sum type with payloads representing different chat message kinds:

```haskell
data ChatMessage
  = SystemMessage T.Text
  | PlayerChat Word8 T.Text
  | TeamChat Word8 T.Text
  deriving (Eq, Show)

deriveNetworkSerialize ''ChatMessage
```

`Text` handles full Unicode, including emoji:

```haskell
let msgs =
      [ SystemMessage "Server restarting"
      , PlayerChat 7 "gg wp"
      , TeamChat 3 "push left \x1F680"  -- rocket emoji
      ]

-- Serialize all messages as a list
let buf = bitSerialize msgs empty

-- Deserialize
case runBitReader (deserializeM :: BitReader [ChatMessage]) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
  -- SystemMessage "Server restarting"
  -- PlayerChat 7 "gg wp"
  -- TeamChat 3 "push left 🚀"
```

The enum tag uses `ceilLog2(3) = 2` bits to distinguish the three
constructors. `Text` is serialized as a 16-bit byte-length prefix
followed by raw UTF-8 bytes — wire-compatible with the Rust `String`
serialization.

---

## 3. Game Event Stream

Multiple event types serialized as a list:

```haskell
data GameEvent
  = PlayerSpawned Word8 Word16 Word16    -- player ID, x, y
  | DamageDealt Word8 Word8 Word16       -- attacker, target, amount
  | ItemPickup Word8 Word16              -- player ID, item ID
  | PlayerDied Word8 Word8               -- player ID, killer ID
  deriving (Eq, Show)

deriveNetworkSerialize ''GameEvent
```

Deserialize with the `BitReader` monad using do-notation:

```haskell
let events =
      [ PlayerSpawned 1 100 200
      , DamageDealt 1 2 50
      , ItemPickup 2 1001
      , PlayerDied 2 1
      ]

let buf = bitSerialize events empty

-- The BitReader monad threads buffer state and error propagation
-- automatically — no manual case-matching on ReadResult needed.
let reader :: BitReader [GameEvent]
    reader = do
      count <- readBitsM 16  -- list length prefix
      sequence [ deserializeM | _ <- [1 .. count] ]

case runBitReader reader buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
```

Or simply use the `[a]` instance which handles the length prefix
automatically:

```haskell
case runBitReader (deserializeM :: BitReader [GameEvent]) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> mapM_ print result
```

---

## 4. Inventory System

`Maybe` for optional equipment slots. `Nothing` costs only 1 bit on the
wire; `Just x` costs 1 bit + the payload.

```haskell
-- Equipment slots: each is optional
type ItemId = BitWidth 8 Word8

let helmet = Just (BitWidth 12 :: ItemId)
let weapon = Just (BitWidth 3 :: ItemId)
let armor  = Nothing :: Maybe ItemId

-- Serialize a 3-tuple of optional slots
let loadout = (helmet, weapon, armor)
let buf = bitSerialize loadout empty

-- helmet: 1 (present) + 8 (item ID) = 9 bits
-- weapon: 1 (present) + 8 (item ID) = 9 bits
-- armor:  1 (absent)                 = 1 bit
-- Total: 19 bits

serializedSizeBits (bitSerialize loadout)  -- 19

case runBitReader (deserializeM :: BitReader (Maybe ItemId, Maybe ItemId, Maybe ItemId)) buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right (result, _) -> print result
  -- (Just (BitWidth {unBitWidth = 12}), Just (BitWidth {unBitWidth = 3}), Nothing)
```

Nested optional: `Maybe (Maybe Bool)` works out of the box:

```haskell
let nested = Just (Just True) :: Maybe (Maybe Bool)
let buf = bitSerialize nested empty
-- 1 (outer present) + 1 (inner present) + 1 (Bool) = 3 bits
serializedSizeBits (bitSerialize nested)  -- 3
```

---

## 5. Raw Bit Packing

Manual `writeBits` / `readBits` for a custom wire format. Pack a
movement input: 2-bit direction, 1-bit jump, 1-bit shoot, 4-bit
sequence number.

```haskell
let direction = 2 :: Word64  -- 0=up, 1=right, 2=down, 3=left
let jump      = True
let shoot     = False
let seqNum    = 13 :: Word64

-- Pack manually (writes are composed right-to-left)
let buf = writeBits seqNum 4
        $ writeBit shoot
        $ writeBit jump
        $ writeBits direction 2
          empty

-- 2 + 1 + 1 + 4 = 8 bits (exactly 1 byte)
bitPosition buf     -- 8
toBitString buf     -- "10101101"
                    -- ^^        direction = 2 (10)
                    --   ^       jump = 1
                    --    ^      shoot = 0
                    --     ^^^^  seqNum = 13 (1101)

-- Read back with the BitReader monad
let reader = do
      dir <- readBitsM 2
      j   <- readBitM
      s   <- readBitM
      sq  <- readBitsM 4
      pure (dir, j, s, sq)

case runBitReader reader buf of
  Left err -> putStrLn $ "Error: " ++ err
  Right ((dir, j, s, sq), _) ->
    putStrLn $ "dir=" ++ show dir
            ++ " jump=" ++ show j
            ++ " shoot=" ++ show s
            ++ " seq=" ++ show sq
    -- dir=2 jump=True shoot=False seq=13
```

Use `serializedSizeBits` to verify the wire size of any serialization:

```haskell
serializedSizeBits (writeBits 0 2 . writeBit False . writeBit False . writeBits 0 4)
-- 8
```

---

## 6. Packet Header + Payload

Build a complete packet: `PacketHeader` followed by a payload type, then
convert to bytes for the wire and back.

```haskell
data Payload = MovePayload
  { moveX :: Word16
  , moveY :: Word16
  } deriving (Eq, Show)

deriveNetworkSerialize ''MovePayload

let header = PacketHeader
      { packetType  = Payload
      , sequenceNum = 42
      , ack         = 40
      , ackBitfield = 0x00000003  -- ack-1 and ack-2 received
      }

let payload = MovePayload { moveX = 300, moveY = 750 }

-- Serialize header + payload into a single buffer
let buf = bitSerialize payload $ bitSerialize header empty

-- Convert to ByteString for sending over UDP
let bytes = toBytes buf
-- 68 bits (header) + 32 bits (payload) = 100 bits = 13 bytes

-- On the receiving end: fromBytes and deserialize
let rxBuf = fromBytes bytes

let reader = do
      hdr <- deserializeM :: BitReader PacketHeader
      msg <- deserializeM :: BitReader MovePayload
      pure (hdr, msg)

case runBitReader reader rxBuf of
  Left err -> putStrLn $ "Error: " ++ err
  Right ((hdr, msg), _) -> do
    putStrLn $ "Type: " ++ show (packetType hdr)
    putStrLn $ "Seq:  " ++ show (sequenceNum hdr)
    putStrLn $ "Move: " ++ show (moveX msg) ++ ", " ++ show (moveY msg)
    -- Type: Payload
    -- Seq:  42
    -- Move: 300, 750
```

End-to-end: serialize → `toBytes` → (network) → `fromBytes` → deserialize.
The wire format is identical to the Rust gbnet library, so a Haskell
client can communicate with a Rust server (and vice versa) with zero
conversion overhead.
