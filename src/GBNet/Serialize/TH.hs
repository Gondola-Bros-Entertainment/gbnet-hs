{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : GBNet.Serialize.TH
-- Description : Template Haskell for zero-allocation Storable instances
--
-- Generates 'Storable' instances for product types, enabling zero-allocation
-- serialization via 'serialize'. Wire format is always little-endian for
-- cross-platform compatibility. On LE platforms, the generated code compiles
-- to native memory operations with zero overhead.
--
-- Usage:
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
-- import GBNet.Serialize.TH
-- import GBNet.Serialize
--
-- data Vec3 = Vec3 !Float !Float !Float
-- deriveStorable ''Vec3
--
-- data Transform = Transform !Vec3 !Quaternion
-- deriveStorable ''Transform  -- nested types just work
--
-- -- Serialize any Storable type:
-- serialize (Vec3 1.0 2.0 3.0)        -- :: ByteString
-- serialize (Transform pos rot)       -- :: ByteString
-- @
module GBNet.Serialize.TH
  ( deriveStorable,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GBNet.Serialize
  ( peekDoubleLE,
    peekFloatLE,
    peekInt16LE,
    peekInt32LE,
    peekInt64LE,
    peekWord16LE,
    peekWord32LE,
    peekWord64LE,
    pokeDoubleLE,
    pokeFloatLE,
    pokeInt16LE,
    pokeInt32LE,
    pokeInt64LE,
    pokeWord16LE,
    pokeWord32LE,
    pokeWord64LE,
  )
import Language.Haskell.TH

-- | Byte sizes and alignments of primitive types.
primitiveSizeAlign :: Type -> Maybe (Int, Int)
primitiveSizeAlign (ConT n)
  | n == ''Word8 = Just (1, 1)
  | n == ''Word16 = Just (2, 2)
  | n == ''Word32 = Just (4, 4)
  | n == ''Word64 = Just (8, 8)
  | n == ''Int8 = Just (1, 1)
  | n == ''Int16 = Just (2, 2)
  | n == ''Int32 = Just (4, 4)
  | n == ''Int64 = Just (8, 8)
  | n == ''Float = Just (4, 4)
  | n == ''Double = Just (8, 8)
primitiveSizeAlign _ = Nothing

-- | Check if a type is a known primitive.
isPrimitive :: Type -> Bool
isPrimitive t = case primitiveSizeAlign t of
  Just _ -> True
  Nothing -> False

-- | Get the LE poke function for a multi-byte primitive.
-- Returns Nothing for single-byte types (no endianness concern).
lePokeName :: Type -> Maybe Name
lePokeName (ConT n)
  | n == ''Word16 = Just 'pokeWord16LE
  | n == ''Word32 = Just 'pokeWord32LE
  | n == ''Word64 = Just 'pokeWord64LE
  | n == ''Int16 = Just 'pokeInt16LE
  | n == ''Int32 = Just 'pokeInt32LE
  | n == ''Int64 = Just 'pokeInt64LE
  | n == ''Float = Just 'pokeFloatLE
  | n == ''Double = Just 'pokeDoubleLE
lePokeName _ = Nothing

-- | Get the LE peek function for a multi-byte primitive.
-- Returns Nothing for single-byte types (no endianness concern).
lePeekName :: Type -> Maybe Name
lePeekName (ConT n)
  | n == ''Word16 = Just 'peekWord16LE
  | n == ''Word32 = Just 'peekWord32LE
  | n == ''Word64 = Just 'peekWord64LE
  | n == ''Int16 = Just 'peekInt16LE
  | n == ''Int32 = Just 'peekInt32LE
  | n == ''Int64 = Just 'peekInt64LE
  | n == ''Float = Just 'peekFloatLE
  | n == ''Double = Just 'peekDoubleLE
lePeekName _ = Nothing

-- | Derive a Storable instance for a product type.
-- Supports primitives and nested Storable types.
deriveStorable :: Name -> Q [Dec]
deriveStorable typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [con] _) -> do
      let (conName, fieldTypes) = conFieldTypes con
      mkStorableInstance typeName conName fieldTypes
    TyConI DataD {} ->
      fail $ "deriveStorable: " ++ show typeName ++ " must have exactly one constructor (no sum types)"
    TyConI (NewtypeD _ _ _ _ con _) -> do
      let (conName, fieldTypes) = conFieldTypes con
      mkStorableInstance typeName conName fieldTypes
    _ -> fail $ "deriveStorable: " ++ show typeName ++ " is not a data type"

-- | Generate Storable instance.
mkStorableInstance :: Name -> Name -> [Type] -> Q [Dec]
mkStorableInstance typeName conName fieldTypes = do
  let fieldCount = length fieldTypes

  -- Generate sizeOf
  sizeOfBody <- mkSizeOfBody fieldTypes

  -- Generate alignment (max of field alignments, minimum 1)
  alignBody <- mkAlignmentBody fieldTypes

  -- Generate poke
  pokeBody <- mkPokeBody conName fieldTypes fieldCount

  -- Generate peek
  peekBody <- mkPeekBody conName fieldTypes

  -- Build the instance
  let inst =
        InstanceD
          Nothing
          [] -- No constraints for now; nested types must have Storable
          (AppT (ConT ''Storable) (ConT typeName))
          [ FunD 'sizeOf [Clause [WildP] (NormalB sizeOfBody) []],
            FunD 'alignment [Clause [WildP] (NormalB alignBody) []],
            FunD 'poke pokeBody,
            FunD 'peek peekBody
          ]
  return [inst]

-- | Generate sizeOf body: sum of all field sizes.
mkSizeOfBody :: [Type] -> Q Exp
mkSizeOfBody [] = litE (integerL 0)
mkSizeOfBody fieldTypes = do
  sizeExps <- mapM mkFieldSizeOf fieldTypes
  return $ foldl1 (\a b -> InfixE (Just a) (VarE '(+)) (Just b)) sizeExps

-- | Generate size expression for a single field.
mkFieldSizeOf :: Type -> Q Exp
mkFieldSizeOf t = case primitiveSizeAlign t of
  Just (size, _) -> litE (integerL (fromIntegral size))
  Nothing -> [|sizeOf (undefined :: $(return t))|]

-- | Generate alignment body: max of field alignments.
mkAlignmentBody :: [Type] -> Q Exp
mkAlignmentBody [] = litE (integerL 1)
mkAlignmentBody fieldTypes = do
  alignExps <- mapM mkFieldAlignment fieldTypes
  return $ foldl1 mkMaxApp alignExps
  where
    mkMaxApp a = AppE (AppE (VarE 'max) a)

-- | Generate alignment expression for a single field.
mkFieldAlignment :: Type -> Q Exp
mkFieldAlignment t = case primitiveSizeAlign t of
  Just (_, align) -> litE (integerL (fromIntegral align))
  Nothing -> [|alignment (undefined :: $(return t))|]

-- | Generate poke body.
mkPokeBody :: Name -> [Type] -> Int -> Q [Clause]
mkPokeBody conName fieldTypes fieldCount = do
  ptrName <- newName "ptr"
  varNames <- mapM (\i -> newName ("f" ++ show i)) [0 .. fieldCount - 1]

  let pat = ConP conName [] (map VarP varNames)

  -- Build offset calculations and poke statements
  stmts <- mkPokeStmts ptrName varNames fieldTypes

  let body = DoE Nothing stmts
  return [Clause [VarP ptrName, pat] (NormalB body) []]

-- | Generate poke statements with offset tracking.
mkPokeStmts :: Name -> [Name] -> [Type] -> Q [Stmt]
mkPokeStmts ptrName varNames fieldTypes = go 0 (zip varNames fieldTypes)
  where
    go _ [] = return []
    go offset ((varName, fieldType) : rest) = do
      stmt <- mkPokeStmt ptrName varName fieldType offset
      restStmts <- case primitiveSizeAlign fieldType of
        Just (size, _) -> go (offset + size) rest
        Nothing -> goAfterNested ptrName offset fieldType rest
      return (stmt : restStmts)

    -- After a nested type, offset must be calculated dynamically
    goAfterNested _ _ _ [] = return []
    goAfterNested ptr baseOffset prevType ((varName, fieldType) : rest) = do
      stmt <- mkPokeStmtDynamic ptr varName fieldType baseOffset prevType
      restStmts <- case primitiveSizeAlign fieldType of
        Just (size, _) -> goAfterNested ptr (baseOffset + size) fieldType rest
        Nothing -> goAfterNested ptr baseOffset fieldType rest
      return (stmt : restStmts)

-- | Generate a single poke statement for a field (LE-aware for multi-byte primitives).
mkPokeStmt :: Name -> Name -> Type -> Int -> Q Stmt
mkPokeStmt ptrName varName fieldType offset
  | isPrimitive fieldType = case lePokeName fieldType of
      Nothing ->
        -- Single-byte type, no endianness concern
        NoBindS <$> [|pokeByteOff $(varE ptrName) offset $(varE varName)|]
      Just pokeFn ->
        -- Multi-byte primitive, use LE helper
        NoBindS <$> [|$(varE pokeFn) $(varE ptrName) offset $(varE varName)|]
  | otherwise =
      -- Nested Storable type: poke at offset using castPtr
      NoBindS <$> [|poke (castPtr ($(varE ptrName) `plusPtr` offset)) $(varE varName)|]

-- | Generate poke with dynamic offset (after nested type).
mkPokeStmtDynamic :: Name -> Name -> Type -> Int -> Type -> Q Stmt
mkPokeStmtDynamic ptrName varName fieldType baseOffset prevType = do
  let offsetExpr = [|baseOffset + sizeOf (undefined :: $(return prevType))|]
  if isPrimitive fieldType
    then case lePokeName fieldType of
      Nothing ->
        NoBindS <$> [|pokeByteOff $(varE ptrName) ($offsetExpr) $(varE varName)|]
      Just pokeFn ->
        NoBindS <$> [|$(varE pokeFn) $(varE ptrName) ($offsetExpr) $(varE varName)|]
    else NoBindS <$> [|poke (castPtr ($(varE ptrName) `plusPtr` ($offsetExpr))) $(varE varName)|]

-- | Generate peek body.
mkPeekBody :: Name -> [Type] -> Q [Clause]
mkPeekBody conName fieldTypes = do
  ptrName <- newName "ptr"

  -- Build the peek expression using Applicative
  peekExpr <- mkPeekExpr ptrName conName fieldTypes

  return [Clause [VarP ptrName] (NormalB peekExpr) []]

-- | Generate peek expression using Applicative style.
-- Builds: pure Constructor <*> peek1 <*> peek2 <*> ...
mkPeekExpr :: Name -> Name -> [Type] -> Q Exp
mkPeekExpr _ conName [] = [|return $(return (ConE conName))|]
mkPeekExpr ptrName conName fieldTypes = do
  -- Start with: pure Constructor
  let startAcc = AppE (VarE 'pure) (ConE conName)
  go 0 fieldTypes startAcc
  where
    go _ [] acc = return acc
    go offset (t : ts) acc = do
      peekField <- mkPeekField ptrName t offset
      -- acc <*> peekField
      let newAcc = InfixE (Just acc) (VarE '(<*>)) (Just peekField)
      case primitiveSizeAlign t of
        Just (size, _) -> go (offset + size) ts newAcc
        Nothing -> goAfterNested ptrName offset t ts newAcc

    goAfterNested _ _ _ [] acc = return acc
    goAfterNested ptr baseOffset prevType (t : ts) acc = do
      peekField <- mkPeekFieldDynamic ptr t baseOffset prevType
      let newAcc = InfixE (Just acc) (VarE '(<*>)) (Just peekField)
      case primitiveSizeAlign t of
        Just (size, _) ->
          goAfterNested ptr (baseOffset + size) t ts newAcc
        Nothing ->
          goAfterNested ptr baseOffset t ts newAcc

-- | Generate peek for a single field (LE-aware for multi-byte primitives).
mkPeekField :: Name -> Type -> Int -> Q Exp
mkPeekField ptrName fieldType offset
  | isPrimitive fieldType = case lePeekName fieldType of
      Nothing -> [|peekByteOff $(varE ptrName) offset|]
      Just peekFn -> [|$(varE peekFn) $(varE ptrName) offset|]
  | otherwise = [|peek (castPtr ($(varE ptrName) `plusPtr` offset))|]

-- | Generate peek with dynamic offset.
mkPeekFieldDynamic :: Name -> Type -> Int -> Type -> Q Exp
mkPeekFieldDynamic ptrName fieldType baseOffset prevType = do
  let offsetExpr = [|baseOffset + sizeOf (undefined :: $(return prevType))|]
  if isPrimitive fieldType
    then case lePeekName fieldType of
      Nothing -> [|peekByteOff $(varE ptrName) ($offsetExpr)|]
      Just peekFn -> [|$(varE peekFn) $(varE ptrName) ($offsetExpr)|]
    else [|peek (castPtr ($(varE ptrName) `plusPtr` ($offsetExpr)))|]

-- | Extract constructor name and field types.
conFieldTypes :: Con -> (Name, [Type])
conFieldTypes (NormalC name fields) = (name, map snd fields)
conFieldTypes (RecC name fields) = (name, map (\(_, _, t) -> t) fields)
conFieldTypes (InfixC (_, t1) name (_, t2)) = (name, [t1, t2])
conFieldTypes (ForallC _ _ con) = conFieldTypes con
conFieldTypes (GadtC [name] fields _) = (name, map snd fields)
conFieldTypes (RecGadtC [name] fields _) = (name, map (\(_, _, t) -> t) fields)
conFieldTypes _ = error "deriveStorable: unsupported constructor form"
