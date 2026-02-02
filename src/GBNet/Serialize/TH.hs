{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : GBNet.Serialize.TH
-- Description : Template Haskell derive for BitSerialize/BitDeserialize
--
-- Provides 'deriveNetworkSerialize' — a TH splice that generates
-- 'BitSerialize' and 'BitDeserialize' instances for user-defined types.
--
-- Supports:
--   * Records (named fields) — fields serialized in declaration order
--   * Simple enums (no payloads) — tag encoded with minimum bits
--   * Enums with payloads — tag followed by constructor fields

module GBNet.Serialize.TH
  ( deriveNetworkSerialize
  ) where

import Language.Haskell.TH
import GBNet.Serialize.BitBuffer (ReadResult(..), writeBits, readBits)
import GBNet.Serialize.Class (BitSerialize(..), BitDeserialize(..))

-- | Number of bits needed to represent @n@ distinct values.
ceilLog2 :: Int -> Int
ceilLog2 n
  | n <= 1    = 0
  | n <= 2    = 1
  | otherwise = 1 + ceilLog2 ((n + 1) `div` 2)

-- | Derive 'BitSerialize' and 'BitDeserialize' instances for a type.
deriveNetworkSerialize :: Name -> Q [Dec]
deriveNetworkSerialize typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ cons _) -> do
      serInst   <- mkSerializeInstance typeName cons
      deserInst <- mkDeserializeInstance typeName cons
      return [serInst, deserInst]
    _ -> fail $ "deriveNetworkSerialize: " ++ show typeName ++ " is not a data type"

-- ====================================================================
-- Serialization
-- ====================================================================

mkSerializeInstance :: Name -> [Con] -> Q Dec
mkSerializeInstance typeName cons = do
  let tagBits = ceilLog2 (length cons)
  clauses <- mapM (mkSerClause tagBits (length cons)) (zip [0..] cons)
  return $ InstanceD Nothing []
    (AppT (ConT ''BitSerialize) (ConT typeName))
    [FunD 'bitSerialize clauses]

mkSerClause :: Int -> Int -> (Int, Con) -> Q Clause
mkSerClause tagBits numCons (tagVal, con) = do
  let (conName, fieldCount) = conInfo con
  varNames <- mapM (\i -> newName ("f" ++ show i)) [0 .. fieldCount - 1]
  let pat = ConP conName [] (map VarP varNames)
  -- Build the body as a composition of writes
  -- We need: \buf -> writeFieldN ... (writeField1 (writeTag buf))
  -- As a composition: bitSerialize fN . ... . bitSerialize f1 . writeBits tag tagBits
  let tagExpr = if numCons > 1
                then Just [| writeBits $(litE (integerL (fromIntegral tagVal)))
                                       $(litE (integerL (fromIntegral tagBits))) |]
                else Nothing
      fieldExprs = map (\v -> [| bitSerialize $(varE v) |]) varNames
      allExprs = case tagExpr of
                   Just t  -> t : fieldExprs
                   Nothing -> fieldExprs
  body <- case allExprs of
    []  -> [| id |]
    [e] -> e
    es  -> foldr1 (\later earlier -> [| $later . $earlier |]) (reverse es)
  return $ Clause [pat] (NormalB body) []

-- ====================================================================
-- Deserialization
-- ====================================================================

mkDeserializeInstance :: Name -> [Con] -> Q Dec
mkDeserializeInstance typeName cons = do
  let tagBits = ceilLog2 (length cons)
  deserClause <- mkDeserClause tagBits cons
  return $ InstanceD Nothing []
    (AppT (ConT ''BitDeserialize) (ConT typeName))
    [FunD 'bitDeserialize [deserClause]]

mkDeserClause :: Int -> [Con] -> Q Clause
mkDeserClause tagBits cons = do
  bufName <- newName "buf"
  body <- if length cons == 1
    then do
      -- Single constructor, no tag
      let (conName, fieldCount) = conInfo (head cons)
      mkReadFields conName fieldCount (varE bufName)
    else do
      -- Read tag, then dispatch
      tagName <- newName "tagVal"
      buf'Name <- newName "buf'"
      matches <- mapM (mkConMatch buf'Name) (zip [0..] cons)
      let wildMatch = Match WildP (NormalB (AppE (ConE 'Left) (LitE (StringL "bitDeserialize: invalid tag")))) []
      [| case readBits $(litE (integerL (fromIntegral tagBits))) $(varE bufName) of
           Left err -> Left err
           Right (ReadResult $(varP tagName) $(varP buf'Name)) ->
             $(return $ CaseE (VarE tagName) (matches ++ [wildMatch]))
       |]
  return $ Clause [VarP bufName] (NormalB body) []

mkConMatch :: Name -> (Int, Con) -> Q Match
mkConMatch bufName (tagVal, con) = do
  let (conName, fieldCount) = conInfo con
  body <- mkReadFields conName fieldCount (varE bufName)
  return $ Match (LitP (IntegerL (fromIntegral tagVal))) (NormalB body) []

-- | Generate code that reads N fields and applies them to a constructor.
mkReadFields :: Name -> Int -> Q Exp -> Q Exp
mkReadFields conName 0 bufExpr =
  [| Right (ReadResult $(conE conName) $bufExpr) |]
mkReadFields conName fieldCount bufExpr = do
  -- Generate a chain of bitDeserialize calls
  mkReadFieldsLoop conName fieldCount 0 [] bufExpr

mkReadFieldsLoop :: Name -> Int -> Int -> [Name] -> Q Exp -> Q Exp
mkReadFieldsLoop conName total current vars bufExpr
  | current == total = do
      let conApp = foldl (\e v -> AppE e (VarE v)) (ConE conName) vars
      [| Right (ReadResult $(return conApp) $bufExpr) |]
  | otherwise = do
      vName  <- newName ("v" ++ show current)
      bName  <- newName ("b" ++ show current)
      rest   <- mkReadFieldsLoop conName total (current + 1) (vars ++ [vName]) (varE bName)
      [| case bitDeserialize $bufExpr of
           Left err -> Left err
           Right (ReadResult $(varP vName) $(varP bName)) -> $(return rest)
       |]

-- | Extract constructor name and field count.
conInfo :: Con -> (Name, Int)
conInfo (NormalC name fields)      = (name, length fields)
conInfo (RecC name fields)         = (name, length fields)
conInfo (InfixC _ name _)          = (name, 2)
conInfo (ForallC _ _ con)          = conInfo con
conInfo (GadtC [name] fields _)    = (name, length fields)
conInfo (RecGadtC [name] fields _) = (name, length fields)
conInfo _ = error "deriveNetworkSerialize: unsupported constructor form"
