{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : GBNet.Serialize.TH
-- Description : Template Haskell derive for BitSerialize/BitDeserialize
--
-- Provides 'deriveNetworkSerialize' to auto-generate serialization instances.
-- Supports records, simple enums, and enums with payloads.
module GBNet.Serialize.TH
  ( deriveNetworkSerialize,
  )
where

import GBNet.Serialize.BitBuffer (ReadResult (..), readBits, writeBits)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import Language.Haskell.TH

-- | Number of bits needed to represent n distinct values.
ceilLog2 :: Int -> Int
ceilLog2 n
  | n <= 1 = 0
  | n <= 2 = 1
  | otherwise = 1 + ceilLog2 ((n + 1) `div` 2)

-- | Derive BitSerialize and BitDeserialize instances for a type.
deriveNetworkSerialize :: Name -> Q [Dec]
deriveNetworkSerialize typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ cons _) -> do
      serInst <- mkSerializeInstance typeName cons
      deserInst <- mkDeserializeInstance typeName cons
      return [serInst, deserInst]
    _ -> fail $ "deriveNetworkSerialize: " ++ show typeName ++ " is not a data type"

mkSerializeInstance :: Name -> [Con] -> Q Dec
mkSerializeInstance typeName cons = do
  let tagBits = ceilLog2 (length cons)
  clauses <- mapM (mkSerClause tagBits (length cons)) (zip [0 ..] cons)
  return $
    InstanceD
      Nothing
      []
      (AppT (ConT ''BitSerialize) (ConT typeName))
      [FunD 'bitSerialize clauses]

mkSerClause :: Int -> Int -> (Int, Con) -> Q Clause
mkSerClause tagBits numCons (tagVal, con) = do
  let (conName, fieldTypes) = conFieldTypes con
      fieldCount = length fieldTypes
  varNames <- mapM (\i -> newName ("f" ++ show i)) [0 .. fieldCount - 1]
  let pat = ConP conName [] (map VarP varNames)
      tagExpr =
        if numCons > 1
          then
            Just
              [|
                writeBits
                  $(litE (integerL (fromIntegral tagVal)))
                  $(litE (integerL (fromIntegral tagBits)))
                |]
          else Nothing
      fieldExprs = zipWith mkFieldSerExpr varNames fieldTypes
      allExprs = case tagExpr of
        Just t -> t : fieldExprs
        Nothing -> fieldExprs
  body <- case allExprs of
    [] -> [|id|]
    [e] -> e
    es -> foldr1 (\later earlier -> [|$later . $earlier|]) (reverse es)
  return $ Clause [pat] (NormalB body) []

mkFieldSerExpr :: Name -> Type -> Q Exp
mkFieldSerExpr v _ty =
  [|bitSerialize $(varE v)|]

mkDeserializeInstance :: Name -> [Con] -> Q Dec
mkDeserializeInstance typeName cons = do
  let tagBits = ceilLog2 (length cons)
  deserClause <- mkDeserClause tagBits cons
  return $
    InstanceD
      Nothing
      []
      (AppT (ConT ''BitDeserialize) (ConT typeName))
      [FunD 'bitDeserialize [deserClause]]

mkDeserClause :: Int -> [Con] -> Q Clause
mkDeserClause tagBits cons = do
  bufName <- newName "buf"
  body <-
    if length cons == 1
      then do
        let (conName, fieldTypes) = conFieldTypes (head cons)
        mkReadFields conName fieldTypes (varE bufName)
      else do
        tagName <- newName "tagVal"
        buf'Name <- newName "buf'"
        matches <- mapM (mkConMatch buf'Name) (zip [0 ..] cons)
        let wildMatch = Match WildP (NormalB (AppE (ConE 'Left) (LitE (StringL "bitDeserialize: invalid tag")))) []
        [|
          case readBits $(litE (integerL (fromIntegral tagBits))) $(varE bufName) of
            Left err -> Left err
            Right (ReadResult $(varP tagName) $(varP buf'Name)) ->
              $(return $ CaseE (VarE tagName) (matches ++ [wildMatch]))
          |]
  return $ Clause [VarP bufName] (NormalB body) []

mkConMatch :: Name -> (Int, Con) -> Q Match
mkConMatch bufName (tagVal, con) = do
  let (conName, fieldTypes) = conFieldTypes con
  body <- mkReadFields conName fieldTypes (varE bufName)
  return $ Match (LitP (IntegerL (fromIntegral tagVal))) (NormalB body) []

mkReadFields :: Name -> [Type] -> Q Exp -> Q Exp
mkReadFields conName [] bufExpr =
  [|Right (ReadResult $(conE conName) $bufExpr)|]
mkReadFields conName fieldTypes bufExpr =
  mkReadFieldsLoop conName fieldTypes 0 [] bufExpr

mkReadFieldsLoop :: Name -> [Type] -> Int -> [Name] -> Q Exp -> Q Exp
mkReadFieldsLoop conName types current vars bufExpr
  | current == length types = do
      let conApp = foldl (\e v -> AppE e (VarE v)) (ConE conName) vars
      [|Right (ReadResult $(return conApp) $bufExpr)|]
  | otherwise = do
      vName <- newName ("v" ++ show current)
      bName <- newName ("b" ++ show current)
      rest <- mkReadFieldsLoop conName types (current + 1) (vars ++ [vName]) (varE bName)
      [|
        case bitDeserialize $bufExpr of
          Left err -> Left err
          Right (ReadResult $(varP vName) $(varP bName)) -> $(return rest)
        |]

conFieldTypes :: Con -> (Name, [Type])
conFieldTypes (NormalC name fields) = (name, map snd fields)
conFieldTypes (RecC name fields) = (name, map (\(_, _, t) -> t) fields)
conFieldTypes (InfixC (_, t1) name (_, t2)) = (name, [t1, t2])
conFieldTypes (ForallC _ _ con) = conFieldTypes con
conFieldTypes (GadtC [name] fields _) = (name, map snd fields)
conFieldTypes (RecGadtC [name] fields _) = (name, map (\(_, _, t) -> t) fields)
conFieldTypes _ = error "deriveNetworkSerialize: unsupported constructor form"
