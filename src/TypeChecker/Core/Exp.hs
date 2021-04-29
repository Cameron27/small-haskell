module TypeChecker.Core.Exp where

import Common.Formatting
import Common.Functions
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Com
import TypeChecker.Core.Type
import TypeChecker.Core.Types
import TypeChecker.Features.BasicOperations
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification

typeExp :: Exp -> TEnv -> Either TypeError Type
typeExp (Int _) r = Right TInt
typeExp (Double _) r = Right TDouble
typeExp (Bool _) r = Right TBool
typeExp (String _) r = Right TString
typeExp Read r = Right TString
typeExp (I i1) r = lookupTEnv i1 r
typeExp (RefExp e1) r = typeExp e1 r >>= ref (RefExp e1)
typeExp (ArrayExp e1 e2 t1) r = do
  typeType t1
  te1 <- typeExp e1 r >>= rval (ArrayExp e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayExp e1 e2 t1)
  if te1 `eq` TInt && te2 `eq` TInt
    then TArray <$> ref (ArrayExp e1 e2 t1) t1
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayExp e1 e2 t1))
typeExp (RecordExp is ts) r = do
  typeTypes ts
  if allDifferent is
    then do
      ts' <- foldr (\t ts' -> do t' <- ref (RecordExp is ts) t; (t' :) <$> ts') (Right []) ts
      return $ TRecord (zip is ts')
    else err $ printf "all identifiers must be unique in \"%s\"" (pretty (RecordExp is ts))
typeExp (Func e1 es) r = do
  f <- typeExp e1 r
  ts <- foldr (\e ts -> do t <- typeExp e r; (t :) <$> ts) (Right []) es
  case f of
    TFunc fts t ->
      if (length ts == length fts) && and (zipWith eq ts fts)
        then return t
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"" (show ts) (show fts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a function in \"%s\"" (show f) (pretty (Proc e1 es))
typeExp (IfExp e1 e2 e3) r = do
  t <- typeExp e1 r >>= rval (IfExp e1 e2 e3)
  if t `eq` TBool
    then do
      t1 <- typeExp e2 r
      t2 <- typeExp e3 r
      tryMerge (IfExp e1 e2 e3) t1 t2
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (IfExp e1 e2 e3))
typeExp (Valof t1 c1) (TEnv r _) = do
  typeType t1
  typeCom c1 (TEnv r t1)
  return t1
typeExp (Cont e1) r = do
  t <- typeExp e1 r
  if t `eq` TRefAny
    then return $ deref t
    else err $ printf "cont cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Cont e1))
typeExp (ArrayAccess e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (ArrayAccess e1 e2)
  t2 <- typeExp e2 r >>= rval (ArrayAccess e1 e2)
  if t1 `eq` TArrayAny
    then
      if t2 `eq` TInt
        then return $ arrayType t1
        else err $ printf "array access cannot have index of type \"%s\" in \"%s\"" (show t2) (pretty (ArrayAccess e1 e2))
    else err $ printf "array access cannot be performed on type \"%s\" in \"%s\"" (show t1) (pretty (ArrayAccess e1 e2))
typeExp (Dot e1 e2) r = do
  t <- typeExp e1 r >>= rval (Dot e1 e2)
  if t `eq` TRecordAny
    then typeExp e2 (updateTEnv (uncurry newTEnvMulti (unzip $ recordTypes t)) r)
    else err $ printf "dot operation be performed on type \"%s\" in \"%s\"" (show t) (pretty (Dot e1 e2))
typeExp (Not e1) r = do
  t <- typeExp e1 r >>= rval (Not e1)
  if t `eq` TBool
    then return TBool
    else err $ printf "! cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Not e1))
typeExp (Op o1 e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (Op o1 e1 e2)
  t2 <- typeExp e2 r >>= rval (Op o1 e1 e2)
  typeOp (Op o1 e1 e2) o1 (t1, t2)