module TypeChecker.Core.Exp where

import Common.Formatting
import Common.Functions
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Com
import TypeChecker.Core.Type
import TypeChecker.Core.Types
import TypeChecker.Features.BasicOperations
import TypeChecker.Features.Classes
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification

-- | @typeExp e r@ returns the type `e` represents if `e` type checks under the environment `r`.
typeExp :: Exp -> TEnv -> TypeResult Type
typeExp (Int _) r = Right TInt
typeExp (Double _) r = Right TDouble
typeExp (Bool _) r = Right TBool
typeExp (String _) r = Right TString
typeExp Read r = Right TString
typeExp (I i1) r = lookupTEnv i1 r
typeExp (RefExp e1) r = typeExp e1 r >>= ref (RefExp e1)
typeExp (ArrayExp e1 e2 t1) r = do
  t1 <- typeType t1 r
  te1 <- typeExp e1 r >>= rval (ArrayExp e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayExp e1 e2 t1)
  if subtype r te1 TInt && subtype r te2 TInt
    then TArray <$> ref (ArrayExp e1 e2 t1) t1
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"." (show te1) (show te2) (pretty (ArrayExp e1 e2 t1))
typeExp (RecordExp is ts) r = do
  ts <- typeTypes ts r
  if allDifferent is
    then do
      ts' <- mapM (ref (RecordExp is ts)) ts
      return $ TRecord (zip is ts')
    else err $ printf "all identifiers must be unique in \"%s\"." (pretty (RecordExp is ts))
typeExp (Func e1 es) r = do
  f <- typeExp e1 r
  ts <- mapM (`typeExp` r) es
  case f of
    TFunc fts t ->
      if (length ts == length fts) && and (zipWith (subtype r) ts fts)
        then return t
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"." (show ts) (show fts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a function in \"%s\"." (show f) (pretty (Proc e1 es))
typeExp (IfExp e1 e2 e3) r = do
  t <- typeExp e1 r >>= rval (IfExp e1 e2 e3)
  if subtype r t TBool
    then do
      t1 <- typeExp e2 r
      t2 <- typeExp e3 r
      tryMerge (IfExp e1 e2 e3) r t1 t2 -- Allows types where the only difference is one is references once more to be type compatible
    else err $ printf "test cannot be \"%s\" in \"%s\"." (show t) (pretty (IfExp e1 e2 e3))
typeExp (Valof t1 c1) (TEnv r c rt t i) = do
  t1 <- typeType t1 (TEnv r c rt t i)
  typeCom c1 (TEnv r c t1 t i)
  return t1
typeExp (Cont e1) r = do
  t <- typeExp e1 r
  if subtype r t TRefAny
    then return $ deref t
    else err $ printf "cont cannot be applied to type \"%s\" in \"%s\"." (show t) (pretty (Cont e1))
typeExp (ArrayAccess e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (ArrayAccess e1 e2)
  t2 <- typeExp e2 r >>= rval (ArrayAccess e1 e2)
  if subtype r t1 TArrayAny
    then
      if subtype r t2 TInt
        then return $ arrayType t1
        else err $ printf "array access cannot have index of type \"%s\" in \"%s\"." (show t2) (pretty (ArrayAccess e1 e2))
    else err $ printf "array access cannot be performed on type \"%s\" in \"%s\"." (show t1) (pretty (ArrayAccess e1 e2))
typeExp (Dot e1 e2) r = do
  t <- typeExp e1 r >>= rval (Dot e1 e2)
  process t
  where
    process t
      | subtype r t TRecordAny = typeExp e2 (updateTEnv (recordEnvironment t) r) -- Records evaluate e2 with the record updating the environment
      | subtype r t TObjectAny = typeExp e2 (updateTEnv (objectEnvironment t r) r) -- Objects evaluate e2 with the object updating the environment
      | otherwise = err $ printf "dot operation be performed on type \"%s\" in \"%s\"." (show t) (pretty (Dot e1 e2))
typeExp (New i1) r = typeNewExp (New i1) r
typeExp Null r = typeNullExp Null r
typeExp This r = typeThisExp This r
typeExp (Op2 o1 e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (Op2 o1 e1 e2)
  t2 <- typeExp e2 r >>= rval (Op2 o1 e1 e2)
  typeOp2 (Op2 o1 e1 e2) o1 (t1, t2)
typeExp (Op1 o1 e1) r = do
  t1 <- typeExp e1 r >>= rval (Op1 o1 e1)
  typeOp1 (Op1 o1 e1) o1 t1