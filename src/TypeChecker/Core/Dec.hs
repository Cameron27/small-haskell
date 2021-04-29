module TypeChecker.Core.Dec where

import Common.Formatting
import Common.Functions
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Com
import {-# SOURCE #-} TypeChecker.Core.Exp
import TypeChecker.Core.Type
import TypeChecker.Core.Types
import TypeChecker.Features.DefaultEnvironment
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification

typeDec :: Dec -> TEnv -> Either TypeError TEnv
typeDec (Const i1 t1 e1) r = do
  t <- typeExp e1 r >>= rval (Const i1 t1 e1)
  if t `eq` t1
    then return $ newTEnv i1 t
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1) (pretty (Const i1 t1 e1))
typeDec (Var i1 t1 e1) r = do
  t <- typeExp e1 r >>= rval (Var i1 t1 e1)
  t1' <- ref (Var i1 t1 e1) t1
  if t `assignable` t1'
    then return $ newTEnv i1 t1'
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1') (pretty (Var i1 t1 e1))
typeDec (Own i1 t1 e1) r = do
  t <- typeExp e1 defaultTEnv >>= rval (Own i1 t1 e1)
  t1' <- ref (Own i1 t1 e1) t1
  if t `assignable` t1'
    then return $ newTEnv i1 t1'
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1') (pretty (Own i1 t1 e1))
typeDec (ArrayDec i1 e1 e2 t1) r = do
  typeType t1
  te1 <- typeExp e1 r >>= rval (ArrayDec i1 e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayDec i1 e1 e2 t1)
  if te1 `eq` TInt && te2 `eq` TInt
    then newTEnv i1 <$> (TArray <$> ref (ArrayDec i1 e1 e2 t1) t1)
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayDec i1 e1 e2 t1))
typeDec (RecordDec i1 is ts) r = do
  typeTypes ts
  if allDifferent is
    then do
      ts' <- foldr (\t ts' -> do t' <- ref (RecordDec i1 is ts) t; (t' :) <$> ts') (Right []) ts
      return $ newTEnv i1 $ TRecord (zip is ts')
    else err $ printf "all identifiers must be unique in \"%s\"" (pretty (RecordDec i1 is ts))
typeDec (FileDec i1 i2 t1) r = do
  typeType t1
  t1' <- ref (FileDec i1 i2 t1) t1
  t2' <- ref (FileDec i1 i2 t1) (TFile t1)
  return $ newTEnvMulti [i1, i2] [t2', t1']
typeDec (ProcDec i1 is ts c1) r = do
  typeTypes ts
  typeCom c1 (updateTEnv (newTEnvMulti is ts) r)
  return $ newTEnv i1 (TProc ts)
typeDec (RecProcDec i1 is ts c1) r = do
  typeTypes ts
  typeCom c1 (updateTEnv (newTEnvMulti (i1 : is) (TProc ts : ts)) r)
  return $ newTEnv i1 (TProc ts)
typeDec (FuncDec i1 is ts t1 e1) r = do
  typeTypes ts
  typeType t1
  t <- typeExp e1 (updateTEnv (newTEnvMulti is ts) r)
  if t `eq` t1
    then return $ newTEnv i1 (TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeDec (RecFuncDec i1 is ts t1 e1) r = do
  typeTypes ts
  typeType t1
  t <- typeExp e1 (updateTEnv (newTEnvMulti (i1 : is) (TFunc ts t1 : ts)) r)
  if t `eq` t1
    then return $ newTEnv i1 (TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeDec (ChainDec d1 d2) r = do
  r1 <- typeDec d1 r
  r2 <- typeDec d2 (updateTEnv r1 r)
  return (updateTEnv r2 r1)
typeDec SkipDec r = Right emptyTEnv