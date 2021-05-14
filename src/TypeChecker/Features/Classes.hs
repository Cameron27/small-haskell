module TypeChecker.Features.Classes where

import Common.Formatting
import Common.Functions
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Com
import {-# SOURCE #-} TypeChecker.Core.Dec
import {-# SOURCE #-} TypeChecker.Core.Exp
import TypeChecker.Core.Type
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification

typeClassDec :: Dec -> TEnv -> Either TypeError TEnv
typeClassDec (ClassDec i1 cds) r = do
  (TEnv c _ _) <- typeCDecInterface cds
  let r' = newTEnv i1 $ TClass $ Class i1 c
  typeCDec cds (updateThisTEnv emptyClass (updateTEnv r' r)) $ Class i1 c
  return r'

typeCDec :: Dec -> TEnv -> Class -> Either TypeError TEnv
typeCDec (ProcDec i1 is ts c1) r c = do
  typeTypes ts r
  typeCom c1 (updateThisTEnv c (updateTEnv (newTEnvMulti is ts) r))
  return $ newTEnv i1 (TMethod $ TProc ts)
typeCDec (FuncDec i1 is ts t1 e1) r c = do
  typeTypes ts r
  typeType t1 r
  t <- typeExp e1 (updateThisTEnv c (updateTEnv (newTEnvMulti is ts) r))
  if t <: t1
    then return $ newTEnv i1 (TMethod $ TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeCDec (ChainDec d1 d2) r c = do
  r1 <- typeCDec d1 r c
  r2 <- typeCDec d2 r c
  return (updateTEnv r2 r1)
typeCDec d1 r c = typeDec d1 r

typeCDecInterface :: Dec -> Either TypeError TEnv
typeCDecInterface (Const i1 t1 e1) = return $ newTEnv i1 t1
typeCDecInterface (Var i1 t1 e1) = do
  t1' <- ref (Var i1 t1 e1) t1
  return $ newTEnv i1 t1'
typeCDecInterface (ArrayDec i1 e1 e2 t1) = newTEnv i1 <$> (TArray <$> ref (ArrayDec i1 e1 e2 t1) t1)
typeCDecInterface (RecordDec i1 is ts) = do
  ts' <- foldr (\t ts' -> do t' <- ref (RecordDec i1 is ts) t; (t' :) <$> ts') (Right []) ts
  return $ newTEnv i1 $ TRecord (zip is ts')
typeCDecInterface (FileDec i1 i2 t1) = do
  t1' <- ref (FileDec i1 i2 t1) t1
  t2' <- ref (FileDec i1 i2 t1) (TFile t1)
  return $ newTEnvMulti [i1, i2] [t2', t1']
typeCDecInterface (ProcDec i1 is ts c1) = return $ newTEnv i1 (TMethod $ TProc ts)
typeCDecInterface (FuncDec i1 is ts t1 e1) = return $ newTEnv i1 (TMethod $ TFunc ts t1)
typeCDecInterface (ChainDec d1 d2) = do
  r1 <- typeCDecInterface d1
  r2 <- typeCDecInterface d2
  return (updateTEnv r2 r1)
typeCDecInterface SkipDec = Right emptyTEnv

typeNewExp :: Exp -> TEnv -> Either TypeError Type
typeNewExp (New i1) r = do
  c <- lookupTEnv i1 r
  if c <: TClassAny
    then return $ TObject i1
    else err $ printf "no class %s in %s" i1 (pretty (New i1))

typeThisExp :: Exp -> TEnv -> Either TypeError Type
typeThisExp This (TEnv _ _ (Class i _)) = return $ TObject i

typeNullExp :: Exp -> TEnv -> Either TypeError Type
typeNullExp Null r = return TNull
