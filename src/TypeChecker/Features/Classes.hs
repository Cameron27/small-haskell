module TypeChecker.Features.Classes where

import Common.Formatting
import Common.Functions
import qualified Data.HashMap.Strict as HashMap
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

-- | @typeClassDec d r@ returns an environment containing the information of declaration `d` if class `d` type checks
-- | under the environment `r`.
typeClassDec :: Dec -> TEnv -> Either TypeError TEnv
typeClassDec (ClassDec i1 scds) r = do
  (TEnv c11 _ _ _ _, TEnv c12 _ _ _ _) <- typeSCDecInterface scds (updateTEnv (fst $ newClassTEnv i1 HashMap.empty r) r) -- Generate the interface with public and private variables
  let (r1', c') = newClassTEnv i1 (HashMap.union c12 c11) r
  (TEnv c2 _ _ _ _) <- typeSCDec scds (updateThisTEnv emptyClass (updateTEnv r1' r)) c' -- Type check the class using the interface with only public variables
  let (r2', _) = newClassTEnv i1 c2 r
  return r2'

-- | @typeSCDec scd r c@ returns an environment containing the public information of scoped class declaration `scd` if `scd`
-- | type checks under the environment `r` using the class `c` as "this" where relevant.
typeSCDec :: SCDec -> TEnv -> Class -> Either TypeError TEnv
typeSCDec (Public cd1) r c = typeCDec cd1 r c
typeSCDec (Private cd1) r c = do
  typeCDec cd1 r c
  return emptyTEnv
typeSCDec SkipSCDec r c = return emptyTEnv
typeSCDec (ChainSCDec scd1 scd2) r c = do
  r1 <- typeSCDec scd1 r c
  r2 <- typeSCDec scd2 r c
  return (updateTEnv r2 r1)

-- | @typeSCDecInterface scd r@ returns an environment containing the public and private information of scoped class
-- | declaration `scd` if `scd` type checks under the environment `r` but does not check any of the right hand values or
-- | procedure/function bodies. \\
-- | This is used to generate the type information of a class which can then be used to fully type check a class.
typeSCDecInterface :: SCDec -> TEnv -> Either TypeError (TEnv, TEnv)
typeSCDecInterface (Public cd1) r = do
  r' <- typeCDecInterface cd1 r
  return (r', emptyTEnv)
typeSCDecInterface (Private cd1) r = do
  r' <- typeCDecInterface cd1 r
  return (emptyTEnv, r')
typeSCDecInterface SkipSCDec r = return (emptyTEnv, emptyTEnv)
typeSCDecInterface (ChainSCDec scd1 scd2) r = do
  (r11, r12) <- typeSCDecInterface scd1 r
  (r21, r22) <- typeSCDecInterface scd2 r
  return (updateTEnv r21 r11, updateTEnv r22 r12)

-- | @typeCDec cd r c@ returns an environment containing the information of class declaration `cd` if `cd` type
-- | checks under the environment `r` using the class `c` as "this" where relevant.
typeCDec :: CDec -> TEnv -> Class -> Either TypeError TEnv
typeCDec (ProcDec i1 is ts c1) r c = do
  ts <- typeTypes ts r
  typeCom c1 (updateThisTEnv c (updateTEnv (newTEnvMulti is ts) r)) -- Set "this" to the class when checking the body
  return $ newTEnv i1 (TMethod $ TProc ts) -- Procedures in classes are methods
typeCDec (FuncDec i1 is ts t1 e1) r c = do
  ts <- typeTypes ts r
  t1 <- typeType t1 r
  t <- typeExp e1 (updateThisTEnv c (updateTEnv (newTEnvMulti is ts) r)) -- Set "this" to the class when checking the body
  if t <: t1
    then return $ newTEnv i1 (TMethod $ TFunc ts t1) -- Functions in classes are methods
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeCDec d1 r c = typeDec d1 r

-- | @typeCDecInterface cd r@ returns an environment containing the information of class declaration `cd` if `cd` type
-- | checks under the environment `r` but does not check any of the right hand values or procedure/function bodies.
typeCDecInterface :: CDec -> TEnv -> Either TypeError TEnv
typeCDecInterface (Const i1 t1 e1) r = do
  t1 <- typeType t1 r
  return $ newTEnv i1 t1
typeCDecInterface (Var i1 t1 e1) r = do
  t1 <- typeType t1 r
  t1' <- ref (Var i1 t1 e1) t1
  return $ newTEnv i1 t1'
typeCDecInterface (ArrayDec i1 e1 e2 t1) r = do
  t1 <- typeType t1 r
  newTEnv i1 <$> (TArray <$> ref (ArrayDec i1 e1 e2 t1) t1)
typeCDecInterface (RecordDec i1 is ts) r = do
  ts <- typeTypes ts r
  ts' <- foldr (\t ts' -> do t' <- ref (RecordDec i1 is ts) t; (t' :) <$> ts') (Right []) ts
  return $ newTEnv i1 $ TRecord (zip is ts')
typeCDecInterface (FileDec i1 i2 t1) r = do
  t1 <- typeType t1 r
  t1' <- ref (FileDec i1 i2 t1) t1
  t2' <- ref (FileDec i1 i2 t1) (TFile t1)
  return $ newTEnvMulti [i1, i2] [t2', t1']
typeCDecInterface (ProcDec i1 is ts c1) r = do
  ts <- typeTypes ts r
  return $ newTEnv i1 (TMethod $ TProc ts)
typeCDecInterface (FuncDec i1 is ts t1 e1) r = do
  t1 <- typeType t1 r
  ts <- typeTypes ts r
  return $ newTEnv i1 (TMethod $ TFunc ts t1)
typeCDecInterface (ChainDec d1 d2) r = do
  r1 <- typeCDecInterface d1 r
  r2 <- typeCDecInterface d2 r
  return (updateTEnv r2 r1)
typeCDecInterface SkipDec r = Right emptyTEnv

-- | @typeNewExp e r@ returns the type `e` represents if the new expression `e` type checks under the environment `r`.
typeNewExp :: Exp -> TEnv -> Either TypeError Type
typeNewExp (New i1) r = do
  c <- lookupTEnv i1 r
  if c <: TClassAny
    then let (TClass (Class i _)) = c in return $ TObject i
    else err $ printf "no class \"%s\" in \"%s\"" i1 (pretty (New i1))

-- | @typeThisExp e r@ returns the type `e` represents if the this expression `e` type checks under the environment `r`.
typeThisExp :: Exp -> TEnv -> Either TypeError Type
typeThisExp This (TEnv _ _ _ (Class i _) _) = return $ TObject i

-- | @typeNullExp e r@ returns the type `e` represents if the null expression `e` type checks under the environment `r`.
typeNullExp :: Exp -> TEnv -> Either TypeError Type
typeNullExp Null r = return TNull
