module TypeChecker.Features.Classes where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
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
-- under the environment `r`.
typeClassDec :: Dec -> TEnv -> Either TypeError TEnv
typeClassDec (ClassDec i1 scds) r = do
  (c11, c12) <- typeSCDecInterface scds (updateTEnv (fst $ newClassTEnv i1 emptyClassId HashMap.empty r) r) -- Generate the interface with public and private variables
  if HashSet.null $ HashSet.intersection (HashMap.keysSet c11) (HashMap.keysSet c12)
    then return ()
    else
      err $
        printf
          "The identifiers %s appear as both public and private values in \"%s\"."
          (show $ HashSet.intersection (HashMap.keysSet c11) (HashMap.keysSet c12))
          (pretty (ClassDec i1 scds))
  let (r1', o) = newClassTEnv i1 emptyClassId (HashMap.union c12 c11) r
  c2 <- typeSCDec scds (updateTEnv r1' r') o -- Type check the class using the interface with only public variables
  let (r2', _) = newClassTEnv i1 emptyClassId c2 r
  return r2'
  where
    r' = let TEnv r'' c _ _ i = r in TEnv r'' c TVoid emptyClassId i -- Environment with on return address or current object id
typeClassDec d1 _ = error $ printf "Cannot run typeClassDec with \"%s\"." (pretty d1)

-- | @typeSCDec scd r c@ returns an environment containing the public information of scoped class declaration `scd` if `scd`
-- type checks under the environment `r` using the class `c` as "this" where relevant.
typeSCDec :: SCDec -> TEnv -> ClassId -> Either TypeError TypeMap
typeSCDec (Public cd1) r o = typeCDec cd1 r o
typeSCDec (Private cd1) r o = do
  typeCDec cd1 r o
  return HashMap.empty
typeSCDec SkipSCDec r o = return HashMap.empty
typeSCDec (ChainSCDec scd1 scd2) r c = do
  r1 <- typeSCDec scd1 r c
  r2 <- typeSCDec scd2 r c
  return (HashMap.union r2 r1)

-- | @typeSCDecInterface scd r@ returns an environment containing the public and private information of scoped class
-- declaration `scd` if `scd` type checks under the environment `r` but does not check any of the right hand values or
-- procedure/function bodies. \\
-- This is used to generate the type information of a class which can then be used to fully type check a class.
typeSCDecInterface :: SCDec -> TEnv -> Either TypeError (TypeMap, TypeMap)
typeSCDecInterface (Public cd1) r = do
  r' <- typeCDecInterface cd1 r
  return (r', HashMap.empty)
typeSCDecInterface (Private cd1) r = do
  r' <- typeCDecInterface cd1 r
  return (HashMap.empty, r')
typeSCDecInterface SkipSCDec r = return (HashMap.empty, HashMap.empty)
typeSCDecInterface (ChainSCDec scd1 scd2) r = do
  (r11, r12) <- typeSCDecInterface scd1 r
  (r21, r22) <- typeSCDecInterface scd2 r
  return (HashMap.union r21 r11, HashMap.union r22 r12)

-- | @typeCDec cd r c@ returns an environment containing the information of class declaration `cd` if `cd` type
-- checks under the environment `r` using the class `c` as "this" where relevant.
typeCDec :: CDec -> TEnv -> ClassId -> Either TypeError TypeMap
typeCDec (ProcDec i1 is ts c1) r o = do
  ts <- typeTypes ts r
  typeCom c1 (updateThisTEnv o (updateTEnv (newTEnvMulti is ts) r)) -- Set "this" to the class when checking the body
  return $ HashMap.singleton i1 (TProc ts) -- Procedures in classes are methods
typeCDec (FuncDec i1 is ts t1 e1) r o = do
  ts <- typeTypes ts r
  t1 <- typeType t1 r
  t <- typeExp e1 (updateThisTEnv o (updateTEnv (newTEnvMulti is ts) r)) -- Set "this" to the class when checking the body
  if subtype r t t1
    then return $ HashMap.singleton i1 (TFunc ts t1) -- Functions in classes are methods
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"." (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeCDec d1 r _ = do
  TEnv r _ _ _ _ <- typeDec d1 r
  return r

-- | @typeCDecInterface cd r@ returns an environment containing the information of class declaration `cd` if `cd` type
-- checks under the environment `r` but does not check any of the right hand values or procedure/function bodies.
typeCDecInterface :: CDec -> TEnv -> Either TypeError TypeMap
typeCDecInterface (Const i1 t1 e1) r = do
  t1 <- typeType t1 r
  return $ HashMap.singleton i1 t1
typeCDecInterface (Var i1 t1 e1) r = do
  t1 <- typeType t1 r
  t1' <- ref (Var i1 t1 e1) t1
  return $ HashMap.singleton i1 t1'
typeCDecInterface (ArrayDec i1 e1 e2 t1) r = do
  t1 <- typeType t1 r
  HashMap.singleton i1 <$> (TArray <$> ref (ArrayDec i1 e1 e2 t1) t1)
typeCDecInterface (RecordDec i1 is ts) r = do
  ts <- typeTypes ts r
  ts' <- mapM (ref (RecordDec i1 is ts)) ts
  return $ HashMap.singleton i1 $ TRecord (zip is ts')
typeCDecInterface (FileDec i1 i2 t1) r = do
  t1 <- typeType t1 r
  t1' <- ref (FileDec i1 i2 t1) t1
  t2' <- ref (FileDec i1 i2 t1) (TFile t1)
  return $ HashMap.fromList [(i1, t2'), (i2, t1')]
typeCDecInterface (ProcDec i1 is ts c1) r = do
  ts <- typeTypes ts r
  return $ HashMap.singleton i1 (TProc ts)
typeCDecInterface (FuncDec i1 is ts t1 e1) r = do
  t1 <- typeType t1 r
  ts <- typeTypes ts r
  return $ HashMap.singleton i1 (TFunc ts t1)
typeCDecInterface (ChainDec d1 d2) r = do
  r1 <- typeCDecInterface d1 r
  r2 <- typeCDecInterface d2 r
  return (HashMap.union r2 r1)
typeCDecInterface SkipDec r = Right HashMap.empty
typeCDecInterface cd1 _ = error $ printf "Cannot run typeCDecInterface with \"%s\"." (pretty cd1)

-- | @typeNewExp e r@ returns the type `e` represents if the new expression `e` type checks under the environment `r`.
typeNewExp :: Exp -> TEnv -> Either TypeError Type
typeNewExp (New i1) r = do
  c <- lookupTEnv i1 r
  if subtype r c TClassAny
    then let (TClass (Class i _ _)) = c in return $ TObject i
    else err $ printf "no class \"%s\" in \"%s\"." i1 (pretty (New i1))
typeNewExp e1 _ = error $ printf "Cannot run typeNewExp with \"%s\"." (pretty e1)

-- | @typeThisExp e r@ returns the type `e` represents if the this expression `e` type checks under the environment `r`.
typeThisExp :: Exp -> TEnv -> Either TypeError Type
typeThisExp This (TEnv _ _ _ i _) = return $ TObject i
typeThisExp e1 _ = error $ printf "Cannot run typeThisExp with \"%s\"." (pretty e1)

-- | @typeNullExp e r@ returns the type `e` represents if the null expression `e` type checks under the environment `r`.
typeNullExp :: Exp -> TEnv -> Either TypeError Type
typeNullExp Null r = return TNull
typeNullExp e1 _ = error $ printf "Cannot run typeNullExp with \"%s\"." (pretty e1)
