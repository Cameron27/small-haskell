module TypeChecker.Core.Com where

import Common.Formatting
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Dec
import {-# SOURCE #-} TypeChecker.Core.Exp
import TypeChecker.Core.Types
import TypeChecker.Features.For
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification

-- | @typeCom c r@ type checks command `c` under the environment `r`.
typeCom :: Com -> TEnv -> Either TypeError ()
typeCom (Assign e1 e2) r = do
  t1 <- typeExp e1 r
  t2 <- typeExp e2 r >>= rval (Assign e1 e2)
  if t2 `assignable` t1
    then return ()
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"." (show t2) (show t1) (pretty (Assign e1 e2))
typeCom (Output e1) r = do
  t <- typeExp e1 r >>= rval (Output e1)
  if isPrintable t
    then return ()
    else err $ printf "\"%s\" is not a printable type in \"%s\"." (show t) (pretty (Output e1))
typeCom (Proc e1 es) r = do
  p <- typeExp e1 r
  ts <- foldr (\e ts -> do t <- typeExp e r; (t :) <$> ts) (Right []) es
  case p of
    TProc pts ->
      if (length ts == length pts) && and (zipWith (<:) ts pts)
        then return ()
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"." (show ts) (show pts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a procedure in \"%s\"." (show p) (pretty (Proc e1 es))
typeCom (If e1 c1 c2) r = do
  t <- typeExp e1 r >>= rval (If e1 c1 c2)
  if t <: TBool
    then do
      typeCom c1 r
      typeCom c2 r
    else err $ printf "test cannot be \"%s\" in \"%s\"." (show t) (pretty (If e1 c1 c2))
typeCom (While e1 c1) r = do
  t <- typeExp e1 r >>= rval (While e1 c1)
  if t <: TBool
    then typeCom c1 r
    else err $ printf "test cannot be \"%s\" in \"%s\"." (show t) (pretty (While e1 c1))
typeCom (Repeat e1 c1) r = do
  t <- typeExp e1 r >>= rval (Repeat e1 c1)
  if t <: TBool
    then typeCom c1 r
    else err $ printf "test cannot be \"%s\" in \"%s\"." (show t) (pretty (Repeat e1 c1))
typeCom (For i1 f1 c1) r = typeForCom (For i1 f1 c1) r
typeCom (Trap cs is) r = do
  typeCom (head cs) (updateTEnv (newTEnvMulti is ts) r)
  foldl (\c1 c2 -> c1 >> typeCom c2 r) (Right ()) (tail cs)
  where
    ts = TEscape : ts
typeCom (Escape i1) r = do
  t <- lookupTEnv i1 r
  if t <: TEscape
    then return ()
    else err $ printf "escape cannot be applied to a \"%s\" in \"%s\"." (show t) (pretty (Escape i1))
typeCom (Return e1) r = do
  t <- typeExp e1 r >>= rval (Return e1)
  if t <: rt
    then return ()
    else err $ printf "return type \"%s\" does not match expected return type \"%s\" in \"%s\"." (show t) (show rt) (pretty (Return e1))
  where
    (TEnv _ _ rt _ _) = r
typeCom (WithDo e1 c1) r = do
  t <- typeExp e1 r >>= rval (WithDo e1 c1)
  if t <: TRecordAny
    then typeCom c1 (updateTEnv (recordEnvironment t) r)
    else err $ printf "with cannot be performed on type \"%s\" in \"%s\"." (show t) (pretty (WithDo e1 c1))
typeCom (Block d1 c1) r = do
  r1 <- typeDec d1 r
  typeCom c1 (updateTEnv r1 r)
typeCom (Chain c1 c2) r =
  do
    typeCom c1 r
    typeCom c2 r
typeCom Skip r = Right ()