module TypeChecker.Features.For where

import Common.Formatting
import Parser.Core.Types
import Text.Printf
import {-# SOURCE #-} TypeChecker.Core.Com
import TypeChecker.Core.Exp
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TypeModification
import Prelude hiding (max)

-- | @typeForCom c r@ type checks for command `c` under the environment `r`.
typeForCom :: Com -> TEnv -> TypeResult ()
typeForCom (For i1 f1 c1) r = do
  t1 <- typeExp (I i1) r
  t2 <- typeFor f1 r
  if assignable r t2 t1
    then typeCom c1 r
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"." (show t2) (show t1) (pretty (For i1 f1 c1))
typeForCom c1 _ = error $ printf "Cannot run typeForCom with \"%s\"." (pretty c1)

-- | @typeFor f r@ returns the type `f` represents if `f` type checks under the environment `r`.
typeFor :: For -> TEnv -> TypeResult Type
typeFor (ExpFor e1) r = do
  typeExp e1 r >>= rval (ExpFor e1)
typeFor (WhileFor e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (WhileFor e1 e2)
  t2 <- typeExp e2 r >>= rval (WhileFor e1 e2)
  if subtype r t2 TBool
    then return t1
    else err $ printf "test cannot be type \"%s\" in \"%s\"." (show t2) (pretty (WhileFor e1 e2))
typeFor (StepFor e1 e2 e3) r = do
  t1 <- typeExp e1 r >>= rval (StepFor e1 e2 e3)
  t2 <- typeExp e2 r >>= rval (StepFor e1 e2 e3)
  t3 <- typeExp e3 r >>= rval (StepFor e1 e2 e3)
  if subtype r t1 TInt && subtype r t2 TInt && subtype r t3 TInt
    then return TInt
    else err $ printf "for step cannot have types \"%s\" in \"%s\"." (show [t1, t2, t3]) (pretty (StepFor e1 e2 e3))
typeFor (ChainFor f1 f2) r = do
  t1 <- typeFor f1 r
  t2 <- typeFor f2 r
  if eitherSubtype r t1 t2
    then return $ max r t1 t2
    else err $ printf "for cannot have mixed types \"%s\" and \"%s\" in \"%s\"." (show t1) (show t2) (pretty (ChainFor f1 f2))