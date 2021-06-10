module TypeChecker.Core.Type where

import Common.Functions
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv

-- | @typeType t r@ returns the type `t` represents if `t` type checks under the environment `r`.
typeType :: Type -> TEnv -> Either TypeError Type
typeType TInt r = return TInt
typeType TDouble r = return TDouble
typeType TBool r = return TBool
typeType TString r = return TString
typeType (TArray t) r = TArray <$> typeType t r
typeType (TRecord ts) r =
  if allDifferent (map fst ts)
    then do
      ts' <- typeTypes (map snd ts) r
      return $ TRecord (zip (map fst ts) ts')
    else err $ printf "all identifiers must be unique in \"%s\"" (show (TRecord ts))
typeType (TProc ts) r = TProc <$> typeTypes ts r
typeType (TFunc ts t) r = do
  ts <- typeTypes ts r
  t <- typeType t r
  return $ TFunc ts t
typeType (TFile t) r = TFile <$> typeType t r
typeType (TRef t) r = TRef <$> typeType t r
typeType (TObjectNamed i) r = do
  c <- lookupTEnv i r
  if c <: TClassAny
    then let (TClass (Class i _)) = c in return $ TObject i
    else err $ printf "no class \"%s\"" i
typeType t _ = error $ printf "Should not be type checking \"%s\"." (show t)

-- | @typeType ts r@ returns the type `ts` represents if `ts` type checks under the environment `r`.
typeTypes :: [Type] -> TEnv -> Either TypeError [Type]
typeTypes [] _ = return []
typeTypes (t : ts) r = do
  t <- typeType t r
  ts <- typeTypes ts r
  return $ t : ts