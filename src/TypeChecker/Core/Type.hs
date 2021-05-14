module TypeChecker.Core.Type where

import Common.Functions
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv

typeType :: Type -> TEnv -> Either TypeError ()
typeType (TArray t) r = typeType t r
typeType (TRecord ts) r =
  if allDifferent (map fst ts)
    then typeTypes (map snd ts) r
    else err $ printf "all identifiers must be unique in \"%s\"" (show (TRecord ts))
typeType (TProc ts) r = typeTypes ts r
typeType (TFunc ts t) r = do
  typeTypes ts r
  typeType t r
typeType (TFile t) r = typeType t r
typeType (TRef t) r = typeType t r
typeType (TRefMaybe t) r = typeType t r
typeType (TObject i) r = do
  c <- lookupTEnv i r
  if c <: TClassAny
    then return ()
    else err $ printf "no class %s" i
typeType _ _ = return ()

typeTypes :: [Type] -> TEnv -> Either TypeError ()
typeTypes [] _ = return ()
typeTypes (t : ts) r = do
  typeType t r
  typeTypes ts r