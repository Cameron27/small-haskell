module TypeChecker.Core.Type where

import Common.Functions
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control

typeType :: Type -> Either TypeError ()
typeType (TArray t) = typeType t
typeType (TRecord ts) =
  if allDifferent (map fst ts)
    then typeTypes (map snd ts)
    else err $ printf "all identifiers must be unique in \"%s\"" (show (TRecord ts))
typeType (TProc ts) = typeTypes ts
typeType (TFunc ts t) = do
  typeTypes ts
  typeType t
typeType (TFile t) = typeType t
typeType (TRef t) = typeType t
typeType (TRefMaybe t) = typeType t
typeType _ = return ()

typeTypes :: [Type] -> Either TypeError ()
typeTypes [] = return ()
typeTypes (t : ts) = do
  typeType t
  typeTypes ts