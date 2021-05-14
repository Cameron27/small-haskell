module TypeChecker.Helper.TypeModification where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import Prelude hiding (max)

ref :: Pretty a => a -> Type -> Either TypeError Type
ref src t =
  if isSv t
    then return $ TRef t
    else err $ printf "\"%s\" is not a storable value in \"%s\"" (show t) (pretty src)

deref :: Type -> Type
deref (TRef t) = t
deref (TRefMaybe t) = t
deref t = t

rval :: Pretty a => a -> Type -> Either TypeError Type
rval src t =
  if isRv t'
    then return t'
    else err $ printf "\"%s\" is not a right hand value in \"%s\"" (show t') (pretty src)
  where
    t' = deref t

tryMerge :: Pretty a => a -> Type -> Type -> Either TypeError Type
tryMerge src (TRef t1) (TRef t2) = case TRef <$> tryMerge src t1 t2 of
  Left _ -> err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRef t1) (show $ TRef t2) (pretty src)
  x -> x
tryMerge src (TRefMaybe t1) (TRefMaybe t2) =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRef t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src (TRef t1) (TRefMaybe t2) =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRef t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src (TRefMaybe t1) (TRef t2) =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRefMaybe t1) (show $ TRef t2) (pretty src)
tryMerge src t1 (TRefMaybe t2) =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src (TRefMaybe t1) t2 =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRefMaybe t1) (show t2) (pretty src)
tryMerge src t1 (TRef t2) =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show t1) (show $ TRef t2) (pretty src)
tryMerge src (TRef t1) t2 =
  if t1 <::> t2
    then return $ TRefMaybe $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show $ TRef t1) (show t2) (pretty src)
tryMerge src t1 t2 =
  if t1 <::> t2
    then return $ max t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"" (show t1) (show t2) (pretty src)

recordTypes :: Type -> [(Ide, Type)]
recordTypes (TRecord ts) = ts

objectTypes :: Type -> TEnv -> TEnv
objectTypes (TObject i1) r = do
  case lookupTEnv i1 r of
    (Right (TClass (Class _ c))) -> TEnv c TVoid emptyClass

arrayType :: Type -> Type
arrayType (TArray t) = t

isSv :: Type -> Bool
isSv TInt = True
isSv TDouble = True
isSv TBool = True
isSv TString = True
isSv (TRef _) = True
isSv (TArray _) = True
isSv (TRecord _) = True
isSv (TFile _) = True
isSv (TObject _) = True
isSv TNull = True
isSv _ = False

isRv :: Type -> Bool
isRv TInt = True
isRv TDouble = True
isRv TBool = True
isRv TString = True
isRv (TRef _) = True
isRv (TArray _) = True
isRv (TRecord _) = True
isRv (TObject _) = True
isRv TNull = True
isRv _ = False

isPrintable :: Type -> Bool
isPrintable TInt = True
isPrintable TDouble = True
isPrintable TBool = True
isPrintable TString = True
isPrintable _ = False

assignable :: Type -> Type -> Bool
assignable t1 (TRef t2) = t1 <: t2
assignable _ _ = False