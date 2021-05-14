module TypeChecker.Helper.TypeModification where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import Prelude hiding (max)

-- | @ref src t@ returns the references version of `t` if it is a storable value. `src` is the expression to use in the
-- | error message.
ref :: Pretty a => a -> Type -> Either TypeError Type
ref src t =
  if isSv t
    then return $ TRef t
    else err $ printf "\"%s\" is not a storable value in \"%s\"" (show t) (pretty src)

-- | @deref t@ returns the dereferences version of `t`.
deref :: Type -> Type
deref (TRef t) = t
deref (TRefMaybe t) = t
deref t = t

-- | @rval src t@ returns the dereferences version of `t` if it is a right hand value. `src` is the expression to use in
-- | the error message.
rval :: Pretty a => a -> Type -> Either TypeError Type
rval src t =
  if isRv t'
    then return t'
    else err $ printf "\"%s\" is not a right hand value in \"%s\"" (show t') (pretty src)
  where
    t' = deref t

-- | @tryMerge src t1 t2@ returns the merged type of `t1` and `t2`. This means returning whichever type is the super
-- | type of the other and resolving difference in the level of referencing by 1 with a `TRefMaybe`. `src` is the
-- | expression to use in the error message.
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

-- | @recordEnvironment r@ returns the type environment record `r` represents.
recordEnvironment :: Type -> TEnv
recordEnvironment (TRecord ts) = uncurry newTEnvMulti (unzip $ ts)

-- | @objectEnvironment o r@ returns the type environment object `o` represents under the environment `r`.
objectEnvironment :: Type -> TEnv -> TEnv
objectEnvironment (TObject i1) r = do
  case lookupTEnv i1 r of
    (Right (TClass (Class _ c))) -> TEnv c TVoid emptyClass

-- | @arrayType a@ returns the type of a array `a`.
arrayType :: Type -> Type
arrayType (TArray t) = t

-- @isSv t@ returns `True` iff `t` is a storable value.
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

-- @isRv t@ returns `True` iff `t` is a right hand value.
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

-- @isPrintable t@ returns `True` iff `t` is a printable type.
isPrintable :: Type -> Bool
isPrintable TInt = True
isPrintable TDouble = True
isPrintable TBool = True
isPrintable TString = True
isPrintable _ = False

-- @assignable t1 t2@ returns `True` iff a value of type `t1` can be assigned to an identifier of type `t2`.
assignable :: Type -> Type -> Bool
assignable t1 (TRef t2) = t1 <: t2
assignable _ _ = False