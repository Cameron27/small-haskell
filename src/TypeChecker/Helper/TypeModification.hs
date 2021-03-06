module TypeChecker.Helper.TypeModification where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import Prelude hiding (max)

-- | @ref src t@ returns the references version of `t` if it is a storable value. `src` is the expression to use in the
-- error message.
ref :: Pretty a => a -> Type -> TypeResult Type
ref src t =
  if isSv t
    then return $ TRef t
    else err $ printf "\"%s\" is not a storable value in \"%s\"." (show t) (pretty src)

-- | @deref t@ returns the dereferences version of `t`.
deref :: Type -> Type
deref (TRef t) = t
deref (TRefMaybe t) = t
deref t = t

-- | @rval src t@ returns the dereferences version of `t` if it is a right hand value. `src` is the expression to use in
-- the error message.
rval :: Pretty a => a -> Type -> TypeResult Type
rval src t =
  if isRv t'
    then return t'
    else err $ printf "\"%s\" is not a right hand value in \"%s\"." (show t') (pretty src)
  where
    t' = deref t

-- | @tryMerge src t1 t2@ returns the merged type of `t1` and `t2`. This means returning whichever type is the super
-- type of the other and resolving difference in the level of referencing by 1 with a `TRefMaybe`. `src` is the
-- expression to use in the error message.TypeResult TypeError Type
tryMerge src r (TRef t1) (TRef t2) = case TRef <$> tryMerge src r t1 t2 of
  Left _ -> err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRef t1) (show $ TRef t2) (pretty src)
  x -> x
tryMerge src r (TRefMaybe t1) (TRefMaybe t2) =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRef t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src r (TRef t1) (TRefMaybe t2) =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRef t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src r (TRefMaybe t1) (TRef t2) =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRefMaybe t1) (show $ TRef t2) (pretty src)
tryMerge src r t1 (TRefMaybe t2) =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show t1) (show $ TRefMaybe t2) (pretty src)
tryMerge src r (TRefMaybe t1) t2 =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRefMaybe t1) (show t2) (pretty src)
tryMerge src r t1 (TRef t2) =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show t1) (show $ TRef t2) (pretty src)
tryMerge src r (TRef t1) t2 =
  if eitherSubtype r t1 t2
    then return $ TRefMaybe $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show $ TRef t1) (show t2) (pretty src)
tryMerge src r t1 t2 =
  if eitherSubtype r t1 t2
    then return $ max r t1 t2
    else err $ printf "types \"%s\" and \"%s\" are incompatible in \"%s\"." (show t1) (show t2) (pretty src)

-- | @recordEnvironment r@ returns the type environment record `r` represents.
recordEnvironment :: Type -> TEnv
recordEnvironment (TRecord ts) = uncurry newTEnvMulti (unzip ts)
recordEnvironment t = error $ printf "Tried to get the record environment of a \"%s\"." (show t)

-- | @objectEnvironment o r@ returns the type environment object `o` represents under the environment `r`.
objectEnvironment :: Type -> TEnv -> TEnv
objectEnvironment (TObject i1) r = do
  case lookupClassTEnv i1 r of
    (Right (Class _ _ c _)) -> TEnv c HashMap.empty TVoid emptyClassId (-1)
    _ -> error $ printf "There is no class with id %d." i1
objectEnvironment t _ = error $ printf "Tried to get the object environment of a \"%s\"." (show t)

-- | @arrayType a@ returns the type of a array `a`.
arrayType :: Type -> Type
arrayType (TArray t) = t
arrayType t = error $ printf "Tried to get the array type of a \"%s\"." (show t)

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
assignable :: TEnv -> Type -> Type -> Bool
assignable r t1 (TRef t2) = subtype r t1 t2
assignable _ _ _ = False