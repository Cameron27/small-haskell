module TypeChecker.Core.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Text.Printf

-- | An `Ide` is a identifier in small.
type Ide = String

-- | A `TypeError` is an error produced during the type checking of small.
newtype TypeError
  = -- | @TypeError s@ is a type error with error message `s`.
    TypeError String

instance Show TypeError where
  show (TypeError err) = err

-- | A `TEnv` is a type environment.
data TEnv
  = -- | @TEnv m1 m2 t c i@ is a type environment with `m1` being the mapping from identifiers to the types they
    -- represent, `m2` being the mapping from unique ids to the classes they represent, `t` being the current type
    -- expected by the return address, `c` being the current class represented by "this" and `i` being the next
    -- available unique id.
    TEnv (HashMap.HashMap Ide Type) (HashMap.HashMap Int Class) Type Class Int
  deriving (Show)

-- | A `Type` is a type in small.
data Type
  = TInt
  | TDouble
  | TBool
  | TString
  | TArray Type
  | TArrayAny
  | TRecord [(Ide, Type)]
  | TRecordAny
  | TRecordInvalid
  | TProc [Type]
  | TProcAny
  | TFunc [Type] Type
  | TFuncAny
  | TFile Type
  | TFileAny
  | TRef Type
  | TRefAny
  | TRefMaybe Type
  | TEscape
  | TVoid
  | TClass Class
  | TClassAny
  | TObjectNamed Ide
  | TObject Int
  | TObjectAny
  | TNull
  | TUnion [Type]
  deriving (Eq, Ord, Show)

instance Pretty Type where
  pretty x = show x

-- | A `Class` represents a class type in small.
data Class
  = -- | @Class i m@ is a class with unique id `i` and with `m` being the mapping from identifiers to the types they
    -- represent.
    Class Int (HashMap.HashMap Ide Type)
  deriving (Eq, Ord, Show)

-- | @t1 <: t2@ returns `True` iff `t1` is a subtype of `t2`.
(<:) :: Type -> Type -> Bool
-- any cases
(TArray _) <: TArrayAny = True
(TRecord _) <: TRecordAny = True
(TProc _) <: TProcAny = True
(TFunc _ _) <: TFuncAny = True
(TFile _) <: TFileAny = True
(TRef _) <: TRefAny = True
(TClass _) <: TClassAny = True
(TObject _) <: TObjectAny = True
TNull <: TObjectAny = True
TNull <: (TObject _) = True
t <: TUnion ts = any (t <:) ts
-- nested cases
(TArray t1) <: (TArray t2) = t1 <: t2
(TRecord ts1) <: (TRecord ts2) = (length ts1 >= length ts2) && all (\(i2, t2) -> isJust (find (\(i1, t1) -> i1 == i2 && t1 <: t2) ts1)) ts2
(TProc ts1) <: (TProc ts2) = length ts1 == length ts2 && all (uncurry (<:)) (zip ts2 ts1)
(TFunc ts1 t1) <: (TFunc ts2 t2) = length ts1 == length ts2 && all (uncurry (<:)) (zip ts2 ts1) && t1 <: t2
(TFile t1) <: (TFile t2) = t1 <: t2
(TRef t1) <: (TRef t2) = t1 <: t2
(TRefMaybe t1) <: (TRefMaybe t2) = t1 <: t2
t1 <: t2 = t1 == t2

-- | @t1 <::> t2@ returns `True` iff `t1` is a subtype of `t2` or `t2` is a subtype of `t1`.
(<::>) :: Type -> Type -> Bool
t1 <::> t2 = t1 <: t2 || t2 <: t1

-- | @max t1 t2@ returns `t2` if `t1` is a subtype of `t2` and returns `t1` if `t2` is a subtype of `t1`.
max :: Type -> Type -> Type
max t1 t2
  | t1 <: t2 = t2
  | t2 <: t1 = t1
  | otherwise = error $ printf "neither \"%s\" or \"%s\" are a subtype of the other." (show t1) (show t2)