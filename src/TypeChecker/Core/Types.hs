module TypeChecker.Core.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Text.Printf

-- | An `Ide` is a identifier in small.
type Ide = String

-- | An `ObjectId` is an id indicating the type of object.
type ClassId = Int

-- | Id of the empty object.
emptyClassId :: Int
emptyClassId = -1

-- | A `TypeError` is an error produced during the type checking of small.
newtype TypeError
  = -- | @TypeError s@ is a type error with error message `s`.
    TypeError String

instance Show TypeError where
  show (TypeError err) = err

-- | A `TypeMap` is a mapping from identifiers to types.
type TypeMap = HashMap.HashMap Ide Type

-- | A `TEnv` is a type environment.
data TEnv
  = -- | @TEnv m1 m2 t o i@ is a type environment with `m1` being the mapping from identifiers to the types they
    -- represent, `m2` being the mapping from unique objects ids to the classes they represent, `t` being the current type
    -- expected by the return address, `o` being the id of the object this represents and `i` being the next
    -- available unique id.
    TEnv TypeMap (HashMap.HashMap ClassId Class) Type ClassId Int
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
  = -- | @Class o1 o2 m1 m2@ is a class with unique class id `o1`, `o2` being the ide of the parent class id, `m1` being
    -- the mapping from public identifiers to the types they represent and `m2` being the mapping from private
    -- identifiers to the types they represent.
    Class ClassId ClassId TypeMap TypeMap
  deriving (Eq, Ord, Show)

-- | @t1 <: t2@ returns `True` iff `t1` is a subtype of `t2`.
subtype :: TEnv -> Type -> Type -> Bool
-- any cases
subtype r (TArray _) TArrayAny = True
subtype r (TRecord _) TRecordAny = True
subtype r (TProc _) TProcAny = True
subtype r (TFunc _ _) TFuncAny = True
subtype r (TFile _) TFileAny = True
subtype r (TRef _) TRefAny = True
subtype r (TClass _) TClassAny = True
subtype r (TObject _) TObjectAny = True
subtype r TNull TObjectAny = True
subtype r TNull (TObject _) = True
subtype r (TUnion ts1) (TUnion ts2) = all (\t1 -> any (subtype r t1) ts2) ts1
subtype r t (TUnion ts) = any (subtype r t) ts
-- nested cases
subtype r (TArray t1) (TArray t2) = subtype r t1 t2
subtype r (TRecord ts1) (TRecord ts2) = all (\(i2, t2) -> isJust (find (\(i1, t1) -> i1 == i2 && subtype r t1 t2) ts1)) ts2
subtype r (TProc ts1) (TProc ts2) = length ts1 == length ts2 && all (uncurry (subtype r)) (zip ts2 ts1)
subtype r (TFunc ts1 t1) (TFunc ts2 t2) = length ts1 == length ts2 && all (uncurry (subtype r)) (zip ts2 ts1) && subtype r t1 t2
subtype r (TFile t1) (TFile t2) = subtype r t1 t2
subtype r (TRef t1) (TRef t2) = subtype r t1 t2
subtype r (TRefMaybe t1) (TRefMaybe t2) = subtype r t1 t2
subtype r (TObject o1) (TObject o2)
  | o1 == o2 = True
  | o2 == emptyClassId = True
  | o1 == emptyClassId = False
  | otherwise =
    let (Just (Class o11 o12 _ _)) = HashMap.lookup o1 c
     in o12 == o2 || subtype r (TObject o12) (TObject o2)
  where
    (TEnv _ c _ _ _) = r
subtype r t1 t2 = t1 == t2

-- | @t1 <::> t2@ returns `True` iff `t1` is a subtype of `t2` or `t2` is a subtype of `t1`.
eitherSubtype :: TEnv -> Type -> Type -> Bool
eitherSubtype r t1 t2 = subtype r t1 t2 || subtype r t2 t1

-- | @max t1 t2@ returns `t2` if `t1` is a subtype of `t2` and returns `t1` if `t2` is a subtype of `t1`.
max :: TEnv -> Type -> Type -> Type
max r t1 t2
  | subtype r t1 t2 = t2
  | subtype r t2 t1 = t1
  | otherwise = error $ printf "neither \"%s\" or \"%s\" are a subtype of the other." (show t1) (show t2)