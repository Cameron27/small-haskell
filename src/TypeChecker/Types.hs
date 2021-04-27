module TypeChecker.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Printf

type Ide = String

newtype TypeError = TypeError String

instance Show TypeError where
  show (TypeError err) = err

data TEnv = TEnv (HashMap.HashMap Ide Type) Type
  deriving (Show)

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
  deriving (Eq, Ord, Show)

eq :: Type -> Type -> Bool
-- any cases
eq TArrayAny (TArray _) = True
eq (TArray _) TArrayAny = True
eq TRecordAny (TRecord _) = True
eq (TRecord _) TRecordAny = True
eq TProcAny (TProc _) = True
eq (TProc _) TProcAny = True
eq TFuncAny (TFunc _ _) = True
eq (TFunc _ _) TFuncAny = True
eq TFileAny (TFile _) = True
eq (TFile _) TFileAny = True
eq TRefAny (TRef _) = True
eq (TRef _) TRefAny = True
-- nested cases
eq (TArray t1) (TArray t2) = t1 `eq` t2
eq (TRecord ts1) (TRecord ts2) = length ts1 == length ts2 && all (\(i, t) -> isJust (find (\(i', t') -> i == i' && t `eq` t') ts2)) ts1
eq (TProc ts1) (TProc ts2) = length ts1 == length ts2 && all (uncurry eq) (zip ts1 ts2)
eq (TFunc ts1 t1) (TFunc ts2 t2) = length ts1 == length ts2 && all (uncurry eq) (zip ts1 ts2) && t1 `eq` t2
eq (TFile t1) (TFile t2) = t1 `eq` t2
eq (TRef t1) (TRef t2) = t1 `eq` t2
eq (TRefMaybe t1) (TRefMaybe t2) = t1 `eq` t2
eq t1 t2 = t1 == t2
