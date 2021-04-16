module TypeChecker.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List
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
  | TRecord [(Ide, Type)]
  | TProc [Type]
  | TFunc [Type] Type
  | TFile Type
  | TRef Type
  | TEscape
  | TJump
  | TSet (Set.Set Type)
  | TVoid
  | TAnyRecord
  | TAny
  deriving (Eq, Ord)

instance Show Type where
  show TInt = "int"
  show TDouble = "float"
  show TBool = "bool"
  show TString = "string"
  show (TArray t) = printf "array %s" (show t)
  show (TRecord ts) = printf "record(%s)" (intercalate ", " $ map (\(i, t) -> printf "%s: %s" i (show t)) ts)
  show (TProc ts) = printf "proc(%s)" (intercalate ", " $ map show ts)
  show (TFunc ts t) = printf "func(%s) %s" (intercalate ", " $ map show ts) (show t)
  show (TFile t) = printf "file %s" (show t)
  show (TRef t) = printf "ref %s" (show t)
  show TEscape = "escape"
  show TJump = "jump"
  show (TSet ts) = printf "set(%s)" (intercalate ", " $ map show (Set.toList ts))
  show TVoid = "void"
  show TAnyRecord = "record"
  show TAny = "any"

leq :: Type -> Type -> Bool
-- Any
_ `leq` TAny = True
TRecord _ `leq` TAnyRecord = True
-- Jump
TJump `leq` TVoid = False
TJump `leq` _ = True
-- Array
TArray a `leq` TArray b = a `leq` b
-- Record
TRecord a `leq` TRecord b = length a == length b && all (\((ai, at), (bi, bt)) -> ai == bi && at `leq` bt) (zip a b)
-- Procedure
TProc a `leq` TProc b = length a == length b && all (uncurry leq) (zip a b)
TFunc a1 a2 `leq` TFunc b1 b2 = length a1 == length b1 && all (uncurry leq) (zip a1 b1) && a2 `leq` b2
-- Files
TFile a `leq` TFile b = a `leq` b
-- References
TRef a `leq` TRef b = a `leq` b
-- Sets
TSet a `leq` b = all (`leq` b) a
a `leq` TSet b = any (a `leq`) b
-- Default
a `leq` b = a == b
