module Interpreter.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.List
import System.Exit
import Text.Printf

type Ide = String

type Loc = Integer

type Bv = Rv

data Dv
  = DLoc Loc
  | DArray Array
  | DRecord Record
  | DProc Procedure Int
  | DFunc Function Int
  | DJump Jump
  | DInt Integer
  | DDouble Double
  | DBool Bool
  | DString String
  | DCc Cc

instance Pretty Dv where
  pretty (DLoc x) = printf "Loc(%d)" x
  pretty (DArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  pretty (DRecord (Record x)) = printf "RECORD(%s)" (intercalate "," (HashMap.keys x))
  pretty (DProc x i) = "PROCEDURE" ++ show i
  pretty (DFunc x i) = "FUNCTION" ++ show i
  pretty (DJump x) = "JUMP"
  pretty (DCc x) = "CC"
  pretty (DInt x) = show x
  pretty (DDouble x) = printf "%f" x
  pretty (DBool True) = "true"
  pretty (DBool False) = "false"
  pretty (DString x) = x

data Sv
  = SInt Integer
  | SDouble Double
  | SBool Bool
  | SString String
  | SLoc Loc

type Ev = Dv

data Rv
  = RInt Integer
  | RDouble Double
  | RBool Bool
  | RString String
  | RLoc Loc

instance Typeable Rv where
  typeStr (RInt _) = "int"
  typeStr (RDouble _) = "double"
  typeStr (RBool _) = "bool"
  typeStr (RString _) = "string"
  typeStr (RLoc _) = "location"

data Env = Env (HashMap.HashMap Ide Dv) Ec

data Store = Store (HashMap.HashMap Loc Sv) Loc

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

data Array = Array Integer Integer [Loc]

newtype Record = Record (HashMap.HashMap Ide Dv)

type Procedure = Cc -> [Ev] -> Cc

type Function = Ec -> [Ev] -> Cc

type Jump = Ev -> Ec -> Cc

type Ans = IO ExitCode