module Interpreter.Types where

import Common.Formatting
import Data.HashMap.Strict
import System.Exit
import Text.Printf

type Ide = String

type Loc = Integer

inputLoc :: Loc
inputLoc = -1

type Bv = Rv

data Dv
  = DLoc Loc
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

data Env = Env (HashMap Ide Dv) Ec

data Store = Store (HashMap Loc Sv) Loc

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

type Procedure = Cc -> [Ev] -> Cc

type Function = Ec -> [Ev] -> Cc

type Jump = Ev -> Ec -> Cc

type Ans = IO ExitCode