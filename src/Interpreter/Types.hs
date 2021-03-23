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
  | DProc Procedure
  | DFunc Function
  | DInt Integer
  | DDouble Double
  | DBool Bool
  | DString String
  | DCc Cc

instance Pretty Dv where
  pretty (DLoc x) = printf "Loc(%d)" x
  pretty (DProc x) = "PROCEDURE"
  pretty (DFunc x) = "FUNCTION"
  pretty (DCc x) = "CC"
  pretty (DInt x) = show x
  pretty (DDouble x) = printf "%f" x
  pretty (DBool True) = "true"
  pretty (DBool False) = "false"
  pretty (DString x) = x

instance Show Dv where
  show (DLoc x) = "DLoc " ++ show x
  show (DProc _) = "DProc _"
  show (DFunc _) = "DFunc _"
  show (DInt x) = "DInt " ++ show x
  show (DDouble x) = "DDouble " ++ show x
  show (DBool x) = "DBool " ++ show x
  show (DString x) = "DString " ++ show x

data Sv
  = SInt Integer
  | SDouble Double
  | SBool Bool
  | SString String
  deriving (Show)

type Ev = Dv

data Rv
  = RInt Integer
  | RDouble Double
  | RBool Bool
  | RString String
  deriving (Show)

instance Typeable Rv where
  typeStr (RInt _) = "int"
  typeStr (RDouble _) = "double"
  typeStr (RBool _) = "bool"
  typeStr (RString _) = "string"

type Env = HashMap Ide Dv

data Store = Store (HashMap Loc Sv) Loc deriving (Show)

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

type Procedure = Cc -> Ec

type Function = Ec -> Ec

type Ans = IO ExitCode