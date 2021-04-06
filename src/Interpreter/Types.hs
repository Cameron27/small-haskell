module Interpreter.Types where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.List
import System.Exit
import Text.Printf

type Ide = String

type Loc = Integer

data Dv
  = DInt Integer
  | DDouble Double
  | DBool Bool
  | DString String
  | DLoc Loc
  | DArray Array
  | DRecord Record
  | DFile File
  | DProc Procedure Int
  | DFunc Function Int
  | DJump Jump
  | DCc Cc

instance Pretty Dv where
  pretty (DInt x) = show x
  pretty (DDouble x) = printf "%f" x
  pretty (DBool True) = "true"
  pretty (DBool False) = "false"
  pretty (DString x) = show x
  pretty (DLoc x) = printf "Loc(%d)" x
  pretty (DArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  pretty (DRecord (Record x)) = printf "RECORD(%s)" (intercalate "," (HashMap.keys x))
  pretty (DFile _) = "FILESTATE"
  pretty (DProc _ y) = printf "PROCEDURE%d" y
  pretty (DFunc _ y) = printf "FUNCTION%d" y
  pretty (DJump _) = "JUMP"
  pretty (DCc _) = "CC"

data Sv
  = SInt Integer
  | SDouble Double
  | SBool Bool
  | SString String
  | SLoc Loc
  | SArray Array
  | SRecord Record
  | SFile File

instance Pretty Sv where
  pretty (SInt x) = show x
  pretty (SDouble x) = printf "%f" x
  pretty (SBool True) = "true"
  pretty (SBool False) = "false"
  pretty (SString x) = show x
  pretty (SLoc x) = printf "Loc(%d)" x
  pretty (SArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  pretty (SRecord (Record x)) = printf "RECORD(%s)" (intercalate "," (HashMap.keys x))
  pretty (SFile x) = "FILE"

type Ev = Dv

data Rv
  = RInt Integer
  | RDouble Double
  | RBool Bool
  | RString String
  | RLoc Loc
  | RArray Array
  | RRecord Record

instance Typeable Rv where
  typeStr (RInt _) = "int"
  typeStr (RDouble _) = "double"
  typeStr (RBool _) = "bool"
  typeStr (RString _) = "string"
  typeStr (RLoc _) = "location"
  typeStr (RArray (Array x y _)) = printf "array[%s:%s]" x y
  typeStr (RRecord (Record x)) = printf "record(%s)" (intercalate "," (HashMap.keys x))

data Env = Env (HashMap.HashMap Ide Dv) Ec

data Store = Store (HashMap.HashMap Loc Sv) Loc

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

data Array = Array Integer Integer [Loc]

newtype Record = Record (HashMap.HashMap Ide Dv)

data File = File [Rv] Integer Loc

type Procedure = Cc -> [Ev] -> Cc

type Function = Ec -> [Ev] -> Cc

type Jump = Ev -> Ec -> Cc

type Ans = IO ExitCode