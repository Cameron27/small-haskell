module Interpreter.Types where

import Common.Formatting
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
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

instance Show Dv where
  show (DInt x) = show x
  show (DDouble x) = printf "%f" x
  show (DBool True) = "true"
  show (DBool False) = "false"
  show (DString x) = show x
  show (DLoc x) = printf "Loc(%d)" x
  show (DArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  show (DRecord (Record x)) = printf "RECORD(%s)" (intercalate "," (HashMap.keys x))
  show (DFile _) = "FILESTATE"
  show (DProc _ y) = printf "PROCEDURE%d" y
  show (DFunc _ y) = printf "FUNCTION%d" y
  show (DJump _) = "JUMP"
  show (DCc _) = "CC"

data Sv
  = SInt Integer
  | SDouble Double
  | SBool Bool
  | SString String
  | SLoc Loc
  | SArray Array
  | SRecord Record
  | SFile File

instance Show Sv where
  show (SInt x) = show x
  show (SDouble x) = printf "%f" x
  show (SBool True) = "true"
  show (SBool False) = "false"
  show (SString x) = show x
  show (SLoc x) = printf "Loc(%d)" x
  show (SArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  show (SRecord (Record x)) = printf "RECORD(%s)" (intercalate "," (HashMap.keys x))
  show (SFile x) = "FILE"

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

data Env = Env (HashMap.HashMap Ide Dv) (HashMap.HashMap Int Dv) Ec

instance Show Env where
  show (Env r w _) = printf "Env %s %s" (show r) (show w)

data Store = Store (HashMap.HashMap Loc Sv) Loc
  deriving (Show)

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