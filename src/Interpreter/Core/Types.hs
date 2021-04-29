module Interpreter.Core.Types where

import Common.Formatting
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List
import System.Exit
import Text.Printf

type Ide = String

type Loc = Int

data Posn = Posn Int | PosnChain Posn Int

(!) :: Posn -> Int -> Posn
p ! i = PosnChain p i

instance Show Posn where
  show (Posn i) = show i
  show (PosnChain p i) = show p ++ "." ++ show i

instance Eq Posn where
  Posn i1 == Posn i2 = i1 == i2
  PosnChain p1 i1 == PosnChain p2 i2 = i1 == i2 && p1 == p2
  _ == _ = False

instance Hashable Posn where
  hashWithSalt n (Posn i) = hashWithSalt n i
  hashWithSalt n (PosnChain p i) = hashWithSalt n p `xor` hashWithSalt n i

data Dv
  = DInt Int
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
  = SInt Int
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
  = RInt Int
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

data Env = Env (HashMap.HashMap Ide Dv) (HashMap.HashMap (Ide, Posn) Dv) Ec

data Store = Store (HashMap.HashMap Loc Sv) Loc

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

data Array = Array Int Int [Loc]

newtype Record = Record (HashMap.HashMap Ide Dv)

data File = File [Rv] Int Loc

type Procedure = Cc -> [Ev] -> Cc

type Function = Ec -> [Ev] -> Cc

type Jump = Ev -> Ec -> Cc

type Ans = IO ExitCode