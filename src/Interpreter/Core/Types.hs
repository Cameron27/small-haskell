{-# LANGUAGE FlexibleInstances #-}

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

instance Pretty Posn where
  pretty (Posn i) = show i
  pretty (PosnChain p i) = pretty p ++ "." ++ show i

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
  | DMethod Method
  | DClass Class
  | DObject Object
  | DNull
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
  pretty (DRecord (Record x)) = printf "RECORD(%s)" (pretty x)
  pretty (DFile _) = "FILESTATE"
  pretty (DProc _ y) = printf "PROCEDURE%d" y
  pretty (DFunc _ y) = printf "FUNCTION%d" y
  pretty (DMethod _) = printf "METHOD"
  pretty (DClass _) = printf "CLASS"
  pretty (DObject (Object x)) = printf "OBJECT(%s)" (pretty x)
  pretty DNull = "null"
  pretty (DJump _) = "JUMP"
  pretty (DCc _) = "CC"

data Sv
  = SInt Int
  | SDouble Double
  | SBool Bool
  | SString String
  | SLoc Loc
  | SArray Array
  | SRecord Record
  | SFile File
  | SObject Object
  | SNull

instance Pretty Sv where
  pretty (SInt x) = show x
  pretty (SDouble x) = printf "%f" x
  pretty (SBool True) = "true"
  pretty (SBool False) = "false"
  pretty (SString x) = show x
  pretty (SLoc x) = printf "Loc(%d)" x
  pretty (SArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  pretty (SRecord (Record x)) = printf "RECORD(%s)" (pretty x)
  pretty (SFile x) = "FILE"
  pretty (SObject (Object x)) = printf "OBJECT(%s)" (pretty x)
  pretty SNull = "null"

type Ev = Dv

data Rv
  = RInt Int
  | RDouble Double
  | RBool Bool
  | RString String
  | RLoc Loc
  | RArray Array
  | RRecord Record
  | RObject Object
  | RNull

instance Typeable Rv where
  typeStr (RInt _) = "int"
  typeStr (RDouble _) = "double"
  typeStr (RBool _) = "bool"
  typeStr (RString _) = "string"
  typeStr (RLoc _) = "location"
  typeStr (RArray (Array x y _)) = printf "array[%s:%s]" x y
  typeStr (RRecord (Record x)) = printf "record(%s)" (intercalate "," (HashMap.keys x))
  typeStr (RObject (Object x)) = printf "object(%s)" (intercalate "," (HashMap.keys x))
  typeStr RNull = "null"

data Env = Env (HashMap.HashMap Ide Dv) (HashMap.HashMap (Ide, Posn) Dv) Ec Object

instance Pretty Env where
  pretty (Env x y _ z) = printf "Env (%s) (%s) (%s)" (pretty x) (pretty y) (pretty (DObject z))

data Store = Store (HashMap.HashMap Loc Sv) Loc

instance Pretty Store where
  pretty (Store x y) = printf "Store (%s) %d" (pretty x) y

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

type CDc = Class -> Cc

data Array = Array Int Int [Loc]

newtype Record = Record (HashMap.HashMap Ide Dv)

data File = File [Rv] Int Loc

type Procedure = Cc -> [Ev] -> Cc

type Function = Ec -> [Ev] -> Cc

type Method = Ec -> Object -> Cc

newtype Class = Class (Ec -> Cc)

newtype Object = Object (HashMap.HashMap Ide Dv)

type Jump = Ev -> Ec -> Cc

type Ans = IO ExitCode

instance Pretty (HashMap.HashMap Ide Dv) where
  pretty x = intercalate "," (zipWith (\a b -> printf "%s = %s" a (pretty b)) (HashMap.keys x) (HashMap.elems x))

instance Pretty (HashMap.HashMap Loc Sv) where
  pretty x = intercalate "," (zipWith (\a b -> printf "%d = %s" a (pretty b)) (HashMap.keys x) (HashMap.elems x))

instance Pretty (HashMap.HashMap (Ide, Posn) Dv) where
  pretty x = intercalate "," (zipWith (\(a, b) c -> printf "%s = %s" ("(" ++ a ++ "," ++ pretty b ++ ")") (pretty c)) (HashMap.keys x) (HashMap.elems x))