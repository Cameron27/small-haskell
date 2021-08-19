{-# LANGUAGE FlexibleInstances #-}

module Interpreter.Core.Types where

import Common.Formatting
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List
import System.Exit
import Text.Printf

-- | An `Ide` is a identifier in small.
type Ide = String

-- | A `Loc` is location.
type Loc = Int

instance Pretty Loc where
  pretty l = printf "Loc(%d)" l

-- | A `Posn` is a code position.
type Posn = [Int]

-- | @w ! i@ returns a new position that is `w` with `i` appended to the end.
(!) :: Posn -> Int -> Posn
w ! i = i : w

instance Pretty Posn where
  pretty [] = ""
  pretty (i : s) = pretty s ++ "." ++ show i

-- | A `Ev` is a denotable value.
data Ev
  = EInt Int
  | EDouble Double
  | EBool Bool
  | EString String
  | ELoc Loc
  | EArray Array
  | ERecord Record
  | EFile File
  | EProc Procedure Int
  | EFunc Function Int
  | EMethod Method
  | EClass Class
  | EObject Object
  | ENull
  | ECc Cc

instance Pretty Ev where
  pretty (EInt x) = show x
  pretty (EDouble x) = printf "%f" x
  pretty (EBool True) = "true"
  pretty (EBool False) = "false"
  pretty (EString x) = show x
  pretty (ELoc x) = printf "Loc(%d)" x
  pretty (EArray (Array x y _)) = printf "ARRAY[%d:%d]" x y
  pretty (ERecord (Record x)) = printf "RECORD(%s)" (pretty x)
  pretty (EFile _) = "FILE"
  pretty (EProc _ y) = printf "PROCEDURE%d" y
  pretty (EFunc _ y) = printf "FUNCTION%d" y
  pretty (EMethod _) = printf "METHOD"
  pretty (EClass _) = printf "CLASS"
  pretty (EObject (Object x)) = printf "OBJECT(%s)" (pretty x)
  pretty ENull = "null"
  pretty (ECc _) = "CC"

instance Typeable Ev where
  typeStr (EInt _) = "int"
  typeStr (EDouble _) = "double"
  typeStr (EBool _) = "bool"
  typeStr (EString _) = "string"
  typeStr (ELoc _) = "location"
  typeStr (EArray (Array x y _)) = printf "array[%s:%s]" x y
  typeStr (ERecord (Record x)) = printf "record(%s)" (intercalate "," (HashMap.keys x))
  typeStr (EObject (Object x)) = printf "object(%s)" (intercalate "," (HashMap.keys x))
  typeStr ENull = "null"
  typeStr e = error $ printf "%s is not right hand value." (pretty e)

-- | A `EnvValue` is a value that can be in a environment.
data EnvVal = Dv Ev | Unbound

instance Pretty EnvVal where
  pretty (Dv e) = pretty e
  pretty Unbound = "Unbound"

-- | An `Env` is an environment.
data Env
  = -- @Env m1 m2 k o@ is an environment with `m1` being the mapping from identifiers to the values they represent,
    -- `m2` being the mapping from identifiers and locations to the values they represent for own declarations, `k`
    -- being the current return address and `o` being the current object represented by "this".
    Env (HashMap.HashMap Ide EnvVal) (HashMap.HashMap (Ide, Posn) EnvVal) Ec Object

instance Pretty Env where
  pretty (Env x y _ z) = printf "Env (%s) (%s) (%s)" (pretty x) (pretty y) (pretty (EObject z))

-- | A `StoreValue` is a value that can be in a store.
data StoreVal = Sv Ev | Unassigned | Unused

instance Pretty StoreVal where
  pretty (Sv e) = pretty e
  pretty Unassigned = "Unassigned"
  pretty Unused = "Unused"

-- | A `Store` is a store.
data Store
  = -- | @Store m l@ is a store with `m` being the mapping from location to the values at those locations and `l` being
    -- the next free location in the store.
    Store (HashMap.HashMap Loc StoreVal) Loc

instance Pretty Store where
  pretty (Store x y) = printf "Store (%s) %d" (pretty x) y

-- | A `Cc` is a command continuation. It takes in a `Store` and produces an `Ans`.
type Cc = Store -> Ans

-- | A `Dc` is a declaration continuation. It takes in an `Env` and a `Store` and produces an `Ans`.
type Dc = Env -> Cc

-- | A `CDc` is a class declaration continuation. It takes in an `Object` and a `Store` and produces an
-- `Ans`.
type CDc = Object -> Cc

-- | A `Ec` is a expression continuation. It takes in an `Ev` and a `Store` and produces an `Ans`.
type Ec = Ev -> Cc

-- | An `Array` is an array of locations.
data Array
  = -- | @Array i1 i2 ls@ is an array starting at `i1` and ending at `i2` with `ls` being the locations in the array.
    Array Int Int [Loc]

-- | A `Record` is a record of values.
newtype Record
  = -- | @Record m@ is a record with `m` being the mapping from identifiers to the values they represent.
    Record (HashMap.HashMap Ide EnvVal)

-- | A `File` is list of values and a location.
data File
  = -- | @File es i l@ is a file containing the values `es`, currently at position `i` and using location `l` to put the
    -- current value.
    File [Ev] Int Loc

-- | A `Procedure` is a procedure. It takes in a `Cc` a list of `Ev`s and a `Store` and evaluates the procedure then
-- runs the `Cc`.
type Procedure = Cc -> [Ev] -> Cc

-- | A `Function` is a function. It takes in  an `Ev` a list of `Ev`s and a `Store` and evaluates the function then passes
-- the resulting value into the `Ec`.
type Function = Ec -> [Ev] -> Cc

-- | A `Method` is a `Procedure` or `Function` of an `Object`. It takes in an `Ev` an `Object` and a `Store` and bounds
-- the `Object` to "this" in the `Method` then passes the resulting `Procedure` or `Function` into the `Ec`.
type Method = Ec -> Object -> Cc

-- | A `Class` is a class.
newtype Class
  = -- | @Class c@ is a class which you can provide a `Ev` to `c` and it will generate an `Object` from that class.
    Class (Ec -> Cc)

-- | A `Object` is an object.
newtype Object
  = -- | @Object m@ is an object with `m` being the mapping from identifiers to the values they represent.
    Object (HashMap.HashMap Ide EnvVal)

-- | An `Ans` is an `IO` representing what the small program does.
type Ans = IO ExitCode

instance Pretty (HashMap.HashMap Ide EnvVal) where
  pretty x = intercalate "," (zipWith (\a b -> printf "%s = %s" a (pretty b)) (HashMap.keys x) (HashMap.elems x))

instance Pretty (HashMap.HashMap Loc StoreVal) where
  pretty x = intercalate "," (zipWith (\a b -> printf "%d = %s" a (pretty b)) (HashMap.keys x) (HashMap.elems x))

instance Pretty (HashMap.HashMap (Ide, Posn) EnvVal) where
  pretty x = intercalate "," (zipWith (\(a, b) c -> printf "%s = %s" ("(" ++ a ++ "," ++ pretty b ++ ")") (pretty c)) (HashMap.keys x) (HashMap.elems x))