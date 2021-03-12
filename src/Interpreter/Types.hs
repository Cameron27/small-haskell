module Interpreter.Types where

import Classes (Typeable (..))
import Data.HashMap.Internal.Strict (HashMap)

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

instance Show Dv where
  show (DLoc x) = "DLoc " ++ show x
  show (DProc _) = "DProc _"
  show (DFunc _) = "DFunc _"
  show (DInt x) = "DInt " ++ show x
  show (DDouble x) = "DDouble " ++ show x
  show (DBool x) = "DBool " ++ show x
  show (DString x) = "DString " ++ show x

data Sv
  = SFile File
  | SInt Integer
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
  typeSrt (RInt _) = "int"
  typeSrt (RDouble _) = "double"
  typeSrt (RBool _) = "bool"
  typeSrt (RString _) = "string"

type File = [Rv]

type Env = HashMap Ide Dv

data Store = Store (HashMap Loc Sv) Loc deriving (Show)

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

type Procedure = Cc -> Ec

type Function = Ec -> Ec

data Ans
  = Error String
  | Stop
  | Ok Rv Ans

instance Show Ans where
  show (Error s) = "ERROR: " ++ s
  show Stop = ""
  show (Ok (RInt x) a) = show x ++ "\n" ++ show a
  show (Ok (RDouble x) a) = show x ++ "\n" ++ show a
  show (Ok (RBool True) a) = "true\n" ++ show a
  show (Ok (RBool False) a) = "false\n" ++ show a
  show (Ok (RString x) a) = x ++ "\n" ++ show a