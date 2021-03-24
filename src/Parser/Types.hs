module Parser.Types where

import Common.Formatting
import Text.Printf

type Id = String

data Opr
  = Mult
  | Div
  | Mod
  | Add
  | Sub
  | Less
  | LessEq
  | Great
  | GreatEq
  | Equal
  | NEqual
  | And
  | Xor
  | Or
  deriving (Show)

instance Pretty Opr where
  pretty Mult = "*"
  pretty Div = "/"
  pretty Mod = "%"
  pretty Add = "+"
  pretty Sub = "-"
  pretty Less = "<"
  pretty LessEq = "<="
  pretty Great = ">"
  pretty GreatEq = ">="
  pretty Equal = "=="
  pretty NEqual = "!="
  pretty And = "&"
  pretty Xor = "^"
  pretty Or = "|"

newtype Pgm = Program Com deriving (Show)

data Com
  = Assign Exp Exp
  | Output Exp
  | Proc Exp Exp
  | If Exp Com Com
  | While Exp Com
  | Block Dec Com
  | Trap [Com] [Id]
  | Escape Id
  | Chain Com Com
  | Skip
  deriving (Show)

data Dec
  = Const Id Exp
  | Var Id Exp
  | ProcDec Id Id Com
  | FuncDec Id Id Exp
  | ChainDec Dec Dec
  | SkipDec
  deriving (Show)

data Exp
  = Int Integer
  | Double Double
  | Bool Bool
  | String String
  | Read
  | I Id
  | Func Exp Exp
  | IfExp Exp Exp Exp
  | Jumpout Id Exp
  | Op Opr Exp Exp
  deriving (Show)

instance Pretty Exp where
  pretty (Int x) = show x
  pretty (Double x) = show x
  pretty (Bool True) = "true"
  pretty (Bool False) = "false"
  pretty (String x) = show x
  pretty Read = "read"
  pretty (I x) = x
  pretty (Func x y) = printf "%s(%s)" (pretty x) (pretty y)
  pretty (IfExp x y z) = printf "%s ? %s : %s" (pretty x) (pretty y) (pretty z)
  pretty (Op x y z) = printf "%s %s %s" (pretty y) (pretty x) (pretty z)