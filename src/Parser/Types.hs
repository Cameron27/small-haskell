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
  | Return Exp
  | Chain Com Com
  | Skip
  deriving (Show)

instance Pretty Com where
  pretty (Assign x y) = printf "%s = %s;" (pretty x) (pretty y)
  pretty (Output x) = printf "output %s;" (pretty x)
  pretty (Proc x y) = printf "proc %s(%s)" (pretty x) (pretty y)
  pretty (If x y z) = printf "if (%s) %s else %s" (pretty x) (pretty y) (pretty z)
  pretty (While x y) = printf "while (%s) %y" (pretty x) (pretty y)
  pretty (Block x y) = printf "{ %s %s }" (pretty x) (pretty y)
  pretty (Trap x y) = printf "trap { %s %s }" (pretty (head x)) tags
    where
      tags' = zip (tail x) y
      tags = foldl (\s (c, i) -> s ++ printf "%s: %s" i (pretty c)) "" tags'
  pretty (Escape x) = printf "escapeto %s;" x
  pretty (Return x) = printf "return %s;" (pretty x)
  pretty (Chain x y) = printf "%s %s" (pretty x) (pretty y)
  pretty Skip = ""
  pretty _ = "PRETTY_COM"

data Dec
  = Const Id Exp
  | Var Id Exp
  | ProcDec Id Id Com
  | FuncDec Id Id Exp
  | ChainDec Dec Dec
  | SkipDec
  deriving (Show)

instance Pretty Dec where
  pretty (Const x y) = printf "const %s = %s;" x (pretty y)
  pretty (Var x y) = printf "var %s = %s;" x (pretty y)
  pretty (ProcDec x y z) = printf "proc %s(%s) %s" x y (pretty z)
  pretty (FuncDec x y z) = printf "func %s(%s) { %s }" x y (pretty z)
  pretty (ChainDec x y) = printf "%s %s" (pretty x) (pretty y)
  pretty SkipDec = ""
  pretty _ = "PRETTY_DEC"

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
  | Valof Com
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
  pretty (Jumpout x y) = printf "jumpout %s in %s" x (pretty y)
  pretty (Valof x) = printf "valof %s" (pretty x)
  pretty (Op x y z) = printf "%s %s %s" (pretty y) (pretty x) (pretty z)
  pretty _ = "PRETTY_EXP"