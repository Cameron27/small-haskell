module Parser.Types where

import Common.Formatting
import Data.List
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

data Pgm = Program Com [(Int, Exp)]
  deriving (Show)

data Com
  = Assign Exp Exp
  | Output Exp
  | Proc Exp [Exp]
  | If Exp Com Com
  | While Exp Com
  | Repeat Exp Com
  | For Id For Com
  | Block Dec Com
  | Trap [Com] [Id]
  | Escape Id
  | Return Exp
  | WithDo Exp Com
  | Chain Com Com
  | Skip
  deriving (Show)

instance Pretty Com where
  pretty (Assign x y) = printf "%s = %s;" (pretty x) (pretty y)
  pretty (Output x) = printf "output %s;" (pretty x)
  pretty (Proc x y) = printf "%s(%s)" (pretty x) (intercalate ", " $ map pretty y)
  pretty (If x y z) = printf "if (%s) %s else %s" (pretty x) (pretty y) (pretty z)
  pretty (While x y) = printf "while (%s) %s" (pretty x) (pretty y)
  pretty (Repeat x y) = printf "repeat %s until (%s)" (pretty y) (pretty x)
  pretty (For x y z) = printf "for (%s = %s) %s" x (pretty y) (pretty z)
  pretty (Block x y) = printf "{ %s %s }" (pretty x) (pretty y)
  pretty (Trap x y) = printf "trap { %s %s }" (pretty (head x)) tags
    where
      tags' = zip (tail x) y
      tags = foldl (\s (c, i) -> s ++ printf "%s: %s" i (pretty c)) "" tags'
  pretty (Escape x) = printf "escapeto %s;" x
  pretty (Return x) = printf "return %s;" (pretty x)
  pretty (WithDo x y) = printf "with %s do %s" (pretty x) (pretty y)
  pretty (Chain x y) = printf "%s %s" (pretty x) (pretty y)
  pretty Skip = ""
  pretty _ = "PRETTY_COM"

data Dec
  = Const Id Exp
  | Var Id Exp
  | Own Id Exp Int
  | ArrayDec Id Exp Exp
  | RecordDec Id [Id]
  | FileDec Id Id
  | ProcDec Id [Id] Com
  | RecProcDec Id [Id] Com
  | FuncDec Id [Id] Exp
  | RecFuncDec Id [Id] Exp
  | ChainDec Dec Dec
  | SkipDec
  deriving (Show)

instance Pretty Dec where
  pretty (Const x y) = printf "const %s = %s;" x (pretty y)
  pretty (Var x y) = printf "var %s = %s;" x (pretty y)
  pretty (Own x y _) = printf "own %s = %s;" x (pretty y)
  pretty (ArrayDec x y z) = printf "array %s[%s:%s];" x (pretty y) (pretty z)
  pretty (RecordDec x y) = printf "record %s(%s);" x (intercalate ", " y)
  pretty (FileDec x y) = printf "file %s withbuffer %s;" x y
  pretty (ProcDec x y z) = printf "proc %s(%s) %s" x (intercalate ", " y) (pretty z)
  pretty (RecProcDec x y z) = printf "proc %s(%s) %s" x (intercalate ", " y) (pretty z)
  pretty (FuncDec x y z) = printf "func %s(%s) { %s }" x (intercalate ", " y) (pretty z)
  pretty (RecFuncDec x y z) = printf "rec func %s(%s) { %s }" x (intercalate ", " y) (pretty z)
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
  | RefExp Exp
  | ArrayExp Exp Exp
  | RecordExp [Id]
  | Func Exp [Exp]
  | IfExp Exp Exp Exp
  | Jumpout Id Exp
  | Valof Com
  | Cont Exp
  | ArrayAccess Exp Exp
  | Dot Exp Exp
  | Not Exp
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
  pretty (RefExp x) = printf "ref %s" (pretty x)
  pretty (ArrayExp x y) = printf "array[%s:%s]" (pretty x) (pretty y)
  pretty (RecordExp x) = printf "record(%s)" (intercalate ", " x)
  pretty (Func x y) = printf "%s(%s)" (pretty x) (intercalate ", " $ map pretty y)
  pretty (IfExp x y z) = printf "%s ? %s : %s" (pretty x) (pretty y) (pretty z)
  pretty (Jumpout x y) = printf "jumpout %s in %s" x (pretty y)
  pretty (Valof x) = printf "valof %s" (pretty x)
  pretty (Cont x) = printf "cont %s" (pretty x)
  pretty (ArrayAccess x y) = printf "%s[%s]" (pretty x) (pretty y)
  pretty (Dot x y) = printf "%s.%s" (pretty x) (pretty y)
  pretty (Not x) = printf "!%s" (pretty x)
  pretty (Op x y z) = printf "%s %s %s" (pretty y) (pretty x) (pretty z)
  pretty _ = "PRETTY_EXP"

data For
  = ExpFor Exp
  | WhileFor Exp Exp
  | StepFor Exp Exp Exp
  | ChainFor For For
  deriving (Show)

instance Pretty For where
  pretty (ExpFor x) = pretty x
  pretty (WhileFor x y) = printf "%s while %s" (pretty x) (pretty y)
  pretty (StepFor x y z) = printf "%s step %s until %s" (pretty x) (pretty y) (pretty z)
  pretty (ChainFor x y) = printf "%s, %s" (pretty x) (pretty y)
  pretty _ = "PRETTY_FOR"