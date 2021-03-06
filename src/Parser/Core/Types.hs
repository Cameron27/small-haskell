module Parser.Core.Types where

import Common.Formatting
import Data.List
import Text.Printf
import TypeChecker.Core.Types

-- | An `Ide` is a identifier in small.
type Id = String

-- | An `Opr2` is a binary operation.
data Opr2
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

instance Pretty Opr2 where
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

-- | An `Opr1` is a unary operation.
data Opr1
  = Not
  | Positive
  | Negative
  deriving (Show)

instance Pretty Opr1 where
  pretty Not = "!"
  pretty Positive = "+"
  pretty Negative = "-"

-- | A `Pgm` is a small program.
newtype Pgm = Program Com deriving (Show)

-- | A `Com` is a small command.
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

-- | A `Dec` is a small declaration.
data Dec
  = Const Id Type Exp
  | Var Id Type Exp
  | Own Id Type Exp
  | ArrayDec Id Exp Exp Type
  | RecordDec Id [Id] [Type]
  | FileDec Id Id Type
  | ProcDec Id [Id] [Type] Com
  | RecProcDec Id [Id] [Type] Com
  | FuncDec Id [Id] [Type] Type Exp
  | RecFuncDec Id [Id] [Type] Type Exp
  | ClassDec Id (Maybe Id) SCDec
  | ChainDec Dec Dec
  | SkipDec
  deriving (Show)

instance Pretty Dec where
  pretty (Const x y z) = printf "const %s: %s = %s;" x (show y) (pretty z)
  pretty (Var x y z) = printf "var %s: %s = %s;" x (show y) (pretty z)
  pretty (Own x y z) = printf "own %s: %s = %s;" x (show y) (pretty z)
  pretty (ArrayDec w x y z) = printf "array %s[%s:%s]: %s;" w (pretty x) (pretty y) (show z)
  pretty (RecordDec x y z) = printf "record %s(%s);" x (intercalate ", " $ zipWith (\y z -> printf "%s: %s" y (show z)) y z)
  pretty (FileDec x y z) = printf "file %s withbuffer %s: %s;" x y (show z)
  pretty (ProcDec v w x y) = printf "proc %s(%s) %s" v (intercalate ", " $ zipWith (\w x -> printf "%s: %s" w (show x)) w x) (pretty y)
  pretty (RecProcDec v w x y) = printf "proc %s(%s) %s" v (intercalate ", " $ zipWith (\w x -> printf "%s: %s" w (show x)) w x) (pretty y)
  pretty (FuncDec v w x y z) = printf "func %s(%s): %s { %s }" v (intercalate ", " $ zipWith (\w x -> printf "%s: %s" w (show x)) w x) (show y) (pretty z)
  pretty (RecFuncDec v w x y z) = printf "rec func %s(%s): %s { %s }" v (intercalate ", " $ zipWith (\w x -> printf "%s: %s" w (show x)) w x) (show y) (pretty z)
  pretty (ClassDec x y z) = printf "class %s%s { %s }" x (maybe "" (printf " extends %s") y) (pretty z)
  pretty (ChainDec x y) = printf "%s %s" (pretty x) (pretty y)
  pretty SkipDec = ""

-- | A `CDec` is a small class declaration.
type CDec = Dec

-- | A `SCDec` is a small scoped class declaration.
data SCDec
  = Private CDec
  | Public CDec
  | ChainSCDec SCDec SCDec
  | SkipSCDec
  deriving (Show)

instance Pretty SCDec where
  pretty (Private x) = printf "private %s" (pretty x)
  pretty (Public x) = printf "public %s" (pretty x)
  pretty (ChainSCDec x y) = printf "%s %s" (pretty x) (pretty y)
  pretty SkipSCDec = ""

-- | A `Exp` is a small expression.
data Exp
  = Int Int
  | Double Double
  | Bool Bool
  | String String
  | Read
  | I Id
  | RefExp Exp
  | ArrayExp Exp Exp Type
  | RecordExp [Id] [Type]
  | Func Exp [Exp]
  | IfExp Exp Exp Exp
  | Valof Type Com
  | Cont Exp
  | ArrayAccess Exp Exp
  | Dot Exp Exp
  | New Ide
  | This
  | Null
  | Op2 Opr2 Exp Exp
  | Op1 Opr1 Exp
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
  pretty (ArrayExp x y z) = printf "array[%s:%s]: %s" (pretty x) (pretty y) (show z)
  pretty (RecordExp x y) = printf "record(%s)" (intercalate ", " $ zipWith (\x y -> printf "%s: %s" x (show y)) x y)
  pretty (Func x y) = printf "%s(%s)" (pretty x) (intercalate ", " $ map pretty y)
  pretty (IfExp x y z) = printf "%s ? %s : %s" (pretty x) (pretty y) (pretty z)
  pretty (Valof x y) = printf "valof: %s %s" (show x) (pretty y)
  pretty (Cont x) = printf "cont %s" (pretty x)
  pretty (ArrayAccess x y) = printf "%s[%s]" (pretty x) (pretty y)
  pretty (Dot x y) = printf "%s.%s" (pretty x) (pretty y)
  pretty (New x) = printf "new %s()" x
  pretty This = "this"
  pretty Null = "null"
  pretty (Op2 x y z) = printf "%s %s %s" (pretty y) (pretty x) (pretty z)
  pretty (Op1 x y) = printf "%s %s" (pretty y) (pretty x)

-- | A `For` is a small for expression.
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