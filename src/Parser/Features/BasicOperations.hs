module Parser.Features.BasicOperations (binaryOps, unaryOp) where

import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Text.Printf

-- | Parses a binary operation expression.
binaryOps :: Parsec String () Exp
binaryOps = orOp

-- | Parses an or operation expression.
orOp :: Parsec String () Exp
orOp =
  -- Or: E | E
  op2Chain ["|"] xorOp

-- | Parses an xor operation expression.
xorOp :: Parsec String () Exp
xorOp =
  -- XOr: E ^ E
  op2Chain ["^"] andOp

-- | Parses an and operation expression.
andOp :: Parsec String () Exp
andOp =
  -- And: E & E
  op2Chain ["&"] equalityOps

-- | Parses an equality operation expression.
equalityOps :: Parsec String () Exp
equalityOps =
  -- Equals: E == E (or similar)
  op2Chain ["==", "!="] relationalOps

-- | Parses a relational operation expression.
relationalOps :: Parsec String () Exp
relationalOps =
  -- Relation: E < E (or similar)
  op2Chain [">=", "<=", ">", "<"] additiveOps

-- | Parses an additive operation expression.
additiveOps :: Parsec String () Exp
additiveOps =
  -- Addition E + E (or similar)
  op2Chain ["+", "-"] multiplicativeOps

-- | Parses a multiplicative operation expression.
multiplicativeOps :: Parsec String () Exp
multiplicativeOps =
  -- Multiplication: E * E (or similar)
  op2Chain ["*", "/", "%"] unary

-- | Parses unary operation expression.
unaryOp :: Parsec String () Exp
unaryOp =
  do
    -- Unary: ! E | + E | - E
    op <- op1Map <$> opChoice ["!", "+", "-"]
    Op1 op <$> unary

-- | @op2Chain os p@ returns a parser that parses a chain of expressions parsed by `p` joined by the operators `os`;
op2Chain :: [String] -> Parsec String () Exp -> Parsec String () Exp
op2Chain ops lowerParser = do
  e1 <- lowerParser
  es <- many $ do op <- opChoice ops; e <- lowerParser; return (op2Map op, e)
  return $ foldl (\x (y, z) -> Op2 y x z) e1 es

-- | @op2Map o@ returns the `Opr2` represented by the operator string `o`.
op2Map :: [Char] -> Opr2
op2Map "*" = Mult
op2Map "/" = Div
op2Map "%" = Mod
op2Map "+" = Add
op2Map "-" = Sub
op2Map "<" = Less
op2Map "<=" = LessEq
op2Map ">" = Great
op2Map ">=" = GreatEq
op2Map "==" = Equal
op2Map "!=" = NEqual
op2Map "&" = And
op2Map "^" = Xor
op2Map "|" = Or
op2Map s = error $ printf "\"%s\" is not a binary operation." s

-- | @op1Map o@ returns the `Opr1` represented by the operator string `o`.
op1Map :: [Char] -> Opr1
op1Map "!" = Not
op1Map "+" = Positive
op1Map "-" = Negative
op1Map s = error $ printf "\"%s\" is not a unary operation." s