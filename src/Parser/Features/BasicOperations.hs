module Parser.Features.BasicOperations (binaryOps) where

import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec

-- | Parses a binary operation expression.
binaryOps :: Parsec String () Exp
binaryOps = orOp

-- | Parses an or operation expression.
orOp :: Parsec String () Exp
orOp =
  -- Or: E | E
  opChain ["|"] xorOp

-- | Parses an xor operation expression.
xorOp :: Parsec String () Exp
xorOp =
  -- XOr: E ^ E
  opChain ["^"] andOp

-- | Parses an and operation expression.
andOp :: Parsec String () Exp
andOp =
  -- And: E & E
  opChain ["&"] equalityOps

-- | Parses an equality operation expression.
equalityOps :: Parsec String () Exp
equalityOps =
  -- Equals: E == E (or similar)
  opChain ["==", "!="] relationalOps

-- | Parses a relational operation expression.
relationalOps :: Parsec String () Exp
relationalOps =
  -- Relation: E < E (or similar)
  opChain [">=", "<=", ">", "<"] additiveOps

-- | Parses an additive operation expression.
additiveOps :: Parsec String () Exp
additiveOps =
  -- Addition E + E (or similar)
  opChain ["+", "-"] multiplicativeOps

-- | Parses a multiplicative operation expression.
multiplicativeOps :: Parsec String () Exp
multiplicativeOps =
  -- Multiplication: E * E (or similar)
  opChain ["*", "/", "%"] unary

-- | @opChain os p@ returns a parser that parses a chain of expressions parsed by `p` joined by the operators `os`;
opChain :: [String] -> Parsec String () Exp -> Parsec String () Exp
opChain ops lowerParser = do
  e1 <- lowerParser
  es <- many $ do op <- opChoice ops; e <- lowerParser; return (opMap op, e)
  return $ foldl (\x (y, z) -> Op y x z) e1 es

-- | @opMap o@ returns the `Opr` represented by the operator string `o`.
opMap :: [Char] -> Opr
opMap "*" = Mult
opMap "/" = Div
opMap "%" = Mod
opMap "+" = Add
opMap "-" = Sub
opMap "<" = Less
opMap "<=" = LessEq
opMap ">" = Great
opMap ">=" = GreatEq
opMap "==" = Equal
opMap "!=" = NEqual
opMap "&" = And
opMap "^" = Xor
opMap "|" = Or