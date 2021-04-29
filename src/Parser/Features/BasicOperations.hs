module Parser.Features.BasicOperations (binaryOps) where

import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec

binaryOps :: Parsec String () Exp
binaryOps = orOp

-- Or: E1 | E2
orOp :: Parsec String () Exp
orOp = opChain ["|"] xorOp

-- XOr: E1 ^ E2
xorOp :: Parsec String () Exp
xorOp = opChain ["^"] andOp

-- And: E1 & E2
andOp :: Parsec String () Exp
andOp = opChain ["&"] equalityOps

-- Equals: E1 == E2 (or similar)
equalityOps :: Parsec String () Exp
equalityOps = opChain ["==", "!="] relationalOps

-- Relation: E1 < E2 (or similar)
relationalOps :: Parsec String () Exp
relationalOps = opChain [">=", "<=", ">", "<"] additiveOps

-- Addition E1 + E2 (or similar)
additiveOps :: Parsec String () Exp
additiveOps = opChain ["+", "-"] multiplicativeOps

-- Multiplication: E1 * E2 (or similar)
multiplicativeOps :: Parsec String () Exp
multiplicativeOps = opChain ["*", "/", "%"] unary

opChain :: [String] -> Parsec String () Exp -> Parsec String () Exp
opChain ops lowerParser = do
  e1 <- lowerParser
  es <- many $ do op <- opChoice ops; e <- lowerParser; return (opMap op, e)
  return $ foldl (\x (y, z) -> Op y x z) e1 es

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