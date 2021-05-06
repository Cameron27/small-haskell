module Parser.Core.Exp (exp, unary) where

import {-# SOURCE #-} Parser.Core.Com
import Parser.Core.TypeDeclaration
import Parser.Core.Types
import Parser.Features.BasicOperations
import Parser.Helper.Language
import Text.Parsec
import Text.Printf
import Prelude hiding (exp)

exp :: Parsec String () Exp
exp = ternaryOp

-- Ternary: E1 ? E2 : E3
ternaryOp :: Parsec String () Exp
ternaryOp = do
  e1 <- binaryOps
  option
    e1
    ( do
        op "?"
        e2 <- exp
        colon
        IfExp e1 e2 <$> exp
    )

-- Unary: [a-z]+ E
unary :: Parsec String () Exp
unary =
  choice
    ( arrayAccess :
      map
        unaryKey
        [ -- Continuation: cont E
          ("cont", Cont),
          -- Reference: ref E
          ("ref", RefExp)
        ]
        ++ map
          unaryOp
          [ -- Not: ! E
            ("!", Not),
            -- Positive: + E
            ("+", Positive),
            -- Negative: - E
            ("-", Negative)
          ]
    )
  where
    unaryKey :: (String, Exp -> Exp) -> Parsec String () Exp
    unaryKey (kw, e) = do
      keyword kw
      e <$> unary
    unaryOp :: (String, Exp -> Exp) -> Parsec String () Exp
    unaryOp (opr, e) = do
      op opr
      e <$> unary

-- Array Access: E1[E2]
arrayAccess :: Parsec String () Exp
arrayAccess =
  do
    e1 <- dot
    option
      e1
      ( do
          es <- many1 $ brackets exp
          return $ foldl ArrayAccess e1 es
      )

-- Dot: E1.E2
dot :: Parsec String () Exp
dot = do
  e1 <- function
  es <- many $ do
    op "."
    function
  return $ foldl Dot e1 es

-- Function: E ( E1, ..., En )
function :: Parsec String () Exp
function = do
  e1 <- atom
  option
    e1
    ( do
        calls <- many $ parens $ commaSep exp
        return $ foldl Func e1 calls
    )

atom :: Parsec String () Exp
atom =
  choice
    [ -- Read: read
      do
        keyword "read"
        return Read,
      -- Bool: true or false
      Bool <$> boolean,
      -- Number
      do
        x <- naturalOrFloat
        case x of
          Right x -> case x of
            Left i -> return $ Int i
            Right f -> return $ Double f
          Left i -> fail $ printf "Integer %d is too large." i,
      -- String
      String
        <$> stringLiteral,
      -- Identifier
      I <$> ide,
      -- Parentheses: ( E )
      parens exp,
      -- Valof: valof : T { D* C* }
      do
        keyword "valof"
        colon
        t <- typeDeclaration
        Valof t <$> block,
      -- Array: array[ E1 : E2 ] : T
      do
        keyword "array"
        (e1, e2) <-
          brackets
            ( do
                e1 <- exp
                colon
                e2 <- exp
                return (e1, e2)
            )
        colon
        ArrayExp e1 e2 <$> typeDeclaration,
      -- Record: record( I1 : T1, ..., In : Tn )
      do
        keyword "record"
        (is, ts) <-
          unzip
            <$> parens
              ( commaSep
                  ( do
                      i <- ide
                      colon
                      t <- typeDeclaration
                      return (i, t)
                  )
              )
        return $ RecordExp is ts
    ]