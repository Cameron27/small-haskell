module Parser.Core.Exp (exp, unary) where

import {-# SOURCE #-} Parser.Core.Com
import Parser.Core.TypeDeclaration
import Parser.Core.Types
import Parser.Features.BasicOperations
import Parser.Features.Classes
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
    ( accessOp :
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

accessOp :: Parsec String () Exp
accessOp = do
  e1 <- atom
  es <-
    many
      ( do
          choice
            [ -- Dot: E1.E2
              do
                op "."
                e2 <- atom
                return (`Dot` e2),
              -- Function: E ( E1, ..., En )
              do
                e2 <- parens $ commaSep exp
                return (`Func` e2),
              -- Array Access: E1[E2]
              do
                e2 <- brackets exp
                return (`ArrayAccess` e2)
            ]
      )
  return $ foldl (\e f -> f e) e1 es

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
        return $ RecordExp is ts,
      -- New: new I()
      newExp,
      -- This: this
      thisExp,
      -- Null: null
      nullExp
    ]