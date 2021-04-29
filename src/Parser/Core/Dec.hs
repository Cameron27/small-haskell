module Parser.Core.Dec (dec, chainDec) where

import {-# SOURCE #-} Parser.Core.Com
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Features.TypeDeclaration
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

dec :: Parsec String () Dec
dec = do
  choice
    [ -- Constant: const T I = E ;
      do
        keyword "const"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Const i t e,
      -- Variable: T I = E ;
      do
        keyword "var"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Var i t e,
      -- Own: own T I = E ;
      do
        keyword "own"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Own i t e,
      -- Array: array I [ E1 : E2 ] : T ;
      do
        keyword "array"
        i <- ide
        es <-
          brackets
            ( do
                e1 <- exp
                colon
                e2 <- exp
                return (e1, e2)
            )
        colon
        t <- typeDeclaration
        semi
        return $ uncurry (ArrayDec i) es t,
      -- Record: record I( I1 : T1, ..., In : Tn ) ;
      do
        keyword "record"
        i1 <- ide
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
        semi
        return $ RecordDec i1 is ts,
      -- File: file I1 withbuffer I2 : T ;
      do
        keyword "file"
        i1 <- ide
        keyword "withbuffer"
        i2 <- ide
        colon
        t <- typeDeclaration
        semi
        return $ FileDec i1 i2 t,
      -- Procedure: (rec)? proc I( I1 : T1, ..., In : Tn ) { D* C* }
      do
        isRec <-
          try $
            choice
              [ do
                  keyword "proc"
                  return False,
                do
                  keyword "rec"
                  keyword "proc"
                  return True
              ]
        i1 <- ide
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
        (if isRec then RecProcDec else ProcDec) i1 is ts <$> block,
      -- Function: (rec)? func I ( I1 : T1, ..., In : T2 ) : T { E }
      do
        isRec <-
          try $
            choice
              [ do
                  keyword "func"
                  return False,
                do
                  keyword "rec"
                  keyword "func"
                  return True
              ]
        i1 <- ide
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
        colon
        t <- typeDeclaration
        (if isRec then RecFuncDec else FuncDec) i1 is ts t <$> braces exp
    ]

chainDec :: [Dec] -> Dec
chainDec = foldr ChainDec SkipDec