module Parser.Features.Classes where

import {-# SOURCE #-} Parser.Core.Com
import {-# SOURCE #-} Parser.Core.Dec
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.TypeDeclaration
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

classDec :: Parsec String () Dec
classDec = do
  keyword "class"
  i <- ide
  ds <- chainDec <$> braces (many $ try cdec)
  return $ ClassDec i ds

cdec :: Parsec String () Dec
cdec = do
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
        (is, ts) <- typedIdList
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
        keyword "proc"
        i1 <- ide
        (is, ts) <- typedIdList
        ProcDec i1 is ts <$> block,
      -- Function: (rec)? func I ( I1 : T1, ..., In : T2 ) : T { E }
      do
        keyword "func"
        i1 <- ide
        (is, ts) <- typedIdList
        colon
        t <- typeDeclaration
        FuncDec i1 is ts t <$> braces exp
    ]
  where
    -- I1 : T1, ..., In : Tn
    typedIdList =
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

newExp :: Parsec String () Exp
newExp = do
  keyword "new"
  i <- ide
  parens spaces
  return $ New i

thisExp :: Parsec String () Exp
thisExp = do
  keyword "this"
  return This

nullExp :: Parsec String () Exp
nullExp = do
  keyword "null"
  return Null