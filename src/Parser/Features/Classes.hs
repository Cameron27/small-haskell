module Parser.Features.Classes where

import {-# SOURCE #-} Parser.Core.Com
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.TypeDeclaration
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

-- | Parses a class declaration.
classDec :: Parsec String () Dec
classDec = do
  keyword "class"
  i1 <- ide
  i2 <-
    optionMaybe
      ( do
          keyword "extends"
          ide
      )
  scds <- chainSCDec <$> braces (many $ try scdec)
  return $ ClassDec i1 i2 scds

-- | Parses a scoped class declaration.
scdec :: Parsec String () SCDec
scdec =
  choice
    [ do
        keyword "public"
        Public <$> cdec,
      do
        keyword "private"
        Private <$> cdec,
      Private <$> cdec
    ]

-- | @chainSCDec scds@ returns a scoped class declaration that joins the declarations `scds` using `ChainSCDec`.
chainSCDec :: [SCDec] -> SCDec
chainSCDec = foldr ChainSCDec SkipSCDec

-- | Parses a class declaration.
cdec :: Parsec String () CDec
cdec = do
  choice
    [ -- Constant: const I : T = E ;
      do
        keyword "const"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Const i t e,
      -- Variable: I : T = E ;
      do
        keyword "var"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Var i t e,
      -- Array: array I [ E : E ] : T ;
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
      -- Record: record I ( I : T , ... , I : T ) ;
      do
        keyword "record"
        i1 <- ide
        (is, ts) <- typedIdList
        semi
        return $ RecordDec i1 is ts,
      -- File: file I withbuffer I : T ;
      do
        keyword "file"
        i1 <- ide
        keyword "withbuffer"
        i2 <- ide
        colon
        t <- typeDeclaration
        semi
        return $ FileDec i1 i2 t,
      -- Procedure: (rec)? proc I( I : T , ... , I : T ) { D ... D C ... C }
      do
        keyword "proc"
        i1 <- ide
        (is, ts) <- typedIdList
        ProcDec i1 is ts <$> block,
      -- Function: (rec)? func I ( I : T , ... , I : T ) : T { E }
      do
        keyword "func"
        i1 <- ide
        (is, ts) <- typedIdList
        colon
        t <- typeDeclaration
        FuncDec i1 is ts t <$> braces exp
    ]
  where
    -- I : T, ..., I : T
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

-- | Parses a new expression.
newExp :: Parsec String () Exp
newExp =
  -- New: new I ( )
  do
    keyword "new"
    i <- ide
    parens spaces
    return $ New i

-- Parses a this expression.
thisExp :: Parsec String () Exp
thisExp =
  -- This: this
  do
    keyword "this"
    return This

-- Parses a null expression.
nullExp :: Parsec String () Exp
nullExp =
  -- Null: null
  do
    keyword "null"
    return Null