module Parser.Core.Dec (dec, chainDec) where

import {-# SOURCE #-} Parser.Core.Com
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.TypeDeclaration
import Parser.Core.Types
import Parser.Features.Classes
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

-- | Parses a declaration.
dec :: Parsec String () Dec
dec = do
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
      -- Own: own I : T = E ;
      do
        keyword "own"
        i <- ide
        colon
        t <- typeDeclaration
        op "="
        e <- exp
        semi
        return $ Own i t e,
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
      -- Procedure: (rec)? proc I ( I : T , ... , I : T ) { D ... D C ... C }
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
        (is, ts) <- typedIdList
        (if isRec then RecProcDec else ProcDec) i1 is ts <$> block,
      -- Function: (rec)? func I ( I : T , ... , I : T ) : T { E }
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
        (is, ts) <- typedIdList
        colon
        t <- typeDeclaration
        (if isRec then RecFuncDec else FuncDec) i1 is ts t <$> braces exp,
      -- Class: class I { D ... D }
      classDec
    ]
  where
    -- I : T , ... , I : T
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

-- | @chainDec ds@ returns a declaration that joins the declarations `ds` using `ChainDec`.
chainDec :: [Dec] -> Dec
chainDec = foldr ChainDec SkipDec