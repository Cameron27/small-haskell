module Parser.Core.TypeDeclaration (typeDeclaration) where

import Parser.Helper.Language
import Text.Parsec
import TypeChecker.Core.Types

-- | Parses a type declaration.
typeDeclaration :: Parsec String () Type
typeDeclaration = do
  choice
    [ -- Int: int
      do
        keyword "int"
        return TInt,
      -- Float: float
      do
        keyword "float"
        return TDouble,
      -- Bool: bool
      do
        keyword "bool"
        return TBool,
      -- String: string
      do
        keyword "string"
        return TString,
      -- Array: array of T
      do
        keyword "array"
        keyword "of"
        TArray . TRef <$> typeDeclaration,
      -- Record: record ( I : T , ... , I : T )
      do
        keyword "record"
        ts <-
          parens
            ( commaSep
                ( do
                    i <- ide
                    colon
                    t <- TRef <$> typeDeclaration
                    return (i, t)
                )
            )
        return $ TRecord ts,
      -- Procedure: proc ( T , ... , T )
      do
        keyword "proc"
        TProc <$> parens (commaSep typeDeclaration),
      -- Function: func ( T , ... , T ) : T
      do
        keyword "func"
        ts <- parens (commaSep typeDeclaration)
        TFunc ts <$> typeDeclaration,
      -- File: file T
      do
        keyword "file"
        TFile <$> typeDeclaration,
      -- Reference: ref T
      do
        keyword "ref"
        TRef <$> typeDeclaration,
      -- Object: I
      do
        TObjectNamed <$> ide
    ]