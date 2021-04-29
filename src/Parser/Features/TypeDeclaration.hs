module Parser.Features.TypeDeclaration (typeDeclaration) where

import Common.Functions
import Parser.Helper.Language
import Text.Parsec
import TypeChecker.Core.Types

typeDeclaration :: Parsec String () Type
typeDeclaration = do
  choice
    [ do
        keyword "int"
        return TInt,
      do
        keyword "float"
        return TDouble,
      do
        keyword "bool"
        return TBool,
      do
        keyword "string"
        return TString,
      do
        keyword "array"
        keyword "of"
        TArray . TRef <$> typeDeclaration,
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
      do
        keyword "proc"
        TProc <$> parens (commaSep typeDeclaration),
      do
        keyword "func"
        ts <- parens (commaSep typeDeclaration)
        TFunc ts <$> typeDeclaration,
      do
        keyword "file"
        TFile <$> typeDeclaration,
      do
        keyword "ref"
        TRef <$> typeDeclaration
    ]