module Parser.Small (parseSmall) where

import Data.List
import Parser.Core.Com
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

-- | Parses a small program.
pgm :: Parsec String () Pgm
pgm =
  -- program C
  do
    spaces
    keyword "program"
    Program <$> block

-- | @stripComments s@ returns the small program `s` but with comments removed.
stripComments :: String -> String
stripComments = notComment
  where
    notComment :: String -> String
    notComment "" = ""
    notComment xs
      | "//" `isPrefixOf` xs = inComment $ drop 2 xs
      | "/*" `isPrefixOf` xs = inCommentBlock $ drop 2 xs
      | "\"" `isPrefixOf` xs = head xs : inStringLiteral (tail xs)
      | otherwise = head xs : notComment (tail xs)
    inComment :: String -> String
    inComment "" = ""
    inComment xs
      | "\r\n" `isPrefixOf` xs = '\r' : '\n' : notComment (drop 2 xs)
      | "\n" `isPrefixOf` xs = '\n' : notComment (drop 1 xs)
      | otherwise = inComment $ tail xs
    inCommentBlock xs
      | "*/" `isPrefixOf` xs = notComment (drop 2 xs)
      | "\r\n" `isPrefixOf` xs = '\r' : '\n' : inCommentBlock (drop 2 xs)
      | "\n" `isPrefixOf` xs = '\n' : inCommentBlock (drop 1 xs)
      | otherwise = inCommentBlock $ tail xs
    inStringLiteral :: String -> String
    inStringLiteral "" = ""
    inStringLiteral xs
      | "\"" `isPrefixOf` xs = '\"' : notComment (tail xs)
      | "\\\"" `isPrefixOf` xs = '\\' : '\"' : inStringLiteral (drop 2 xs)
      | otherwise = head xs : inStringLiteral (tail xs)

-- | @parseSmall fp s@ returns the parsed small program `s` using the filename `fp` for errors.
parseSmall :: SourceName -> String -> Either ParseError Pgm
parseSmall fp input = parse pgm fp input'
  where
    input' = stripComments input