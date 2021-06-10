module Parser.Small (parseSmall) where

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
    whiteSpace
    keyword "program"
    Program <$> block

-- | @parseSmall fp s@ returns the parsed small program `s` using the filename `fp` for errors.
parseSmall :: SourceName -> String -> Either ParseError Pgm
parseSmall = parse pgm