module Parser.Core.Com where

import Parser.Core.Types
import Text.Parsec

com :: Parsec String () Com
block :: Parsec String () Com