module Parser.Core.Dec where

import Parser.Core.Types
import Text.Parsec

dec :: Parsec String () Dec
chainDec :: [Dec] -> Dec