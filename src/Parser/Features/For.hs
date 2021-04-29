module Parser.Features.For where

import {-# SOURCE #-} Parser.Core.Com
import Parser.Core.Exp
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

forCom :: Parsec String () Com
forCom = do
  keyword "for"
  (i, f) <-
    parens
      ( do
          i <- ide
          op "="
          f <- for
          return (i, f)
      )
  For i f <$> com

for :: Parsec String () For
for = do
  fs <- commaSep1 $
    do
      -- E
      e1 <- exp
      option
        (ExpFor e1)
        ( choice
            [ -- E while E
              do
                keyword "while"
                WhileFor e1 <$> exp,
              -- E step E until E
              do
                keyword "step"
                e2 <- exp
                keyword "until"
                StepFor e1 e2 <$> exp
            ]
        )
  case fs of
    [f] -> return f
    fs -> return $ foldr1 ChainFor fs