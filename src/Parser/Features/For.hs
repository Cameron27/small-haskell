module Parser.Features.For where

import {-# SOURCE #-} Parser.Core.Com
import Parser.Core.Exp
import Parser.Core.Types
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

-- | Parses a for command.
forCom :: Parsec String () Com
forCom =
  -- For: for ( I = F ) C
  do
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

-- | Parses a for expression.
for :: Parsec String () For
for =
  -- ForChain: F , F
  do
    fs <- commaSep1 $
      do
        -- ForExp: E
        e1 <- exp
        option
          (ExpFor e1)
          ( choice
              [ -- ForWhile: E while E
                do
                  keyword "while"
                  WhileFor e1 <$> exp,
                -- ForStep: E step E until E
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