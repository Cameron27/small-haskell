module Parser.Core.Com where

import {-# SOURCE #-} Parser.Core.Dec
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Features.For
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

com :: Parsec String () Com
com = do
  choice
    [ -- Block: { D* C* }
      block,
      -- Assign and Procedure: E1 ... ;
      do
        e1 <- exp
        c <-
          choice
            [ -- Assign: E1 = E2 ;
              do
                op "="
                Assign e1 <$> exp,
              -- Procedure: E ( E1, ..., En ) ;
              case e1 of
                Func e2 e3 -> return $ Proc e2 e3
                _ -> fail ""
            ]
        semi
        return c,
      -- Output: output E ;
      do
        keyword "output"
        e <- exp
        semi
        return $ Output e,
      -- If: if ( E ) C1 else C2 | if ( E ) C1
      do
        keyword "if"
        e <- parens exp
        c1 <- com
        c2 <-
          option
            Skip
            ( do
                keyword "else"
                com
            )
        return $ If e c1 c2,
      -- While: while ( E ) C
      do
        keyword "while"
        e <- parens exp
        While e <$> com,
      -- Repeat: repeat C until ( E )
      do
        keyword "repeat"
        c <- com
        keyword "until"
        e <- parens exp
        return $ Repeat e c,
      -- For: for ( I = F ) C
      forCom,
      -- Trap: trap { C* D* (I1: C1)* }
      do
        keyword "trap"
        braces $ do
          c <- block'
          pairs <-
            many
              ( do
                  i <- ide
                  symbol ":"
                  cs <- many1 $ try com
                  return (i, chain cs)
              )
          let pairs' = unzip pairs
          return $ Trap (c : snd pairs') (fst pairs'),
      -- Escape: escapeto I ;
      do
        keyword "escapeto"
        i <- ide
        semi
        return $ Escape i,
      -- Return: return E ;
      do
        keyword "return"
        e <- exp
        semi
        return $ Return e,
      do
        keyword "with"
        e <- exp
        keyword "do"
        WithDo e <$> com
    ]

chain :: [Com] -> Com
chain = foldr Chain Skip

-- Block Internals: D* C*
block' :: Parsec String () Com
block' =
  do
    ds <- many $ try dec
    cs <- many $ try com
    return $ Block (chainDec ds) (chain cs)

-- Block: { D* C* }
block :: Parsec String () Com
block = braces block'
