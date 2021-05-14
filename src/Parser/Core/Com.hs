module Parser.Core.Com where

import {-# SOURCE #-} Parser.Core.Dec
import {-# SOURCE #-} Parser.Core.Exp
import Parser.Core.Types
import Parser.Features.For
import Parser.Helper.Language
import Text.Parsec
import Prelude hiding (exp)

-- | Parses a command.
com :: Parsec String () Com
com = do
  choice
    [ -- Block: { D ... D C ... C }
      block,
      -- Assign and Procedure: E ... ;
      do
        e1 <- exp
        c <-
          choice
            [ -- Assign: E = E ;
              do
                op "="
                Assign e1 <$> exp,
              -- Procedure: E ( E , ... , E ) ;
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
      -- If: if ( E ) C else C | if ( E ) C
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
      -- Trap: trap { C ... C D ... D I : C ... I : C }
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
      -- With: with E do C
      do
        keyword "with"
        e <- exp
        keyword "do"
        WithDo e <$> com
    ]

-- | @chain cs@ returns a command that joins the commands `cs` using `Chain`.
chain :: [Com] -> Com
chain = foldr Chain Skip

-- | Parses the insides of a block.
block' :: Parsec String () Com
block' =
  -- Block Internals: D ... D C ... C
  do
    ds <- many $ try dec
    cs <- many $ try com
    return $ Block (chainDec ds) (chain cs)

-- | Parses a block.
block :: Parsec String () Com
-- Block: { D ... D C ... C }
block = braces block'
