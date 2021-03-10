module SmallParser where

import Data.List
import ParserHelper
import Text.Parsec
import Prelude hiding (exp)

type Ide = String

data Opr
  = Mult
  | Div
  | Mod
  | Add
  | Sub
  | Less
  | LessEq
  | Great
  | GreatEq
  | Equal
  | NEqual
  | And
  | Xor
  | Or
  deriving (Show)

newtype Pgm = Program Com deriving (Show)

data Com
  = Assign Exp Exp
  | Output Exp
  | Proc Exp Exp
  | If Exp Com Com
  | While Exp Com
  | Block Dec Com
  | Chain Com Com
  | Skip
  deriving (Show)

data Dec
  = Const Ide Exp
  | Var Ide Exp
  | ProcDec Ide Ide Com
  | FuncDec Ide Ide Exp
  | ChainDec Dec Dec
  | SkipDec
  deriving (Show)

data Exp
  = Int Integer
  | Float Double
  | Bool Bool
  | String String
  | Read
  | I Ide
  | Func Exp Exp
  | IfExp Exp Exp Exp
  | Op Opr Exp Exp
  deriving (Show)

pgm :: Parsec String () Pgm
pgm =
  -- program C
  do
    spaces
    keyword "program"
    Program <$> com

com :: Parsec String () Com
com = do
  c <-
    choice
      [ -- Block: { D C }
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
                -- Procedure: E1 ( E2 ) ;
                case e1 of
                  Func e2 e3 -> return $ Proc e2 e3
              ]
          semi
          return c,
        -- Output: output E ;
        do
          keyword "output"
          e <- exp
          semi
          return $ Output e,
        -- If: if ( E ) { C1 } else { C2 }
        do
          keyword "if"
          e <- parens exp
          c1 <- block
          keyword "else"
          If e c1 <$> block,
        -- While: while ( E ) { C }
        do
          keyword "while"
          e <- parens exp
          While e <$> block
      ]
  -- Chain: C1 C2
  option c (Chain c <$> com)

block :: Parsec String () Com
block =
  braces
    ( choice
        [ do
            d <- dec
            Block d <$> option Skip com,
          Block SkipDec <$> com
        ]
    )

dec :: Parsec String () Dec
dec = do
  d <-
    choice
      [ -- Constant: const I = E ;
        do
          keyword "const"
          i <- ide
          op "="
          e <- exp
          semi
          return $ Const i e,
        -- Variable: var I = E ;
        do
          keyword "var"
          i <- ide
          op "="
          e <- exp
          semi
          return $ Var i e,
        -- Procedure: proc I1( I2 ) { C }
        do
          keyword "proc"
          i1 <- ide
          i2 <- parens ide
          ProcDec i1 i2 <$> block,
        -- Function: func I1 ( I2 ) { C }
        do
          keyword "func"
          i1 <- ide
          i2 <- parens ide
          FuncDec i1 i2 <$> braces exp
      ]
  -- Chain: D1 D2
  option d (ChainDec d <$> dec)

exp :: Parsec String () Exp
exp = ternaryOp

ternaryOp :: Parsec String () Exp
ternaryOp = do
  e1 <- orOp
  option
    e1
    ( do
        op "?"
        e2 <- exp
        op ":"
        IfExp e1 e2 <$> exp
    )

-- Or: E1 | E2
orOp :: Parsec String () Exp
orOp = opChain ["|"] xorOp

-- XOr: E1 ^ E2
xorOp :: Parsec String () Exp
xorOp = opChain ["^"] andOp

-- And: E1 & E2
andOp :: Parsec String () Exp
andOp = opChain ["&"] equalityOps

-- Equals: E1 == E2 (or similar)
equalityOps :: Parsec String () Exp
equalityOps = opChain ["==", "!="] relationalOps

-- Relation: E1 < E2 (or similar)
relationalOps :: Parsec String () Exp
relationalOps = opChain [">=", "<=", ">", "<"] additiveOps

-- Addition E1 + E2 (or similar)
additiveOps :: Parsec String () Exp
additiveOps = opChain ["+", "-"] multiplicativeOps

-- Multiplication: E1 * E2 (or similar)
multiplicativeOps :: Parsec String () Exp
multiplicativeOps = opChain ["*", "/", "%"] function

-- Function: E1 ( E2 )
function :: Parsec String () Exp
function = do
  e1 <- atom
  option
    e1
    ( do
        calls <- many $ parens exp
        return $ foldl Func e1 calls
    )

atom :: Parsec String () Exp
atom =
  choice
    [ -- Read: read
      do
        keyword "read"
        return Read,
      -- True: true
      do
        keyword "true"
        return $ Bool True,
      -- False: false
      do
        keyword "false"
        return $ Bool False,
      -- Number
      do
        negate <-
          option
            False
            ( choice
                [ do op "+"; return False,
                  do op "-"; return True
                ]
            )
        x <- naturalOrFloat
        case x of
          Left i -> return $ Int (if negate then - i else i)
          Right f -> return $ Float (if negate then - f else f),
      -- String
      String
        <$> stringLiteral,
      -- Identifier
      I <$> ide,
      -- Parentheses: ( E )
      parens exp
    ]

opChain :: [String] -> Parsec String () Exp -> Parsec String () Exp
opChain ops lowerParser = do
  e1 <- lowerParser
  es <- many $ do op <- opChoice ops; e <- lowerParser; return (opMap op, e)
  return $ foldl (\x (y, z) -> Op y x z) e1 es

opMap :: [Char] -> Opr
opMap "*" = Mult
opMap "/" = Div
opMap "%" = Mod
opMap "+" = Add
opMap "-" = Sub
opMap "<" = Less
opMap "<=" = LessEq
opMap ">" = Great
opMap ">=" = GreatEq
opMap "==" = Equal
opMap "!=" = NEqual
opMap "&" = And
opMap "^" = Xor
opMap "|" = Or

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

parseSmall :: SourceName -> String -> Either ParseError Pgm
parseSmall fp input = parse pgm fp input'
  where
    input' = stripComments input