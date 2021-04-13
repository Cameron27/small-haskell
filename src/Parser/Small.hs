module Parser.Small (parseSmall) where

import Common.Formatting
import Data.List
import Debug.Trace
import Parser.Language
import Parser.Types
import Text.Parsec
import Text.Printf
import Prelude hiding (exp)

pgm :: Parsec String () Pgm
pgm =
  -- program C
  do
    spaces
    keyword "program"
    Program <$> com

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
      -- If: if ( E ) C1 else C2
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
        For i f <$> com,
      -- Trap: trap { C (I1: C1)* }
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

dec :: Parsec String () Dec
dec = do
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
      -- Own: own I = E ;
      do
        keyword "own"
        i <- ide
        op "="
        e <- exp
        semi
        return $ Own i e,
      -- Array: array I [ E1 : E2 ] ;
      do
        keyword "array"
        i <- ide
        es <-
          brackets
            ( do
                e1 <- exp
                op ":"
                e2 <- exp
                return (e1, e2)
            )
        semi
        return $ uncurry (ArrayDec i) es,
      -- Record: record I( I1, ..., In ) ;
      do
        keyword "record"
        i1 <- ide
        i2 <- parens $ commaSep ide
        semi
        return $ RecordDec i1 i2,
      -- File: file I1 withbuffer I2 ;
      do
        keyword "file"
        i1 <- ide
        keyword "withbuffer"
        i2 <- ide
        semi
        return $ FileDec i1 i2,
      -- Procedure: (rec)? proc I( I1, ..., In ) { D* C* }
      do
        isRec <-
          try $
            choice
              [ do
                  keyword "proc"
                  return False,
                do
                  keyword "rec"
                  keyword "proc"
                  return True
              ]
        i1 <- ide
        i2 <- parens $ commaSep ide
        (if isRec then RecProcDec else ProcDec) i1 i2 <$> block,
      -- Function: (rec)? func I ( I1, ..., In ) { E }
      do
        isRec <-
          try $
            choice
              [ do
                  keyword "func"
                  return False,
                do
                  keyword "rec"
                  keyword "func"
                  return True
              ]
        i1 <- ide
        i2 <- parens $ commaSep ide
        (if isRec then RecFuncDec else FuncDec) i1 i2 <$> braces exp
    ]

chainDec :: [Dec] -> Dec
chainDec = foldr ChainDec SkipDec

exp :: Parsec String () Exp
exp = jumpout

-- Jumpout: jumpout I in E
jumpout :: Parsec String () Exp
jumpout =
  choice
    [ do
        keyword "jumpout"
        i <- ide
        keyword "in"
        Jumpout i <$> jumpout,
      ternaryOp
    ]

-- Ternary: E1 ? E2 : E3
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
multiplicativeOps = opChain ["*", "/", "%"] unary

-- Unary: [a-z]+ E
unary :: Parsec String () Exp
unary =
  choice
    ( arrayAccess :
      map
        unaryKey
        [ -- Continuation: cont E
          ("cont", Cont),
          -- Reference: ref E
          ("ref", RefExp)
        ]
        ++ map
          unaryOp
          [ -- Not: ! E
            ("!", Not)
          ]
    )
  where
    unaryKey :: (String, Exp -> Exp) -> Parsec String () Exp
    unaryKey (kw, e) = do
      keyword kw
      e <$> unary
    unaryOp :: (String, Exp -> Exp) -> Parsec String () Exp
    unaryOp (opr, e) = do
      op opr
      e <$> unary

-- Array Access: E1[E2]
arrayAccess :: Parsec String () Exp
arrayAccess =
  do
    e1 <- dot
    option
      e1
      ( do
          es <- many1 $ brackets exp
          return $ foldl ArrayAccess e1 es
      )

-- Dot: E1.E2
dot :: Parsec String () Exp
dot = do
  e1 <- function
  es <- many $ do
    op "."
    function
  return $ foldl Dot e1 es

-- Function: E ( E1, ..., En )
function :: Parsec String () Exp
function = do
  e1 <- atom
  option
    e1
    ( do
        calls <- many $ parens $ commaSep exp
        return $ foldl Func e1 calls
    )

atom :: Parsec String () Exp
atom =
  choice
    [ -- Read: read
      do
        keyword "read"
        return Read,
      -- Bool: true or false
      Bool <$> boolean,
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
          Right x -> case x of
            Left i -> return $ Int (if negate then - i else i)
            Right f -> return $ Double (if negate then - f else f)
          Left i -> fail $ printf "Integer %d is too large." i,
      -- String
      String
        <$> stringLiteral,
      -- Identifier
      I <$> ide,
      -- Parentheses: ( E )
      parens exp,
      -- Valof: valof { D* C* }
      do
        keyword "valof"
        Valof <$> block,
      -- Array: array[ E1 : E2 ]
      do
        keyword "array"
        es <-
          brackets
            ( do
                e1 <- exp
                op ":"
                e2 <- exp
                return (e1, e2)
            )
        return $ uncurry ArrayExp es,
      -- Record: record( I1, ..., In )
      do
        keyword "record"
        RecordExp <$> parens (commaSep ide)
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