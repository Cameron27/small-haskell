module Parser.Helper.Language where

import Data.Char
import Data.Functor.Identity
import Numeric
import Parser.Core.Types
import Text.Parsec
import Text.Parsec.Token hiding (whiteSpace)
import qualified Text.Parsec.Token as Token

tinyDef :: LanguageDef st
tinyDef =
  LanguageDef
    { commentStart = "",
      commentEnd = "",
      commentLine = "",
      nestedComments = False,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = parserZero,
      opLetter = parserZero,
      reservedNames =
        [ "program",
          "output",
          "if",
          "else",
          "while",
          "const",
          "var",
          "proc",
          "func",
          "read",
          "true",
          "false",
          "trap",
          "escapeto",
          "jumpout",
          "in",
          "return",
          "valof",
          "rec",
          "ref",
          "cont",
          "array",
          "record",
          "with",
          "do",
          "file",
          "withbuffer",
          "repeat",
          "until",
          "for",
          "step",
          "int",
          "float",
          "bool",
          "string"
        ],
      reservedOpNames =
        [ "*",
          "/",
          "%",
          "+",
          "-",
          "<",
          "<=",
          ">",
          ">=",
          "==",
          "!=",
          "&",
          "^",
          "|",
          "=",
          "?",
          ":",
          ".",
          "!"
        ],
      caseSensitive = True
    }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser tinyDef

boolean :: ParsecT String u Identity Bool
boolean = do { keyword "true"; return True } <|> do keyword "false"; return False

ide :: ParsecT String u Identity String
ide = Token.identifier lexer

keyword :: String -> ParsecT String u Identity ()
keyword = Token.reserved lexer

op :: String -> ParsecT String u Identity ()
op = Token.reservedOp lexer

opChoice :: [String] -> ParsecT String u Identity String
opChoice ops = choice $ fmap opReturn ops
  where
    opReturn s = do
      op s
      return s

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Token.braces lexer

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Token.brackets lexer

float :: ParsecT String u Identity Double
float = Token.float lexer

naturalOrFloat :: ParsecT String u Identity (Either Integer (Either Int Double))
naturalOrFloat = do
  n <- Token.naturalOrFloat lexer
  case n of
    -- Integer that is too large
    Left i | i > toInteger (minBound :: Int) && i < toInteger (maxBound :: Int) -> return $ Right $ Left $ fromInteger i
    -- Integer in bounds
    Left i | otherwise -> return $ Left i
    -- Float
    Right i -> return $ Right $ Right i

stringLiteral :: ParsecT String u Identity String
stringLiteral =
  do
    str <-
      between
        (char '"')
        (char '"' <?> "end of string")
        (many stringChar)
    whiteSpace
    return str
    <?> "literal string"
  where
    -- Any char besides \ " \r or \n
    stringChar =
      noneOf "\"\\\r\n"
        <|>
        -- Escape code
        do
          _ <- char '\\'
          charEsc <|> asciiCode
            <?> "escape code"
        <?> "string character"
    -- Any of the character f n r t \ or " for an escape code
    charEsc =
      choice
        ( zipWith
            (\c code -> do _ <- char c; return code)
            "fnrt\\\""
            "\f\n\r\t\\\""
        )
    -- An ascii code of the form xFF
    asciiCode =
      do
        oneOf "xX"
        a <- hexDigit
        b <- hexDigit
        case readHex [a, b] of
          [(i, "")] -> return $ chr i

semi :: ParsecT String u Identity String
semi = Token.semi lexer

colon :: ParsecT String u Identity String
colon = Token.colon lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Token.commaSep lexer

commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 = Token.commaSep1 lexer

symbol :: String -> ParsecT String u Identity String
symbol = Token.symbol lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer
