module Parser.Helper.Language where

import Data.Char
import Data.Functor.Identity
import Numeric
import Parser.Core.Types
import Text.Parsec
import Text.Parsec.Token hiding (whiteSpace)
import qualified Text.Parsec.Token as Token

-- | The language definition for small.
tinyDef :: LanguageDef st
tinyDef =
  LanguageDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "//",
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
          "string",
          "class",
          "new",
          "this",
          "null",
          "public",
          "private"
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

-- | The lexer for small.
lexer :: GenTokenParser String u Identity
lexer = makeTokenParser tinyDef

-- | `boolean` parses a bool. Returns the bool.
boolean :: ParsecT String u Identity Bool
boolean = do { keyword "true"; return True } <|> do keyword "false"; return False

-- | `ide` parses a legal identifier. Returns the identifier string.
ide :: ParsecT String u Identity String
ide = Token.identifier lexer

-- | @keyword s@ parses the string `s`.
keyword :: String -> ParsecT String u Identity ()
keyword = Token.reserved lexer

-- | @op o@ parses the string `o`.
op :: String -> ParsecT String u Identity ()
op = Token.reservedOp lexer

opChoice :: [String] -> ParsecT String u Identity String
opChoice ops = choice $ fmap opReturn ops
  where
    opReturn s = do
      op s
      return s

-- | @parens p@ parses `p` enclosed in parentheses ('(' and ')'), returning the value of `p`.
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens lexer

-- | @braces p@ parses `p` enclosed in braces ('{' and '}'), returning the value of `p`.
braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Token.braces lexer

-- | @brackets p@ parses `p` enclosed in brackets ('[' and ']'), returning the value of `p`.
brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Token.brackets lexer

-- | `intOrFloat` parses either an int or a float. Returns either an integer larger than 64 bits or the number.
intOrFloat :: ParsecT String u Identity (Either Integer (Either Int Double))
intOrFloat = do
  n <- Token.naturalOrFloat lexer
  case n of
    -- Integer that is too large
    Left i | i > toInteger (minBound :: Int) && i < toInteger (maxBound :: Int) -> return $ Right $ Left $ fromInteger i
    -- Integer in bounds
    Left i | otherwise -> return $ Left i
    -- Float
    Right i -> return $ Right $ Right i

-- | `stringLiteral` parses a literal string. Returns the literal string value.
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

-- | `semi` parses the character ';' and skips any trailing white space.
semi :: ParsecT String u Identity ()
semi = do Token.semi lexer; return ()

-- | `colon` parses the character ':' and skips any trailing white space.
colon :: ParsecT String u Identity ()
colon = do Token.colon lexer; return ()

-- | @commaSep p@ parses zero or more occurrences of `p` separated by ','. Returns a list of values returned by `p`.
commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Token.commaSep lexer

-- | @commaSep1 p@ parses one or more occurrences of `p` separated by ','. Returns a list of values returned by `p`.
commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 = Token.commaSep1 lexer

-- | @symbol s@ parses string `s` and skips trailing white space.
symbol :: String -> ParsecT String u Identity String
symbol = Token.symbol lexer

-- | `whitespace` parses any white space.
whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer
