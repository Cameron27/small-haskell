module Parser.Language where

import Data.Functor.Identity
import Parser.Types
import Text.Parsec
import Text.Parsec.Token
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
          "withbuffer"
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
          "."
        ],
      caseSensitive = True
    }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser tinyDef

boolean :: ParsecT String u Identity Bool
boolean =
  choice
    [ do
        keyword "true"
        return True,
      do
        keyword "false"
        return False
    ]

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

integer :: ParsecT String u Identity Integer
integer = Token.integer lexer

float :: ParsecT String u Identity Double
float = Token.float lexer

naturalOrFloat :: ParsecT String u Identity (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

stringLiteral :: ParsecT String u Identity String
stringLiteral = Token.stringLiteral lexer

semi :: ParsecT String u Identity String
semi = Token.semi lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Token.commaSep lexer

symbol :: String -> ParsecT String u Identity String
symbol = Token.symbol lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer
