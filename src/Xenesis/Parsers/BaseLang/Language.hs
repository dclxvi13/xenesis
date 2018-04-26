module Xenesis.Parsers.BaseLang.Language where

import           Text.Parsec
import qualified Text.Parsec.Token as P

baseLang :: P.LanguageDef ()
baseLang =
  P.LanguageDef
    { P.commentStart = ""
    , P.commentEnd = ""
    , P.commentLine = ";;"
    , P.nestedComments = False
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> char '_'
    , P.opStart = opSymbol
    , P.opLetter = opSymbol
    , P.caseSensitive = True
    , P.reservedNames = names
    , P.reservedOpNames = operators
    }

opSymbol = oneOf ""

names = ["set", "import", "module", "defun", "defmacro"]

operators = []

lexer = P.makeTokenParser baseLang

identifier = P.identifier lexer

reserved = P.reserved lexer
charLiteral = P.charLiteral lexer

stringLiteral = P.stringLiteral lexer

integer = P.integer lexer

float = P.float lexer

symbol = P.symbol lexer

lexeme = P.symbol lexer

parens = P.parens lexer
