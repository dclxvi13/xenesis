module Xenesis.Parsers.Cpp.Language where

import           Text.Parsec
import qualified Text.Parsec.Token as P

cppLang :: P.LanguageDef ()
cppLang =
  P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> char '_'
    , P.opStart = opSymbol
    , P.opLetter = opSymbol
    , P.caseSensitive = True
    , P.reservedNames = names
    , P.reservedOpNames = operators
    }

opSymbol = oneOf "+-<>=&*"

names = ["#include", "int", "void", "bool", "char", "float", "double", "wchar_t"]

operators = ["<<", ">>", "==", "="]

lexer = P.makeTokenParser cppLang

identifier = P.identifier lexer

reserved = P.reserved lexer

operator = P.operator lexer

reservedOp = P.reservedOp lexer

charLiteral = P.charLiteral lexer

stringLiteral = P.stringLiteral lexer

integer = P.integer lexer

float = P.float lexer

symbol = P.symbol lexer

lexeme = P.symbol lexer

parens = P.parens lexer

braces = P.braces lexer

angles = P.angles lexer

brackets = P.brackets lexer

semi = P.semi lexer

colon = P.colon lexer

dot = P.dot lexer

comma = P.comma lexer

semiSep = P.semiSep lexer

commaSep = P.commaSep lexer
