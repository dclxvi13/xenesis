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

opSymbol = oneOf "+-<>=&*/%^&|~!"

names =
  [ "alignas"
  , "alignof"
  , "asm"
  , "auto"
  , "bool"
  , "break"
  , "case"
  , "catch"
  , "char"
  , "char16_t"
  , "char32_t"
  , "class"
  , "const"
  , "constexpr"
  , "const_cast"
  , "continue"
  , "decltype"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "dynamic_cast"
  , "else"
  , "enum"
  , "explicit"
  , "export"
  , "extern"
  , "false"
  , "float"
  , "for"
  , "friend"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "mutable"
  , "namespace"
  , "new"
  , "noexcept"
  , "nullptr"
  , "operator"
  , "private"
  , "protected"
  , "public"
  , "register"
  , "reinterpret_cast"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "static_assert"
  , "static_cast"
  , "struct"
  , "switch"
  , "template"
  , "this"
  , "thread_local"
  , "throw"
  , "true"
  , "try"
  , "typedef"
  , "typeid"
  , "typename"
  , "union"
  , "unsigned"
  , "using"
  , "virtual"
  , "void"
  , "volatile"
  , "wchar_t"
  , "while"
  ]

operators =
  [ "="
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "^"
  , "&"
  , "|"
  , "~"
  , "!"
  , "="
  , "<"
  , ">"
  , "+="
  , "-="
  , "*="
  , "/="
  , "%="
  , "^="
  , "&="
  , "|="
  , "<<"
  , ">>"
  , "<<="
  , ">>="
  , "=="
  , "!="
  , "<="
  , ">="
  , "&&"
  , "||"
  , "++"
  , "--"
  ]

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
