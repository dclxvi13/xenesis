module Xenesis.Parsers.Cpp where

import Xenesis.Parsers.Cpp.CppLexer

import Text.Parsec

type P = Parsec [L Token] ()

parse :: String -> [L Token]
parse = lexer