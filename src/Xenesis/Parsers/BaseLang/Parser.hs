module Xenesis.Parsers.BaseLang.Parser where

import           Prelude                           hiding (id)

import           Text.Parsec

import           Xenesis.Parsers.BaseLang.Language
import           Xenesis.Parsers.BaseLang.Syntax

type P = Parsec String ()

statement =
  parens $
  (do reserved "set"
      name <- id
      stat <- statement
      return $ SetVal name stat)

id = do
  s <- identifier
  return $ Id s
