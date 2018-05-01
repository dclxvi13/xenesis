module Xenesis.Parsers.Cpp.Expr
  ( expr
  ) where

import           Text.Parsec
import           Text.Parsec.Expr

import           Xenesis.Parsers.Cpp.Language
import           Xenesis.Parsers.Cpp.Syntax

expr, term :: Parsec String () Expression
expr = buildExpressionParser table term <?> "expression"

term =
  parens expr <|>
  (do l <- literal
      return $ Expr_Literal l)

table =
  [ [prefix "++" IncrementPref, prefix "--" DecrementPref]
  , [postfix "++" IncrementPost, postfix "--" DecrementPost]
  , [binary "+" Add AssocLeft]
  ]

-----------------------------------------------------------------------
-- Operator definitions
-----------------------------------------------------------------------
binary name op assoc =
  Infix
    (do reservedOp name
        return $ BinaryOperation op)
    assoc

prefix name op =
  Prefix
    (do reservedOp name
        return $ UnaryOperation op)

postfix name op =
  Postfix
    (do reservedOp name
        return $ UnaryOperation op)

---------------------------------------------
-- Literals
---------------------------------------------
literal =
  boolLiteral <|> intLiteral <|> floatLiteral <|> nullPtr <|>
  (do str <- stringLiteral
      return $ StringL str) <|>
  (do ch <- charLiteral
      return $ CharL ch)

boolLiteral =
  (do reserved "true"
      return $ BoolL True) <|>
  (do reserved "false"
      return $ BoolL False)

intLiteral = do
  val <- integer
  return $ IntL val

floatLiteral = do
  val <- float
  return $ FloatL val

nullPtr = do
  reserved "nullptr"
  return NullPtr
