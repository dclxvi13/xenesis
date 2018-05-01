module Xenesis.Parsers.Cpp.Expr where

import           Text.Parsec
import           Text.Parsec.Expr

import           Xenesis.Parsers.Cpp.Language
import           Xenesis.Parsers.Cpp.Parser
import           Xenesis.Parsers.Cpp.Syntax

expr = buildExpressionParser table term <?> "expression"

term =
  parens expr <|>
  (do l <- literal
      return $ Expr_Literal l)

table =
  [[prefix "++" IncrementPref, prefix "--" DecrementPref]
  ,[postfix "++" IncrementPost, postfix "--" DecrementPost]
  ,[binary "+" Add AssocLeft]]

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
