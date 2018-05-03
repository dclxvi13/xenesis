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
  choice
    [ literal
    , this
    , ident
    , funcCall
    , arrayElemByIndex
    , typeCastOperation
    , sizeofOperation
    , newOperation
    , deleteOperation
    , throwExpr
    , parens expr
    ]

table =
  [ [binary "::" Op_Visibility AssocLeft]
  , [ postfix "++" Op_IncrementPost
    , postfix "--" Op_DecrementPost
    , binary "." Op_AccessByRef AssocLeft
    , binary "->" Op_AccessByPtr AssocLeft
    ]
  , [ prefix "++" Op_IncrementPref
    , prefix "--" Op_DecrementPref
    , prefix "+" Op_Plus
    , prefix "-" Op_Minus
    , prefix "!" Op_Not
    , prefix "~" Op_BitNot
    , prefix "*" Op_Indirection
    , prefix "&" Op_GetRef
    ]
  , [] --TODO add operations of getting member ptr by ref and by ptr (.* and ->*)
  , [binary "*" Op_Multi AssocLeft, binary "/" Op_Division AssocLeft, binary "%" Op_Remaining AssocLeft]
  , [binary "+" Op_Add AssocLeft, binary "-" Op_Subtract AssocLeft]
  , [binary "<<" Op_ShiftLeft AssocLeft, binary ">>" Op_ShiftRight AssocLeft]
  , [ binary "<" Op_Less AssocLeft
    , binary "<=" Op_LessOrEq AssocLeft
    , binary ">" Op_Greater AssocLeft
    , binary ">=" Op_GreaterOrEq AssocLeft
    ]
  , [binary "==" Op_Eq AssocLeft, binary "!=" Op_NotEq AssocLeft]
  , [binary "&" Op_BitAnd AssocLeft]
  , [binary "^" Op_BitXor AssocLeft]
  , [binary "|" Op_BitOr AssocLeft]
  , [binary "&&" Op_And AssocLeft]
  , [binary "||" Op_Or AssocLeft]
  , [ binary "=" Op_Assign AssocRight
    , binary "+=" Op_AssignAdd AssocRight
    , binary "-=" Op_AssignSub AssocRight
    , binary "*=" Op_AssignMul AssocRight
    , binary "/=" Op_AssignDiv AssocRight
    , binary "%=" Op_AssignRem AssocRight
    , binary "<<=" Op_AssignShiftLeft AssocRight
    , binary ">>=" Op_AssignShiftRight AssocRight
    , binary "&=" Op_AssignBitAnd AssocRight
    , binary "^=" Op_AssignBitXor AssocRight
    , binary "|=" Op_AssignBitOr AssocRight
    ]
  ]

-----------------------------------------------------------------------
-- Operator definitions
-----------------------------------------------------------------------
binary name op assoc =
  Infix
    (do reservedOp name
        return $ Expr_BinaryOp op)
    assoc

prefix name op =
  Prefix
    (do reservedOp name
        return $ Expr_UnaryOp op)

postfix name op =
  Postfix
    (do reservedOp name
        return $ Expr_UnaryOp op)

----------------------------------------------------------------------
-- Terms
----------------------------------------------------------------------
arrayElemByIndex = do
  name <- expr
  e <- brackets expr
  return $ Expr_ElemByIndex name e

funcCall = do
  name <- expr
  ps <- parens $ commaSep expr
  return $ Expr_FunctionCall name ps

typeCastOperation = do
  t <- parens expr
  e <- expr
  return $ Expr_TypeCast t e

sizeofOperation = do
  reserved "sizeof"
  e <- expr
  return $ Expr_Sizeof e

newOperation = do
  reserved "new" <|> reserved "new[]"
  e <- expr
  return $ Expr_New e

deleteOperation = do
  reserved "delete" <|> reserved "delete[]"
  e <- expr
  return $ Expr_Delete e

ternarOperation = do
  cond <- expr
  reservedOp "?"
  true <- expr
  reservedOp ":"
  false <- expr
  return $ Expr_TernarOp cond true false

throwExpr = do
  reserved "throw"
  e <- expr
  return $ Expr_Throw e

ident = do
  s <- identifier
  return $ Expr_Id $ Id s

this = do
  reserved "this"
  return Expr_This

---------------------------------------------
-- Literals
---------------------------------------------
literal = do
  l <-
    boolLiteral <|> intLiteral <|> floatLiteral <|> nullPtr <|>
    (do str <- stringLiteral
        return $ StringL str) <|>
    (do ch <- charLiteral
        return $ CharL ch)
  return $ Expr_Literal l

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
