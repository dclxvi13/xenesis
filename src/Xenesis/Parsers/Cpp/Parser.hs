module Xenesis.Parsers.Cpp.Parser where

import           Prelude                      hiding (id)

import           Text.Parsec

import qualified Xenesis.Parsers.Cpp.Expr     as E
import           Xenesis.Parsers.Cpp.Language
import           Xenesis.Parsers.Cpp.Syntax

type P = Parsec String ()

parse :: String -> Either ParseError TranslationUnit
parse = runParser translationUnit () ""

-----------------------------------------------------
-- Declarations
-----------------------------------------------------
translationUnit :: P TranslationUnit
translationUnit = do
  incs <- many include
  decls <- many declaration
  return $ TU incs decls

declaration :: P Declaration
declaration = funcDecl

include :: P Directive
include = do
  reserved "#include"
  s <- stringLiteral <|> angles identifier
  return $ Include s

funcDecl :: P Declaration
funcDecl = do
  t <- cppType
  name <- id
  ps <- parameters
  c <- compound
  return $ Def_Function t name ps c

parameters :: P [(Type, Id)]
parameters = do
  commaSep parameter

parameter :: P (Type, Id)
parameter = do
  t <- cppType
  name <- id
  return (t, name)

---------------------------------------------
-- Statements
---------------------------------------------
oneOrCompound :: P [Statement]
oneOrCompound =
  (do s <- statement
      return [s]) <|>
  compound

compound :: P [Statement]
compound = braces statements

statements :: P [Statement]
statements = semiSep statement

statement :: P Statement
statement =
  choice
    [ varDecl
    , breakStatement
    , continueStatement
    , returnStatement
    , ifStatement
    , whileStatement
    , doWhileStatement
    , forStatement
    , gotoStatement
    , simpleExpression
    ]

varDecl :: P Statement
varDecl = do
  t <- cppType
  name <- id <|> ptr
  e <-
    optionMaybe $ do
      reservedOp "="
      expression
  return $ Stat_VarDecl t name e

simpleExpression :: P Statement
simpleExpression = do
  e <- expression
  return $ Stat_Expr e

continueStatement :: P Statement
continueStatement = do
  reserved "continue"
  return Stat_Continue

breakStatement :: P Statement
breakStatement = do
  reserved "break"
  return Stat_Break

returnStatement :: P Statement
returnStatement = do
  reserved "return"
  e <- expression
  return $ Stat_Return e

ifStatement :: P Statement
ifStatement = do
  reserved "if"
  cond <- parens expression
  s <- oneOrCompound
  return $ Stat_If cond s

whileStatement :: P Statement
whileStatement = do
  reserved "while"
  cond <- parens expression
  s <- oneOrCompound
  return $ Stat_While cond s

doWhileStatement :: P Statement
doWhileStatement = do
  reserved "do"
  s <- oneOrCompound
  reserved "while"
  e <- parens expression
  return $ Stat_DoWhile e s

forStatement :: P Statement
forStatement = do
  reserved "for"
  (s', e1', e2') <-
    parens $ do
      s <- statement
      semi
      e1 <- expression
      semi
      e2 <- expression
      return (s, e1, e2)
  ss <- oneOrCompound
  return $ Stat_For s' e1' e2' ss

-- TODO implement for ranged
forRangedStatement :: P Statement
forRangedStatement = undefined

gotoStatement :: P Statement
gotoStatement = do
  reserved "goto"
  i <- id
  return $ Stat_GoTo i

---------------------------------------------
-- Expressions
---------------------------------------------
expression :: P Expression
expression = E.expr

---------------------------------------------
-- Types
---------------------------------------------
cppType :: P Type
cppType = primitiveType <|> userType <|> autoType

userType :: P Type
userType = do
  t <- id
  return $ UserType t

autoType :: P Type
autoType = do
  reserved "auto"
  return AutoType

primitiveType :: P Type
primitiveType = choice [boolType, charType, intType, floatType, doubleType, wcharType, voidType]

shortType :: P Type
shortType = do
  reserved "short"
  optional $ reserved "int"
  return ShortType

intType :: P Type
intType = do
  reserved "int"
  return IntType

longType :: P Type
longType = do
  reserved "long"
  optional $ reserved "int"
  return LongType

longLongType :: P Type
longLongType = do
  reserved "long"
  reserved "long"
  optional $ reserved "int"
  return LongLongType

voidType :: P Type
voidType = do
  reserved "void"
  return VoidType

boolType :: P Type
boolType = do
  reserved "bool"
  return BoolType

charType :: P Type
charType = do
  reserved "char"
  return CharType

floatType :: P Type
floatType = do
  reserved "float"
  return FloatType

doubleType :: P Type
doubleType = do
  reserved "double"
  return DoubleType

wcharType :: P Type
wcharType = do
  reserved "wchar_t"
  return WCharType

---------------------------------------------
-- Identifier
---------------------------------------------
id :: P Id
id = do
  s <- identifier
  return $ Id s

compoundId :: P Id
compoundId = ptr <|> ref

ptr :: P Id
ptr = do
  char '*'
  s <- identifier
  return $ Ptr $ Id s

ref :: P Id
ref = do
  char '&'
  s <- identifier
  return $ Ref $ Id s
