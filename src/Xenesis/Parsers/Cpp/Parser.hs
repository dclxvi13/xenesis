module Xenesis.Parsers.Cpp.Parser where

import           Prelude                      hiding (id)

import           Text.Parsec

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

include :: P IncludeDirective
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
compound :: P [Statement]
compound = braces statements

statements :: P [Statement]
statements = semiSep statement

statement :: P Statement
statement = choice [varDeclSimple]

varDeclSimple :: P Statement
varDeclSimple = do
  t <- cppType
  name <- id <|> ptr
  return $ VarDecl t name

---------------------------------------------
-- Expressions
---------------------------------------------

---------------------------------------------
-- Types
---------------------------------------------
cppType :: P Type
cppType = primitiveType <|> userType

userType :: P Type
userType = do
  t <- id
  return $ UserType t

primitiveType :: P Type
primitiveType = choice [boolType, charType, intType, floatType, doubleType, wcharType, voidType]

intType :: P Type
intType = do
  reserved "int"
  return IntType

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
-- Literals
---------------------------------------------

literal = undefined

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
