module Xenesis.Parsers.Cpp.Parser where

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
  endOfLine
  return $ Include s

funcDecl :: P Declaration
funcDecl = do
  t <- cppType
  name <- ident
  ps <- parameters
  c <- compound
  return $ Def_Function t name ps c

parameters :: P [(Type, Id)]
parameters = do
  commaSep parameter

parameter :: P (Type, Id)
parameter = do
  t <- cppType
  name <- ident
  return (t, name)

---------------------------------------------
-- Statements
---------------------------------------------

compound :: P [Statement]
compound = braces statements

statements :: P [Statement]
statements = many statement

statement :: P Statement
statement = varDecl

varDecl :: P Statement
varDecl = do
  t <- cppType
  name <- ident
  return $ VarDecl t name

---------------------------------------------
-- Types
---------------------------------------------

--pointerType :: P Type
--pointerType = do
cppType :: P Type
cppType = primitiveType <|> userType

userType :: P Type
userType = do
  t <- ident
  return $ UserType t

primitiveType :: P Type
primitiveType = do
  intType <|> voidType

intType :: P Type
intType = do
  reserved "int"
  return IntType

voidType :: P Type
voidType = do
  reserved "void"
  return VoidType

---------------------------------------------
-- Identificator
---------------------------------------------

ident :: P Id
ident = do
  s <- identifier
  return $ Id s
