module Xenesis.Parsers.Cpp.Parser where

import           Text.Parsec

import           Xenesis.Parsers.Cpp.Language
import           Xenesis.Parsers.Cpp.Syntax

type P = Parsec String ()

include :: P IncludeDirective
include = do
  reserved "#include"
  s <- stringLiteral <|> angles identifier
  endOfLine
  return $ Include s

userType :: P Type
userType = do
  t <- identifier
  return $ UserType t

primitiveType :: P Type
primitiveType = do
  undefined

intType :: P Type
intType = do
  reserved "int"
  return IntType

voidType :: P Type
voidType = do
  reserved "void"
  return VoidType

ident :: P Id
ident = do
  s <- identifier
  return $ Id s
