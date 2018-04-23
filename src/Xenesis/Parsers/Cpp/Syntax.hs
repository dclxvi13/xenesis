module Xenesis.Parsers.Cpp.Syntax where

type Include = String

data TranslationUnit =
  TU [Include]
     [Declaration]

newtype Ident =
  Ident String

data Declaration
  = Decl_Block
  | Def_Function
  | Decl_Template
  | ExplicitInst
  | ExplicitSpec
  | LinkageSpec
  | Def_Namespace
  | Decl_Empty
  | Decl_Attribute

data Expression
  = Expr_Literal
  | Expr_This

data Literal
  = IntL Integer
  | FloatL Float
  | CharL Char
  | StringL String
  | NullPtr
  | BoolL Bool
