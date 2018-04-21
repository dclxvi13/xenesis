module Xenesis.Parsers.Cpp.Syntax where

data TranslationUnit = TU [Declaration]

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