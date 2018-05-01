module Xenesis.Parsers.BaseLang.Syntax where

data CompilationUnit =
  CompilationUnit String
                  [Declaration]

data Declaration
  = Decl_Function
  | Decl_Macro
  | Decl_Module

data Statement =
  SetVal Id Statement
  | FuncCall Id [Statement]
  | GetVal Id
  | LiteralVal Literal

data Id = Id String

data Literal =
  Nil
