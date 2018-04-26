module Xenesis.Parsers.Cpp.Syntax where

--type Include = String

data TranslationUnit =
  TU [IncludeDirective]
     [Declaration]
  deriving (Show)

newtype Id =
  Id String
  deriving (Show)

data Declaration
  = Decl_Block
  | Def_Function Type Id [(Type, Id)] [Statement]
  | Decl_Template
  | ExplicitInst
  | ExplicitSpec
  | LinkageSpec
  | Def_Namespace
  | Decl_Empty
  | Decl_Attribute
  deriving (Show)

data IncludeDirective =
  Include String
  deriving (Show)

data Expression
  = Expr_Literal
  | Expr_This

data Statement
  = VarDecl Type Id
  deriving (Show)

data Type
  = IntType
  | VoidType
  | UserType Id
  | PtrType Type
  deriving (Show)

data Literal
  = IntL Integer
  | FloatL Float
  | CharL Char
  | StringL String
  | NullPtr
  | BoolL Bool
