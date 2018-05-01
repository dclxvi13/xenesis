module Xenesis.Parsers.Cpp.Syntax where

--type Include = String

data TranslationUnit =
  TU [IncludeDirective]
     [Declaration]
  deriving (Show)

data Id =
  Id String
  | Ref Id
  | Ptr Id
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
  deriving (Show, Eq)

data Expression
  = Expr_Literal Literal
  | Expr_This
  | Expr_Id Id
  | Expr_FunctionCall Id [Id]
  | UnaryOperation UnaryOperator Expression
  | BinaryOperation BinaryOperator Expression Expression

data UnaryOperator
  = IncrementPref
  | IncrementPost
  | DecrementPref
  | DecrementPost
  | GetRef

data BinaryOperator
  = Add
  | Multi

data Statement
  = VarDecl Type Id
  deriving (Show)

data Type
  = BoolType
  | IntType
  | CharType
  | FloatType
  | DoubleType
  | WCharType
  | VoidType
  | UserType Id
  deriving (Show)

data Literal
  = IntL Integer
  | FloatL Float
  | CharL Char
  | StringL String
  | NullPtr
  | BoolL Bool
