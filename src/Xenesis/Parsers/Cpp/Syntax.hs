module Xenesis.Parsers.Cpp.Syntax where

data TranslationUnit =
  TU [Directive]
     [Declaration]
  deriving (Show, Eq)

data Id =
  Id String
  | Ref Id
  | Ptr Id
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Directive =
  Include String
  deriving (Show, Eq)

data Statement
  = VarDecl Type Id
  | ExprSimple Expression
  | SetValue Id Expression
  deriving (Show, Eq)

data Expression
  = Expr_Literal Literal
  | Expr_This
  | Expr_Id Id
  | Expr_FunctionCall Id [Id]
  | UnaryOperation UnaryOperator Expression
  | BinaryOperation BinaryOperator Expression Expression
  deriving (Show, Eq)

data UnaryOperator
  = IncrementPref
  | IncrementPost
  | DecrementPref
  | DecrementPost
  | GetRef
  deriving (Show, Eq)

data BinaryOperator
  = Add
  | Multi
  deriving (Show, Eq)

data Type
  = BoolType
  | IntType
  | CharType
  | FloatType
  | DoubleType
  | WCharType
  | VoidType
  | AutoType
  | UserType Id
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | FloatL Double
  | CharL Char
  | StringL String
  | NullPtr
  | BoolL Bool
  deriving (Show, Eq)
