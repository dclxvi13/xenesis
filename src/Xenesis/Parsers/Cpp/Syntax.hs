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
  = Stat_VarDecl Type Id (Maybe Expression)
  | Stat_Expr Expression
  | Stat_If Expression [Statement]
  | Stat_IfElse Expression [Statement] [Statement]
  | Stat_Switch Expression [(Expression, [Statement])]
  | Stat_While Expression [Statement]
  | Stat_For Statement Expression Expression [Statement]
  | Stat_ForRanged Statement Expression [Statement]
  | Stat_DoWhile Expression [Statement]
  | Stat_Return Expression
  | Stat_Break
  | Stat_Continue
  | Stat_GoTo Id
  deriving (Show, Eq)

data Expression
  = Expr_Literal Literal
  | Expr_This
  | Expr_Id Id
  | Expr_ElemByIndex Expression Expression
  | Expr_ElemByRef Expression Expression
  | Expr_ElemByPtr Expression Expression
  | Expr_FunctionCall Expression [Expression]
  | Expr_TypeCast Expression Expression
  | Expr_Sizeof Expression
  | Expr_New Expression
  | Expr_Delete Expression
  | Expr_Throw Expression
  | Expr_UnaryOp UnaryOperator Expression
  | Expr_BinaryOp BinaryOperator Expression Expression
  | Expr_TernarOp Expression Expression Expression
  deriving (Show, Eq)

data UnaryOperator
  = Op_IncrementPref
  | Op_IncrementPost
  | Op_DecrementPref
  | Op_DecrementPost
  | Op_GetRef
  | Op_Indirection
  | Op_Plus
  | Op_Minus
  | Op_Not
  | Op_BitNot
  deriving (Show, Eq)

data BinaryOperator
  = Op_Add
  | Op_Subtract
  | Op_Multi
  | Op_Division
  | Op_Remaining
  | Op_Visibility
  | Op_AccessByRef
  | Op_AccessByPtr
  | Op_ShiftLeft
  | Op_ShiftRight
  | Op_Less
  | Op_LessOrEq
  | Op_Greater
  | Op_GreaterOrEq
  | Op_Eq
  | Op_NotEq
  | Op_BitAnd
  | Op_BitOr
  | Op_BitXor
  | Op_And
  | Op_Or
  | Op_Assign
  | Op_AssignAdd
  | Op_AssignSub
  | Op_AssignMul
  | Op_AssignDiv
  | Op_AssignRem
  | Op_AssignShiftLeft
  | Op_AssignShiftRight
  | Op_AssignBitAnd
  | Op_AssignBitXor
  | Op_AssignBitOr
  deriving (Show, Eq)

data Type
  = BoolType
  | ShortType
  | IntType
  | LongType
  | LongLongType
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
