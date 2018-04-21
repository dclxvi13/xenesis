module Xenesis.Utils where

type XType = String

data XValue
  = StateValue XState
  | NullValue
  | HardValue String

type XModuleNamePair = (XName, XName)

type XName = String

data XParameter = XParameter
  { parameterType :: XType
  , parameterName :: XModuleNamePair
  }

data XState =
  XState [(XParameter, XValue)]

data XExpression
  = SetVar XModuleNamePair
  | GetVar XModuleNamePair
  | FuncCall XModuleNamePair
  | ExternalCall XName
  deriving (Eq)

data XFunction = XFunction
  { functionName :: XModuleNamePair
  , block        :: [XExpression]
  }

data XModule = XModule
  { moduleName      :: XName
  , moduleState     :: XState
  , moduleFunctions :: [XFunction]
  }

data XComponent =
  XComponent XName
             [XModule]

data XApp = XApp
  { appName        :: XName
  , appComponents  :: [XComponent]
  , appEntryPoints :: [XModuleNamePair]
  }

data XChange
  = FuncChange { xchangeFunction    :: XModuleNamePair
               , addedExpressions   :: [XExpression]
               , removedExpressions :: [XExpression] }
  | StateChange { xchangeModule     :: XName
                , addedParameters   :: [XParameter]
                , removedParameters :: [XParameter] }

data XDiff =
  XDiff [XChange]

parameterGetFrom :: XApp -> XModuleNamePair -> [XModuleNamePair]
parameterGetFrom app name = filterByExpr app $ GetVar name

parameterSetFrom :: XApp -> XModuleNamePair -> [XModuleNamePair]
parameterSetFrom app name = filterByExpr app $ SetVar name

functionCalledFrom :: XApp -> XModuleNamePair -> [XModuleNamePair]
functionCalledFrom app name = filterByExpr app $ FuncCall name

filterByExpr :: XApp -> XExpression -> [XModuleNamePair]
filterByExpr app val =
  [ functionName function
  | XComponent _ modulesList <- appComponents app
  , module' <- modulesList
  , function <- moduleFunctions module'
  , expr <- block function
  , expr == val
  ]

