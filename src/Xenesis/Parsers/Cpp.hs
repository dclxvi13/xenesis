module Xenesis.Parsers.Cpp where

import           Xenesis.Parsers.Cpp.CppLexer
import           Xenesis.Parsers.Cpp.Syntax

import           Text.Parsec
import           Text.Parsec.Pos

type P = Parsec [L Token] ()

parse :: String -> Either ParseError TranslationUnit
parse inp = runParser translationUnit () "" (lexer inp)

translationUnit :: P TranslationUnit
translationUnit = do
  return $ TU [] []

{----------------------------------------------------------
- Parser impl
-----------------------------------------------------------}
include :: P Include
include = includeAngles <|> includeStr

includeAngles :: P Include
includeAngles = do
  tok Punc_Hash
  --tok KW_include
  Id s <- angles ident
  return $ s

includeStr :: P Include
includeStr = do
  tok Punc_Hash
  -- tok KW_include
  s <- cppString
  return s

cppString :: P String
cppString =
  cppToken $ \t ->
    case t of
      Lit_string s -> Just s
      _ -> Nothing


literal :: P Literal
literal =
  cppToken $ \t ->
    case t of
      Lit_string s -> Just $ StringL s
      Lit_boolean b -> Just $ BoolL b
      Lit_char c -> Just $ CharL c
      Lit_integer i -> Just $ IntL i
      Lit_float f -> Just $ FloatL f
      Token_Nullptr -> Just NullPtr
      _ -> Nothing

ident :: P Id
ident =
  cppToken $ \t ->
    case t of
      IdentifierToken s -> Just $ Id s
      _                 -> Nothing

list :: P a -> P [a]
list = option [] . many1

parens, braces, brackets, angles :: P a -> P a
parens = between (tok Punc_OpenParen) (tok Punc_CloseParen)

braces = between (tok Punc_OpenCurly) (tok Punc_CloseCurly)

brackets = between (tok Punc_OpenSquare) (tok Punc_CloseSquare)

angles = between (tok Op_Less) (tok Op_Greater)

tok :: Token -> P ()
tok t =
  cppToken $ \r ->
    if r == t
      then Just ()
      else Nothing

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

cppToken :: (Token -> Maybe a) -> P a
cppToken testFun = token showT posT testT
  where
    showT (L _ s) = show s
    posT (L (l, p) _) = newPos "" l p
    testT (L _ s) = testFun s
