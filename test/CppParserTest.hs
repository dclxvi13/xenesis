module CppParserTest where

import           Data.Either                (isLeft, isRight)

import           Test.HUnit

import           Text.Parsec                (runParser)

import           Xenesis.Parsers.Cpp.Parser
import           Xenesis.Parsers.Cpp.Syntax

run = runTestTT tests

tests =
  TestList
    [ "Parse include in angles" ~: anglesIncludeTest
    , "Parse include with quotes" ~: quotesIncludeTest
    , "Parse include with comment at the end" ~: includeWithCommentTest
    -- Expressions
    , "Parse simple add expression" ~: addExpressionTest
    , "Parse complex increment expression" ~: complexIncrementTest
   -- , "Parse id with 3 minus must fail" ~: threeMinusTest
    ]

test' str fun ex =
  TestCase
    (assertBool str $ fun $ ex str)

pass' = isRight
fail' = isLeft

-----------------------------------------------
-- Prepare test
-----------------------------------------------
includeParser = runParser include () ""

expressionParser = runParser expression () ""

-----------------------------------------------
-- Tests
-----------------------------------------------

-- Include
anglesIncludeTest =
  TestCase
    (do let str = "#include <abcd>"
            expected = Right $ Include "abcd"
        assertEqual str expected $ includeParser str)

quotesIncludeTest =
  TestCase
    (do let str = "#include \"abcd\""
            expected = Right $ Include "abcd"
        assertEqual str expected $ includeParser str)

includeWithCommentTest =
  TestCase
    (do let str = "#include <abcd> //comment"
            expected = Right $ Include "abcd"
        assertEqual str expected $ includeParser str)

-- Expression
addExpressionTest =
  TestCase
    (do let str = "42+1"
            expected = Right $ Expr_BinaryOp Op_Add (Expr_Literal $ IntL 42) (Expr_Literal $ IntL 1)
        assertEqual str expected $ expressionParser str)

complexIncrementTest = test' "++i + ++i" pass' expressionParser

threeMinusTest = test' "ab---" fail' expressionParser
threeMinusTest' =
  TestCase
    (do let str = "ab---"
            actual = expressionParser str
        --print actual
        assertBool str $ fail' actual)
