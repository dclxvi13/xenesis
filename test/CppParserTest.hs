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
    , "Parse simple add expression" ~: addExpressionTest
    ]

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
    (do let str = "42 + 1"
        assertBool str $ isRight $ expressionParser str)
