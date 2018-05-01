module CppParserTest where

import           Data.Either                (isLeft)

import           Test.HUnit

import           Text.Parsec                (runParser)

import           Xenesis.Parsers.Cpp.Parser
import           Xenesis.Parsers.Cpp.Syntax

run = runTestTT tests

tests =
  TestList
    [ TestLabel "Parse include in angles" anglesIncludeTest
    , TestLabel "Parse include with quotes" quotesIncludeTest
    , TestLabel "Parse include with comment at the end" includeWithCommentTest
    ]

-----------------------------------------------
-- Prepare test
-----------------------------------------------
includeParser = runParser include () ""

-----------------------------------------------
-- Tests
-----------------------------------------------
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
