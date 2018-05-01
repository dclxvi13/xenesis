module Main where

import Xenesis.Parsers.Cpp.Parser

import System.IO

main :: IO ()
main = do
  let filepath = "/home/dclxvi13/Dummy.cpp"
  s <- readFile filepath
  print $ parse s
