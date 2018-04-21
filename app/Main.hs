module Main where

import Xenesis.Parsers.Cpp

import System.IO

main :: IO ()
main = do
  let filepath = "/home/dclxvi13/Dummy.cpp"
  --withFile filepath ReadMode $ \handle -> undefined
  s <- readFile filepath
  print $ parse s
