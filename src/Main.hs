module Main where

import Lexer

main :: IO()
main = do
    contents <- readFile "hello.c"
    print (foldr (++) "" [(show x) ++ " " | x <- assignKeywords (tokenize contents)])