module Main where

import Lexer
import Parser

main :: IO()
main = do
    contents <- readFile "hello.c"
    case runParser parseFunction (ParserState (assignKeywords (tokenize contents))) of
        Left err -> print err
        Right (val) -> print val