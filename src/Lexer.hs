module Lexer where

import Data.Char

data Token = None
    | Error(Char)
    | IntType
    | Identifier(String)
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | ReturnKeyword
    | IntegerLiteral(String)
    | Semicolon
    deriving(Eq, Show)

assignKeywords :: [Token] -> [Token]
assignKeywords [] = []
assignKeywords ((Identifier "int"):xs) = IntType : assignKeywords xs
assignKeywords ((Identifier "return"):xs) = ReturnKeyword : assignKeywords xs
assignKeywords (x:xs) = x : assignKeywords xs

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':rest) = LeftParen : tokenize rest
tokenize (')':rest) = RightParen : tokenize rest
tokenize ('{':rest) = LeftBrace : tokenize rest
tokenize ('}':rest) = RightBrace : tokenize rest
tokenize (';':rest) = Semicolon : tokenize rest
-- Keywords will be transformed afterwards
tokenize (c:rest) | isAlpha c = Identifier(c:takeWhile isAlphaNum rest) : tokenize (dropWhile isAlphaNum rest)
                  | isDigit c = IntegerLiteral(c:takeWhile isDigit rest) : tokenize (dropWhile isDigit rest)
                  | isSpace c = tokenize (dropWhile isSpace rest)
tokenize s = [Error(head s)]