module Lexer where

import Data.Char

data Token = None
    | TokenError(Char)
    | TokenIntType
    | TokenIdentifier String
    | TokenLeftParen
    | TokenRightParen
    | TokenLeftBrace
    | TokenRightBrace
    | TokenReturnKeyword
    | TokenIntegerLiteral String
    | TokenSemicolon
    deriving(Eq, Show)

isType :: Token -> Bool
isType TokenIntType = True
isType _ = False

isIdentifier :: Token -> Bool
isIdentifier (TokenIdentifier _) = True
isIdentifier _ = False;

assignKeywords :: [Token] -> [Token]
assignKeywords [] = []
assignKeywords ((TokenIdentifier "int"):xs) = TokenIntType : assignKeywords xs
assignKeywords ((TokenIdentifier "return"):xs) = TokenReturnKeyword : assignKeywords xs
assignKeywords (x:xs) = x : assignKeywords xs

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':rest) = TokenLeftParen : tokenize rest
tokenize (')':rest) = TokenRightParen : tokenize rest
tokenize ('{':rest) = TokenLeftBrace : tokenize rest
tokenize ('}':rest) = TokenRightBrace : tokenize rest
tokenize (';':rest) = TokenSemicolon : tokenize rest
-- Keywords will be transformed afterwards
tokenize (c:rest) | isAlpha c = TokenIdentifier(c:takeWhile isAlphaNum rest) : tokenize (dropWhile isAlphaNum rest)
                  | isDigit c = TokenIntegerLiteral(c:takeWhile isDigit rest) : tokenize (dropWhile isDigit rest)
                  | isSpace c = tokenize (dropWhile isSpace rest)
tokenize s = [TokenError(head s)]