module Lexer where

import Data.Char

data Token = None
    | TokenError(Char)
    | TokenTypeKeyword String
    | TokenIdentifier String
    | TokenLeftParen
    | TokenRightParen
    | TokenLeftBrace
    | TokenRightBrace
    | TokenReturnKeyword
    | TokenIfKeyword
    | TokenElseKeyword
    | TokenWhileKeyword
    | TokenForKeyword
    | TokenIntegerLiteral String
    | TokenStringLiteral String
    | TokenSemicolon
    | TokenComma
    | TokenEqual
    | TokenPlus
    | TokenMinus
    | TokenStar
    | TokenSlash
    | TokenAmpersand
    | TokenDoubleEqual
    | TokenBangEqual
    | TokenPlusEqual
    | TokenMinusEqual
    deriving(Eq, Show)

isType :: Token -> Bool
isType (TokenTypeKeyword _) = True
isType _ = False

isIdentifier :: Token -> Bool
isIdentifier (TokenIdentifier _) = True
isIdentifier _ = False;

assignKeywords :: [Token] -> [Token]
assignKeywords [] = []
assignKeywords ((TokenIdentifier "char"):xs)   = TokenTypeKeyword ("char")  : assignKeywords xs
assignKeywords ((TokenIdentifier "short"):xs)  = TokenTypeKeyword ("short") : assignKeywords xs
assignKeywords ((TokenIdentifier "int"):xs)    = TokenTypeKeyword ("int")   : assignKeywords xs
assignKeywords ((TokenIdentifier "long"):xs)   = TokenTypeKeyword ("long")  : assignKeywords xs
assignKeywords ((TokenIdentifier "void"):xs)   = TokenTypeKeyword ("void")  : assignKeywords xs
assignKeywords ((TokenIdentifier "bool"):xs)   = TokenTypeKeyword ("bool")  : assignKeywords xs
assignKeywords ((TokenIdentifier "return"):xs) = TokenReturnKeyword         : assignKeywords xs
assignKeywords ((TokenIdentifier "if"):xs)     = TokenIfKeyword             : assignKeywords xs
assignKeywords ((TokenIdentifier "else"):xs)   = TokenElseKeyword           : assignKeywords xs
assignKeywords ((TokenIdentifier "while"):xs)  = TokenWhileKeyword          : assignKeywords xs
assignKeywords ((TokenIdentifier "for"):xs)    = TokenForKeyword            : assignKeywords xs
assignKeywords (x:xs) = x : assignKeywords xs

tokenize :: String -> [Token]
tokenize [] = []
tokenize('=':'=':rest) = TokenDoubleEqual : tokenize rest
tokenize('!':'=':rest) = TokenBangEqual : tokenize rest
tokenize('+':'=':rest) = TokenPlusEqual : tokenize rest
tokenize('-':'=':rest) = TokenMinusEqual : tokenize rest
tokenize ('(':rest) = TokenLeftParen : tokenize rest
tokenize (')':rest) = TokenRightParen : tokenize rest
tokenize ('{':rest) = TokenLeftBrace : tokenize rest
tokenize ('}':rest) = TokenRightBrace : tokenize rest
tokenize (';':rest) = TokenSemicolon : tokenize rest
tokenize (',':rest) = TokenComma : tokenize rest
tokenize ('=':rest) = TokenEqual : tokenize rest
tokenize ('+':rest) = TokenPlus : tokenize rest
tokenize ('-':rest) = TokenMinus : tokenize rest
tokenize ('*':rest) = TokenStar : tokenize rest
tokenize ('/':rest) = TokenSlash : tokenize rest
tokenize ('&':rest) = TokenAmpersand : tokenize rest
-- Keywords will be transformed afterwards
tokenize (c:rest) | isAlpha c = TokenIdentifier(c:takeWhile isAlphaNum rest) : tokenize (dropWhile isAlphaNum rest)
                  | isDigit c = TokenIntegerLiteral(c:takeWhile isDigit rest) : tokenize (dropWhile isDigit rest)
                  | isSpace c = tokenize (dropWhile isSpace rest)
tokenize('"':rest) = do
    let val = takeWhile (/='"') rest
    TokenStringLiteral val : tokenize (drop 1 (dropWhile (/='"') rest))
tokenize s = [TokenError(head s)]