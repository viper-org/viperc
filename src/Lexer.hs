module Lexer where

import Data.Char

data Token = None
    | TokenError(Char)
    | TokenTypeKeyword String
    | TokenEnumKeyword
    | TokenStructKeyword
    | TokenTypedefKeyword
    | TokenIdentifier String
    | TokenLeftParen
    | TokenRightParen
    | TokenLeftBrace
    | TokenRightBrace
    | TokenLeftBracket
    | TokenRightBracket
    | TokenReturnKeyword
    | TokenIfKeyword
    | TokenElseKeyword
    | TokenWhileKeyword
    | TokenForKeyword
    | TokenBreakKeyword
    | TokenContinueKeyword
    | TokenSwitchKeyword
    | TokenCaseKeyword
    | TokenDefaultKeyword
    | TokenSizeofKeyword
    | TokenIntegerLiteral String
    | TokenStringLiteral String
    | TokenCharLiteral Char
    | TokenNullptr
    | TokenSemicolon
    | TokenComma
    | TokenColon
    | TokenEqual
    | TokenPlus
    | TokenMinus
    | TokenStar
    | TokenSlash
    | TokenDoublePlus
    | TokenDoubleMinus
    | TokenBang
    | TokenAmpersand
    | TokenDoubleAmpersand
    | TokenDoublePipe
    | TokenDoubleEqual
    | TokenBangEqual
    | TokenLessThan
    | TokenGreaterThan
    | TokenLessEqual
    | TokenGreaterEqual
    | TokenPlusEqual
    | TokenMinusEqual
    | TokenEllipsis
    | TokenDot
    | TokenRightArrow
    deriving(Eq, Show)

isType :: Token -> Bool
isType (TokenTypeKeyword _) = True
isType _ = False

isIdentifier :: Token -> Bool
isIdentifier (TokenIdentifier _) = True
isIdentifier _ = False;

isIntegerLiteral :: Token -> Bool
isIntegerLiteral (TokenIntegerLiteral _) = True
isIntegerLiteral _ = False

assignKeywords :: [Token] -> [Token]
assignKeywords [] = []
assignKeywords ((TokenIdentifier "char"):xs)     = TokenTypeKeyword ("char")  : assignKeywords xs
assignKeywords ((TokenIdentifier "short"):xs)    = TokenTypeKeyword ("short") : assignKeywords xs
assignKeywords ((TokenIdentifier "int"):xs)      = TokenTypeKeyword ("int")   : assignKeywords xs
assignKeywords ((TokenIdentifier "long"):xs)     = TokenTypeKeyword ("long")  : assignKeywords xs
assignKeywords ((TokenIdentifier "void"):xs)     = TokenTypeKeyword ("void")  : assignKeywords xs
assignKeywords ((TokenIdentifier "bool"):xs)     = TokenTypeKeyword ("bool")  : assignKeywords xs
assignKeywords ((TokenIdentifier "enum"):xs)     = TokenEnumKeyword           : assignKeywords xs
assignKeywords ((TokenIdentifier "struct"):xs)   = TokenStructKeyword         : assignKeywords xs
assignKeywords ((TokenIdentifier "typedef"):xs)  = TokenTypedefKeyword        : assignKeywords xs
assignKeywords ((TokenIdentifier "return"):xs)   = TokenReturnKeyword         : assignKeywords xs
assignKeywords ((TokenIdentifier "if"):xs)       = TokenIfKeyword             : assignKeywords xs
assignKeywords ((TokenIdentifier "else"):xs)     = TokenElseKeyword           : assignKeywords xs
assignKeywords ((TokenIdentifier "while"):xs)    = TokenWhileKeyword          : assignKeywords xs
assignKeywords ((TokenIdentifier "for"):xs)      = TokenForKeyword            : assignKeywords xs
assignKeywords ((TokenIdentifier "break"):xs)    = TokenBreakKeyword          : assignKeywords xs
assignKeywords ((TokenIdentifier "continue"):xs) = TokenContinueKeyword       : assignKeywords xs
assignKeywords ((TokenIdentifier "switch"):xs)   = TokenSwitchKeyword         : assignKeywords xs
assignKeywords ((TokenIdentifier "case"):xs)     = TokenCaseKeyword           : assignKeywords xs
assignKeywords ((TokenIdentifier "default"):xs)  = TokenDefaultKeyword        : assignKeywords xs
assignKeywords ((TokenIdentifier "sizeof"):xs)   = TokenSizeofKeyword         : assignKeywords xs
assignKeywords ((TokenIdentifier "nullptr"):xs)  = TokenNullptr               : assignKeywords xs
assignKeywords (x:xs) = x : assignKeywords xs

tokenize :: String -> [Token]
tokenize [] = []
tokenize('.':'.':'.':rest) = TokenEllipsis : tokenize rest
tokenize('=':'=':rest) = TokenDoubleEqual : tokenize rest
tokenize('!':'=':rest) = TokenBangEqual : tokenize rest
tokenize('+':'=':rest) = TokenPlusEqual : tokenize rest
tokenize('+':'+':rest) = TokenDoublePlus : tokenize rest
tokenize('-':'-':rest) = TokenDoubleMinus : tokenize rest
tokenize('-':'=':rest) = TokenMinusEqual : tokenize rest
tokenize('<':'=':rest) = TokenLessEqual : tokenize rest
tokenize('>':'=':rest) = TokenGreaterEqual : tokenize rest
tokenize('&':'&':rest) = TokenDoubleAmpersand : tokenize rest
tokenize('|':'|':rest) = TokenDoublePipe : tokenize rest
tokenize('-':'>':rest) = TokenRightArrow : tokenize rest
tokenize('<':rest) = TokenLessThan : tokenize rest
tokenize('>':rest) = TokenGreaterThan : tokenize rest
tokenize ('(':rest) = TokenLeftParen : tokenize rest
tokenize (')':rest) = TokenRightParen : tokenize rest
tokenize ('{':rest) = TokenLeftBrace : tokenize rest
tokenize ('}':rest) = TokenRightBrace : tokenize rest
tokenize ('[':rest) = TokenLeftBracket : tokenize rest
tokenize (']':rest) = TokenRightBracket : tokenize rest
tokenize (';':rest) = TokenSemicolon : tokenize rest
tokenize (',':rest) = TokenComma : tokenize rest
tokenize (':':rest) = TokenColon : tokenize rest
tokenize ('=':rest) = TokenEqual : tokenize rest
tokenize ('+':rest) = TokenPlus : tokenize rest
tokenize ('-':rest) = TokenMinus : tokenize rest
tokenize ('*':rest) = TokenStar : tokenize rest
tokenize ('/':rest) = TokenSlash : tokenize rest
tokenize ('&':rest) = TokenAmpersand : tokenize rest
tokenize ('!':rest) = TokenBang : tokenize rest
tokenize ('.':rest) = TokenDot : tokenize rest
-- Keywords will be transformed afterwards
tokenize (c:rest) | (isAlpha c || c == '_') = TokenIdentifier(c:takeWhile isIdentifierChar rest) : tokenize (dropWhile isIdentifierChar rest)
                  | isDigit c = TokenIntegerLiteral(c:takeWhile isDigit rest) : tokenize (dropWhile isDigit rest)
                  | isSpace c = tokenize (dropWhile isSpace rest)
tokenize('"':rest) = do
    let (str, rest') = parseStringLiteral "" rest
    TokenStringLiteral(str) : tokenize rest'
tokenize('\'':'\\':'n':'\'':rest) = TokenCharLiteral('\n') : tokenize rest
tokenize('\'':'\\':'0':'\'':rest) = TokenCharLiteral('\0') : tokenize rest
tokenize('\'':'\\':'"':'\'':rest) = TokenCharLiteral('"') : tokenize rest
tokenize('\'':'\\':'\'':'\'':rest) = TokenCharLiteral('\'') : tokenize rest
tokenize('\'':'\\':'\\':'\'':rest) = TokenCharLiteral('\\') : tokenize rest
tokenize('\'':c:'\'':rest) = TokenCharLiteral(c) : tokenize rest
tokenize s = [TokenError(head s)]

isIdentifierChar :: Char -> Bool
isIdentifierChar z = isAlphaNum z || z == '_'

parseStringLiteral :: String -> String -> (String, String) -- current string -> rest of text -> (string literal, rest of text)
parseStringLiteral s ('"':rest) = (s, rest)
parseStringLiteral s ('\\':'n':rest) = parseStringLiteral (s++"\n") rest
parseStringLiteral s ('\\':'0':rest) = parseStringLiteral (s++"\0") rest
parseStringLiteral s ('\\':'"':rest) = parseStringLiteral (s++"\"") rest
parseStringLiteral s ('\\':'\'':rest) = parseStringLiteral (s++"'") rest
parseStringLiteral s ('\\':'\\':rest) = parseStringLiteral (s++"\\") rest
parseStringLiteral s (c:rest) = parseStringLiteral (s++[c]) rest
parseStringLiteral s [] = error $ "expected \" to match \""