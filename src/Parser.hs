module Parser where

import Lexer

type Name = String
type ReturnValue = ASTNode
type InitialValue = ASTNode

data ASTNode = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Name InitialValue
             | ASTIntegerLiteral Int
             | ASTVariableExpression Name
             deriving (Eq, Show)

data FunctionDef = FunctionDef {
    name :: String,
    body :: [ASTNode]
} deriving (Eq, Show)

data ParserState = ParserState { tokens :: [Token] } deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: ParserState -> Either String (a, ParserState) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    pure (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s1) <- pf s
    (a, s2) <- pa s1
    pure (f a, s2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> do
    (a, s1) <- p s
    runParser (f a) s1

instance MonadFail Parser where
  fail msg = Parser $ \s -> Left msg

consumeTok :: Parser Token
consumeTok = Parser $ \s ->
    case tokens s of
        [] -> Left $ "Unexpected EOF"
        (x:xs) -> Right (x, s {tokens = xs})

currentTok :: Parser (Maybe Token)
currentTok = Parser $ \s ->
    case tokens s of
        [] -> Right (Nothing, s)
        (x:_) -> Right (Just x, s)

satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken pred = do
    t <- consumeTok
    if pred t then pure t
    else Parser $ \s -> Left $ "Unexpected token" -- Todo: better error

expectToken :: Token -> Parser ()
expectToken z = do
    t <- consumeTok
    if t == z then pure ()
    else Parser $ \s -> Left $ "expected " ++ show z ++ ", found " ++ show t -- Todo: better error

parseFile :: Parser [FunctionDef]
parseFile = do
    tok <- currentTok
    case tok of
        Just TokenIntType -> do
            curr <- parseFunction
            rest <- parseFile
            pure (curr : rest)
        _ -> pure []

parseFunction :: Parser FunctionDef
parseFunction = do
    _ <- expectToken TokenIntType
    TokenIdentifier name <- satisfyToken isIdentifier
    _ <- expectToken TokenLeftParen
    _ <- expectToken TokenRightParen
    _ <- expectToken TokenLeftBrace
    body <- parseBody
    _ <- expectToken TokenRightBrace
    pure (FunctionDef name body)

    where
        parseBody = do
            tok <- currentTok
            case tok of
                Just TokenRightBrace -> pure [] -- end of the function
                _ -> do
                    curr <- parseExpr
                    _ <- expectToken TokenSemicolon
                    rest <- parseBody
                    pure (curr : rest)

parseExpr :: Parser ASTNode
parseExpr = do
    tok <- currentTok
    case tok of
        Just (TokenIntegerLiteral s) -> do
            _ <- consumeTok
            pure (ASTIntegerLiteral (read s :: Int))

        Just (TokenIdentifier s) -> do
            _ <- consumeTok
            pure (ASTVariableExpression s)

        Just TokenReturnKeyword -> parseReturnStatement
        Just TokenIntType -> parseVariableDeclaration
        _ -> Parser $ \s -> Left $ "Expected expression"

parseReturnStatement :: Parser ASTNode
parseReturnStatement = do
    _ <- expectToken TokenReturnKeyword
    tok <- currentTok
    case tok of
        Just TokenSemicolon -> pure (ASTReturnStatement ASTNothing)
        _ -> do
            value <- parseExpr
            pure (ASTReturnStatement value)

parseVariableDeclaration :: Parser ASTNode
parseVariableDeclaration = do
    _ <- expectToken TokenIntType
    TokenIdentifier name <- satisfyToken isIdentifier
    tok <- currentTok
    case tok of
        Just TokenEqual -> do
            _ <- consumeTok
            initVal <- parseExpr
            pure (ASTVariableDeclaration name initVal)
        _ -> pure (ASTVariableDeclaration name ASTNothing)