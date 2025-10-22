module Parser where

import Lexer
import Types
import AST

import Data.Map as Map

defaultPrecedence = 1

data ParserState = ParserState {
    tokens :: [Token],
    symbols :: Map String Type -- maybe make this a proper symbol type later
} deriving (Eq, Show)

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

getSymbols :: Parser (Map String Type)
getSymbols = Parser $ \s ->
    case symbols s of
        (xs) -> Right(xs, s)

getSymbol :: String -> Parser Type
getSymbol ident = do
    syms <- getSymbols
    case Map.lookup ident syms of
        Just sym -> pure sym
        Nothing -> Parser $ \s -> Left $ "Could not find symbol '" ++ ident ++ "'"

addSymbol :: String -> Type -> Parser ()
addSymbol name type' = Parser $ \s ->
    Right((), s { symbols = Map.insert name type' (symbols s) })

addSymbolZipped :: (Type, String) -> Parser ()
addSymbolZipped (type', name) = addSymbol name type'

setSymbols :: Map String Type -> Parser()
setSymbols m = Parser $ \s ->
    Right((), s { symbols = m })

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

parseType :: Parser (Type)
parseType = do
    tok <- currentTok
    case tok of
        Nothing -> Parser $ \s -> Left $ "expected a type"
        Just (TokenTypeKeyword name) -> do
            _ <- consumeTok
            case name of
                "char"  -> parseMore(CharType)
                "short" -> parseMore(ShortType)
                "int"   -> parseMore(IntType)
                "long"  -> parseMore(LongType)
                "void"  -> parseMore(VoidType)
                "bool"  -> parseMore(BoolType)
            
            where
                parseMore type' = do
                    tok <- currentTok
                    case tok of
                        Just TokenStar -> do
                            _ <- consumeTok
                            parseMore $ PointerType type'
                        _ -> pure (type')

parseFile :: Parser [FunctionDef]
parseFile = do
    tok <- currentTok
    case tok of
        Just (TokenTypeKeyword _) -> do
            curr <- parseFunction
            rest <- parseFile
            pure (curr : rest)
        _ -> pure []

parseParams :: Parser [(Type, String)]
parseParams = do
    tok <- currentTok
    case tok of
        Just TokenRightParen -> pure []
        _ -> do
            parm <- parseParam
            rest <- parseMore
            pure (parm : rest)

        where
            parseMore = do
                tok <- currentTok
                case tok of
                    Just TokenComma -> do
                        _ <- consumeTok
                        parm <- parseParam
                        rest <- parseMore
                        pure (parm : rest)
                    _ -> pure []

parseParam :: Parser (Type, String)
parseParam = do
    type' <- parseType
    (TokenIdentifier  parmName) <- satisfyToken isIdentifier
    pure (type', parmName)

parseFunction :: Parser FunctionDef
parseFunction = do
    type' <- parseType
    TokenIdentifier name <- satisfyToken isIdentifier
    _ <- expectToken TokenLeftParen
    parms <- parseParams
    _ <- expectToken TokenRightParen
    let paramTypes = [ty | (ty, _) <- parms]
    let fnType = FunctionType type' paramTypes

    addSymbol name fnType

    oldScope <- getSymbols

    mapM_ addSymbolZipped parms

    tok <- currentTok
    case tok of
        Just TokenSemicolon -> do
            _ <- consumeTok
            setSymbols oldScope
            pure (FunctionDef fnType name parms [] True)
        _ -> do
            _ <- expectToken TokenLeftBrace
            body <- parseBody
            _ <- expectToken TokenRightBrace
            setSymbols oldScope
            pure (FunctionDef fnType name parms body False)

            where
                parseBody = do
                    tok <- currentTok
                    case tok of
                        Just TokenRightBrace -> pure [] -- end of the function
                        _ -> do
                            curr <- parseExpr defaultPrecedence
                            _ <- expectToken TokenSemicolon
                            rest <- parseBody
                            pure (curr : rest)

getBinaryOperatorPrecedence :: Token -> Parser Int
getBinaryOperatorPrecedence TokenLeftParen = pure(90)
getBinaryOperatorPrecedence TokenStar = pure(75)
getBinaryOperatorPrecedence TokenSlash = pure(75)
getBinaryOperatorPrecedence TokenPlus = pure(70)
getBinaryOperatorPrecedence TokenMinus = pure(70)
getBinaryOperatorPrecedence TokenDoubleEqual = pure(50)
getBinaryOperatorPrecedence TokenBangEqual = pure(50)
getBinaryOperatorPrecedence TokenEqual = pure(20)
getBinaryOperatorPrecedence _ = pure(0)

getBinaryOperator :: Token -> Parser BinaryOperator
getBinaryOperator TokenStar = pure(BinaryMul)
getBinaryOperator TokenSlash = pure(BinaryDiv)
getBinaryOperator TokenPlus = pure(BinaryAdd)
getBinaryOperator TokenMinus = pure(BinarySub)
getBinaryOperator TokenDoubleEqual = pure(BinaryEqual)
getBinaryOperator TokenBangEqual = pure(BinaryNotEqual)
getBinaryOperator TokenEqual = pure(BinaryAssign)
getBinaryOperator z = Parser $ \s -> Left $ "unexpected attempt to get binary operator " ++ show z

getUnaryOperatorPrecedence :: Token -> Parser Int
getUnaryOperatorPrecedence TokenAmpersand = pure(85)
getUnaryOperatorPrecedence TokenStar = pure(85)
getUnaryOperatorPrecedence _ = pure(0)

getUnaryOperator :: Token -> Parser UnaryOperator
getUnaryOperator TokenAmpersand = pure(UnaryRef)
getUnaryOperator TokenStar = pure(UnaryIndirect)
getUnaryOperator z = Parser $ \s -> Left $ "unexpected attempt to get unary operator " ++ show z

parseExpr :: Int -> Parser ASTNode
parseExpr prec = do
    tok <- currentTok
    left <- case tok of
        Nothing -> error "!" -- todo: proper error
        Just op -> do
            unaryPrec <- getUnaryOperatorPrecedence op
            if unaryPrec >= prec then do
                _ <- consumeTok
                operand <- parseExpr unaryPrec
                operator <- getUnaryOperator op
                pure $ ASTNode (ASTUnaryExpression operator operand) (ty operand)
            else parsePrimary
    parseMore left
    where
        parseMore l = do
            tok <- currentTok
            case tok of
                Nothing -> pure(l)
                Just op -> do
                    newPrec <- getBinaryOperatorPrecedence op
                    if newPrec < prec then pure(l)
                    else do
                        _ <- consumeTok
                        if op == TokenLeftParen then do
                            call <- parseCallExpression l
                            _ <- expectToken TokenRightParen
                            parseMore call
                        else do
                            operator <- getBinaryOperator op
                            right <- parseExpr newPrec
                            parseMore $ ASTNode (ASTBinaryExpression l operator right) (ty l)

parsePrimary :: Parser ASTNode
parsePrimary = do
    tok <- currentTok
    case tok of
        Just (TokenIntegerLiteral s) -> do
            _ <- consumeTok
            pure $ ASTNode (ASTIntegerLiteral (read s :: Int)) IntType

        Just (TokenIdentifier s) -> do
            _ <- consumeTok
            ty <- getSymbol s
            pure $ ASTNode (ASTVariableExpression s) ty

        Just (TokenStringLiteral s) -> do
            _ <- consumeTok
            pure $ ASTNode (ASTStringLiteral s) (PointerType CharType)

        Just TokenReturnKeyword -> parseReturnStatement
        Just (TokenTypeKeyword _) -> parseVariableDeclaration
        _ -> Parser $ \s -> Left $ "Expected primary expression"

parseCallExpression :: Callee -> Parser ASTNode
parseCallExpression callee = do
    let retType = getReturnType (ty callee)
    tok <- currentTok
    case tok of
        Nothing -> Parser $ \s -> Left $ "Expected ')' to match '('"
        Just TokenRightParen -> pure $ ASTNode (ASTCallExpression callee []) retType
        _ -> do
            curr <- parseExpr defaultPrecedence
            rest <- parseMore
            pure $ ASTNode (ASTCallExpression callee (curr : rest)) retType

        where
            parseMore = do
                tok <- currentTok
                case tok of
                    Just TokenRightParen -> pure []
                    Just TokenComma -> do
                        _ <- consumeTok
                        curr <- parseExpr defaultPrecedence
                        rest <- parseMore
                        pure (curr : rest)
                    _ -> Parser $ \s -> Left $ "Expected ')' to match '('"

parseReturnStatement :: Parser ASTNode
parseReturnStatement = do
    _ <- expectToken TokenReturnKeyword
    tok <- currentTok
    case tok of
        Just TokenSemicolon -> pure $ ASTNode (ASTReturnStatement $ ASTNode ASTNothing VoidType) VoidType
        _ -> do
            value <- parseExpr defaultPrecedence
            pure $ ASTNode (ASTReturnStatement value) VoidType

parseVariableDeclaration :: Parser ASTNode
parseVariableDeclaration = do
    type' <- parseType
    TokenIdentifier name <- satisfyToken isIdentifier
    addSymbol name type'
    tok <- currentTok
    case tok of
        Just TokenEqual -> do
            _ <- consumeTok
            initVal <- parseExpr defaultPrecedence
            pure $ ASTNode (ASTVariableDeclaration type' name initVal) type'
        _ -> pure $ ASTNode (ASTVariableDeclaration type' name (ASTNode ASTNothing VoidType)) type'