module Parser where

import Lexer
import Types
import AST

import Data.Map as Map
import Data.Char

defaultPrecedence = 1

data ParserState = ParserState {
    tokens :: [Token],
    currentReturnType :: Type,
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

setReturnType :: Type -> Parser()
setReturnType t = Parser $ \s ->
    Right((), s { currentReturnType = t })

getCurrentReturnType :: Parser Type
getCurrentReturnType = Parser $ \s ->
    case currentReturnType s of
        x -> Right(x, s)

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

peek :: Parser (Maybe Token)
peek = Parser $ \s ->
    case tokens s of
        (_:x:_) -> Right(Just x, s)
        _ -> Right (Nothing, s)

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

insertToken :: Token -> Parser () -- for compound statements to insert a semicolon
insertToken tok = Parser $ \s -> Right ((), s {tokens = (tok : tokens s)})

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

parseFile :: Parser [ASTGlobal]
parseFile = do
    tok <- currentTok
    case tok of
        Just (TokenTypeKeyword _) -> do
            curr <- parseGlobal
            rest <- parseFile
            pure (curr : rest)
        _ -> pure []

parseGlobal :: Parser ASTGlobal
parseGlobal = do
    tok <- currentTok
    case tok of
        Just (TokenTypeKeyword _) -> do
            type' <- parseType
            (TokenIdentifier name) <- satisfyToken isIdentifier
            tok <- currentTok
            case tok of
                Just TokenLeftParen -> parseFunction type' name
                Just TokenEqual -> parseGlobalVar type' name
                z -> Parser $ \s -> Left $ "expected declaration. found " ++ show z
        z -> Parser $ \s -> Left $ "expected declaration. found " ++ show z

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

parseFunction :: Type -> String -> Parser ASTGlobal
parseFunction type' name = do
    _ <- expectToken TokenLeftParen
    parms <- parseParams
    _ <- expectToken TokenRightParen
    let paramTypes = [ty | (ty, _) <- parms]
    let fnType = FunctionType type' paramTypes

    setReturnType type'
    addSymbol name fnType
    oldScope <- getSymbols

    mapM_ addSymbolZipped parms

    tok <- currentTok
    case tok of
        Just TokenSemicolon -> do
            _ <- consumeTok
            setSymbols oldScope
            pure $ ASTFunction (FunctionDef fnType name parms [] True)
        _ -> do
            _ <- expectToken TokenLeftBrace
            body <- parseBody
            _ <- expectToken TokenRightBrace
            setSymbols oldScope
            pure $ ASTFunction (FunctionDef fnType name parms body False)

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

parseGlobalVar :: Type -> String -> Parser ASTGlobal
parseGlobalVar type' name = do
    addSymbol name type'
    tok <- currentTok
    case tok of
        Just TokenEqual -> do
            _ <- consumeTok
            initVal <- parseExpr defaultPrecedence
            _ <- expectToken TokenSemicolon
            pure $ ASTGlobalVar type' name initVal
        Just TokenSemicolon -> do
            _ <- consumeTok
            pure $ ASTGlobalVar type' name (ASTNode ASTNothing VoidType)

getBinaryOperatorPrecedence :: Token -> Parser Int
getBinaryOperatorPrecedence TokenLeftParen = pure(90)
getBinaryOperatorPrecedence TokenStar = pure(75)
getBinaryOperatorPrecedence TokenSlash = pure(75)
getBinaryOperatorPrecedence TokenPlus = pure(70)
getBinaryOperatorPrecedence TokenMinus = pure(70)
getBinaryOperatorPrecedence TokenDoubleEqual = pure(50)
getBinaryOperatorPrecedence TokenBangEqual = pure(50)
getBinaryOperatorPrecedence TokenLessThan = pure(50)
getBinaryOperatorPrecedence TokenGreaterThan = pure(50)
getBinaryOperatorPrecedence TokenLessEqual = pure(50)
getBinaryOperatorPrecedence TokenGreaterEqual = pure(50)
getBinaryOperatorPrecedence TokenDoubleAmpersand = pure(30)
getBinaryOperatorPrecedence TokenDoublePipe = pure(25)
getBinaryOperatorPrecedence TokenPlusEqual = pure(20)
getBinaryOperatorPrecedence TokenMinusEqual = pure(20)
getBinaryOperatorPrecedence TokenEqual = pure(20)
getBinaryOperatorPrecedence _ = pure(0)

getBinaryOperator :: Token -> Parser BinaryOperator
getBinaryOperator TokenStar = pure(BinaryMul)
getBinaryOperator TokenSlash = pure(BinaryDiv)
getBinaryOperator TokenPlus = pure(BinaryAdd)
getBinaryOperator TokenMinus = pure(BinarySub)
getBinaryOperator TokenDoubleEqual = pure(BinaryEqual)
getBinaryOperator TokenBangEqual = pure(BinaryNotEqual)
getBinaryOperator TokenLessThan = pure(BinaryLessThan)
getBinaryOperator TokenGreaterThan = pure(BinaryGreaterThan)
getBinaryOperator TokenLessEqual = pure(BinaryLessEqual)
getBinaryOperator TokenGreaterEqual = pure(BinaryGreaterEqual)
getBinaryOperator TokenDoubleAmpersand = pure(LogicalAnd)
getBinaryOperator TokenDoublePipe = pure(LogicalOr)
getBinaryOperator TokenPlusEqual = pure(BinaryAddAssign)
getBinaryOperator TokenMinusEqual = pure(BinarySubAssign)
getBinaryOperator TokenEqual = pure(BinaryAssign)
getBinaryOperator z = Parser $ \s -> Left $ "unexpected attempt to get binary operator " ++ show z

getPrefixUnaryOperatorPrecedence :: Token -> Parser Int
getPrefixUnaryOperatorPrecedence TokenAmpersand = pure(85)
getPrefixUnaryOperatorPrecedence TokenStar = pure(85)
getPrefixUnaryOperatorPrecedence TokenMinus = pure(85)
getPrefixUnaryOperatorPrecedence TokenDoublePlus = pure(85)
getPrefixUnaryOperatorPrecedence TokenDoubleMinus = pure(85)
getPrefixUnaryOperatorPrecedence TokenBang = pure(85)
getPrefixUnaryOperatorPrecedence _ = pure(0)

getPrefixUnaryOperator :: Token -> Parser UnaryOperator
getPrefixUnaryOperator TokenAmpersand = pure(UnaryRef)
getPrefixUnaryOperator TokenStar = pure(UnaryIndirect)
getPrefixUnaryOperator TokenMinus = pure(UnaryMinus)
getPrefixUnaryOperator TokenDoublePlus = pure(PrefixInc)
getPrefixUnaryOperator TokenDoubleMinus = pure(PrefixDec)
getPrefixUnaryOperator TokenBang = pure(LogicalNot)
getPrefixUnaryOperator z = Parser $ \s -> Left $ "unexpected attempt to get unary operator " ++ show z

getPostfixUnaryOperatorPrecedence :: Token -> Parser Int
getPostfixUnaryOperatorPrecedence TokenDoublePlus = pure(90)
getPostfixUnaryOperatorPrecedence TokenDoubleMinus = pure(90)
getPostfixUnaryOperatorPrecedence _ = pure(0)

getPostfixUnaryOperator :: Token -> Parser UnaryOperator
getPostfixUnaryOperator TokenDoublePlus = pure(PostfixInc)
getPostfixUnaryOperator TokenDoubleMinus = pure(PostfixDec)

parseExpr :: Int -> Parser ASTNode
parseExpr prec = do
    tok <- currentTok
    left <- case tok of
        Nothing -> error "!" -- todo: proper error
        Just op -> do
            unaryPrec <- getPrefixUnaryOperatorPrecedence op
            if unaryPrec >= prec then do
                _ <- consumeTok
                operand <- parseExpr unaryPrec
                operator <- getPrefixUnaryOperator op
                pure $ ASTNode (ASTUnaryExpression operator operand) (ty operand)
            else parsePrimary
    post <- parsePost left
    parseBin post
    where
        parsePost l = do
            tok <- currentTok
            case tok of
                Nothing -> pure (l)
                Just op -> do
                    postfixPrec <- getPostfixUnaryOperatorPrecedence op
                    if postfixPrec < prec then pure (l)
                    else do
                        _ <- consumeTok
                        operator <- getPostfixUnaryOperator op
                        parsePost $ ASTNode (ASTUnaryExpression operator l) (ty l)
        parseBin l = do
            tok <- currentTok
            case tok of
                Nothing -> pure(l)
                Just op -> do
                    binaryPrec <- getBinaryOperatorPrecedence op
                    if binaryPrec < prec then pure(l)
                    else do
                        _ <- consumeTok
                        if op == TokenLeftParen then do
                            call <- parseCallExpression l
                            _ <- expectToken TokenRightParen
                            parseBin call
                        else do
                            operator <- getBinaryOperator op
                            right <- parseExpr binaryPrec
                            parseBin $ ASTNode (ASTBinaryExpression l operator right) (ty l)

parsePrimary :: Parser ASTNode
parsePrimary = do
    tok <- currentTok
    case tok of
        Just (TokenIntegerLiteral s) -> do
            _ <- consumeTok
            pure $ ASTNode (ASTIntegerLiteral (read s :: Int)) IntType

        Just (TokenCharLiteral c) -> do
            _ <- consumeTok
            pure $ ASTNode (ASTIntegerLiteral (ord c)) CharType

        Just (TokenIdentifier s) -> do
            _ <- consumeTok
            ty <- getSymbol s
            pure $ ASTNode (ASTVariableExpression s) ty

        Just (TokenStringLiteral s) -> do
            _ <- consumeTok
            pure $ ASTNode (ASTStringLiteral s) (PointerType CharType)

        Just TokenLeftParen -> do
            _ <- consumeTok
            e <- parseExpr defaultPrecedence
            _ <- expectToken TokenRightParen
            pure e

        Just TokenReturnKeyword -> parseReturnStatement
        Just (TokenTypeKeyword _) -> parseVariableDeclaration
        Just TokenIfKeyword -> parseIfStatement
        Just TokenWhileKeyword -> parseWhileStatement
        Just TokenForKeyword -> parseForStatement
        Just TokenLeftBrace -> parseCompoundStatement

        Just TokenBreakKeyword -> do
            _ <- consumeTok
            pure $ ASTNode ASTBreakStatement VoidType

        Just TokenContinueKeyword -> do
            _ <- consumeTok
            pure $ ASTNode ASTContinueStatement VoidType

        Just TokenSemicolon -> pure $ ASTNode ASTNothing VoidType

        _ -> Parser $ \s -> Left $ "Expected primary expression"

-- ASTCallExpression stores the full functiontype for typechecking which then transforms it
parseCallExpression :: Callee -> Parser ASTNode
parseCallExpression callee = do
    tok <- currentTok
    case tok of
        Nothing -> Parser $ \s -> Left $ "Expected ')' to match '('"
        Just TokenRightParen -> pure $ ASTNode (ASTCallExpression callee []) (ty callee)
        _ -> do
            curr <- parseExpr defaultPrecedence
            rest <- parseMore
            pure $ ASTNode (ASTCallExpression callee (curr : rest)) (ty callee)

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

-- ASTReturnStatement type field stores it's function's return type for later checking
parseReturnStatement :: Parser ASTNode
parseReturnStatement = do
    _ <- expectToken TokenReturnKeyword
    retType <- getCurrentReturnType
    tok <- currentTok
    case tok of
        Just TokenSemicolon -> pure $ ASTNode (ASTReturnStatement $ ASTNode ASTNothing VoidType) retType
        _ -> do
            value <- parseExpr defaultPrecedence
            pure $ ASTNode (ASTReturnStatement value) retType

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

parseIfStatement :: Parser ASTNode
parseIfStatement = do
    _ <- consumeTok
    _ <- expectToken TokenLeftParen
    cond <- parseExpr defaultPrecedence
    _ <- expectToken TokenRightParen
    body <- parseExpr defaultPrecedence

    tok <- peek
    case tok of
        Just TokenElseKeyword -> do
            _ <- expectToken TokenSemicolon
            _ <- consumeTok -- else
            elseBody <- parseExpr defaultPrecedence
            pure $ ASTNode (ASTIfStatement cond body elseBody) VoidType
        _ -> pure $ ASTNode (ASTIfStatement cond body (ASTNode ASTNothing VoidType)) VoidType

parseWhileStatement :: Parser ASTNode
parseWhileStatement = do
    _ <- consumeTok
    _ <- expectToken TokenLeftParen
    cond <- parseExpr defaultPrecedence
    _ <- expectToken TokenRightParen
    body <- parseExpr defaultPrecedence

    pure $ ASTNode (ASTWhileStatement cond body) VoidType

parseForStatement :: Parser ASTNode
parseForStatement = do
    _ <- consumeTok -- for
    _ <- expectToken TokenLeftParen
    init <- parseExpr defaultPrecedence
    _ <- expectToken TokenSemicolon

    cond <- parseExpr defaultPrecedence
    _ <- expectToken TokenSemicolon

    tok <- currentTok
    iter <- case tok of
        Just TokenRightParen -> pure $ ASTNode ASTNothing VoidType
        _ -> parseExpr defaultPrecedence

    _ <- expectToken TokenRightParen

    body <- parseExpr defaultPrecedence

    pure $ ASTNode (ASTForStatement init cond iter body) VoidType

    where
        isNothing (ASTNode ASTNothing _) = True
        isNothing _ = False

parseCompoundStatement :: Parser ASTNode
parseCompoundStatement = do
    _ <- consumeTok -- {
    tok <- currentTok
    case tok of
        Just TokenRightBrace -> do
            _ <- consumeTok
            insertToken TokenSemicolon
            pure $ ASTNode (ASTCompoundStatement []) VoidType
        _ -> do
            curr <- parseExpr defaultPrecedence
            _ <- expectToken TokenSemicolon
            rest <- parseMore
            _ <- expectToken TokenRightBrace
            insertToken TokenSemicolon
            pure $ ASTNode (ASTCompoundStatement (curr : rest)) VoidType

        where
            parseMore = do
                tok <- currentTok
                case tok of
                    Just TokenRightBrace -> pure []
                    _ -> do
                        curr <- parseExpr defaultPrecedence
                        _ <- expectToken TokenSemicolon
                        rest <- parseMore
                        pure (curr : rest)