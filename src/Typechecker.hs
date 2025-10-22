module Typechecker where

import Types
import AST

typecheckFile :: [FunctionDef] -> [FunctionDef]
typecheckFile decls = do
    [typecheckFunc decl | decl <- decls]

typecheckFunc :: FunctionDef -> FunctionDef
typecheckFunc (FunctionDef a b c body d) = do
    let newBody = [typecheckNode x | x <- body]
    FunctionDef a b c newBody d

typecheckNode :: ASTNode -> ASTNode
typecheckNode (ASTNode (ASTVariableDeclaration ty name (ASTNode ASTNothing _)) ty') = case ty of
    VoidType -> error $ "variable '" ++ name ++ "' has non-object type 'void'"
    t -> (ASTNode (ASTVariableDeclaration ty name (ASTNode ASTNothing VoidType)) ty')
typecheckNode (ASTNode (ASTVariableDeclaration typ name init) ty') = do
    let init' = typecheckNode init
    let initType = ty init'
    case typ of
        VoidType -> error $ "variable '" ++ name ++ "' has non-object type 'void'"
        x -> if x == initType then (ASTNode (ASTVariableDeclaration typ name init') ty')
            else error $ "variable '" ++ name ++ "' has initializer of type '" ++ prettyPrint initType ++ "'"

typecheckNode (ASTNode (ASTReturnStatement val) _) = do
    let retvalType = typecheckNode val
    (ASTNode (ASTReturnStatement val) VoidType) -- todo: check if retvalType matches the current function's return type

typecheckNode (ASTNode (ASTIfStatement cond body elseBody) t) = do
    let cond' = typecheckNode cond
    let body' = typecheckNode body
    let elseBody' = typecheckNode elseBody

    case (ty cond') of
        BoolType -> (ASTNode (ASTIfStatement cond' body' elseBody') t)
        x -> error $ "if-statement has non-boolean condition type '" ++ prettyPrint x ++ "'"

typecheckNode (ASTNode (ASTWhileStatement cond body) t) = do
    let cond' = typecheckNode cond
    let body' = typecheckNode body

    case (ty cond') of
        BoolType -> (ASTNode (ASTWhileStatement cond' body') t)
        x -> error $ "while-statement has non-boolean condition type '" ++ prettyPrint x ++ "'"

typecheckNode (ASTNode (ASTForStatement init cond iter body) t) = do
    let init' = typecheckNode init
    let cond' = typecheckNode cond
    let iter' = typecheckNode iter
    let body' = typecheckNode body

    case cond' of
        (ASTNode ASTNothing _) -> (ASTNode (ASTForStatement init' cond' iter' body') t)
        c -> case (ty c) of
            BoolType -> (ASTNode (ASTForStatement init' cond' iter' body') t)
            x -> error $ "for-statement has non-boolean condition type '" ++ prettyPrint x ++ "'"

typecheckNode (ASTNode (ASTCompoundStatement body) t) = do
    let body' = map typecheckNode body
    (ASTNode (ASTCompoundStatement body') t)

typecheckNode (ASTNode (ASTBinaryExpression l BinaryAdd r) ty') = do
    let l' = typecheckNode l
    let r' = typecheckNode r
    let lType = ty l'
    let rType = ty r'
    case lType of
        (PointerType p) -> do
            if isIntegerType rType then (ASTNode (ASTBinaryExpression l' BinaryAdd r') lType)
            else error $ "binary expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType
        x -> do
            if not (isIntegerType lType) then error $ "binary expression has invalid type " ++ prettyPrint lType
            else
                if isIntegerType rType && lType == rType then (ASTNode (ASTBinaryExpression l' BinaryAdd r') lType)
                else if isPointerType rType then (ASTNode (ASTBinaryExpression l' BinaryAdd r') rType)
                else error $ "binary expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType

typecheckNode (ASTNode (ASTBinaryExpression l op r) ty') = do
    let l' = typecheckNode l
    let r' = typecheckNode r
    case op of
        BinaryEqual -> do
            if (ty l') == (ty r') then (ASTNode (ASTBinaryExpression l' op r') BoolType)
            else error $ "binary expression has differing types " ++ prettyPrint (ty l') ++ " and " ++ prettyPrint (ty r')
        BinaryNotEqual -> do
            if (ty l') == (ty r') then (ASTNode (ASTBinaryExpression l' op r') BoolType)
            else error $ "binary expression has differing types " ++ prettyPrint (ty l') ++ " and " ++ prettyPrint (ty r')
        BinaryAddAssign -> do
            if isPointerType (ty l') && isIntegerType (ty r') then (ASTNode (ASTBinaryExpression l' op r') (ty l'))
            else if (ty l') == (ty r') && isIntegerType (ty l') then (ASTNode (ASTBinaryExpression l' op r') (ty l'))
            else error $ "binary expression has differing types " ++ prettyPrint (ty l') ++ " and " ++ prettyPrint (ty r')
        _ -> do
            if (ty l') == (ty r') then (ASTNode (ASTBinaryExpression l' op r') (ty l'))
            else error $ "binary expression has differing types " ++ prettyPrint (ty l') ++ " and " ++ prettyPrint (ty r')

typecheckNode (ASTNode (ASTUnaryExpression op val) ty') = do
    let val' = typecheckNode val
    case op of
        UnaryRef -> ASTNode (ASTUnaryExpression op val') (PointerType $ ty val')
        UnaryIndirect -> ASTNode (ASTUnaryExpression op val') (getPointeeType $ ty val')

typecheckNode n = n