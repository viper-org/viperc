module Typechecker where

import Types
import AST

import Data.List

typecheckFile :: [ASTGlobal] -> [ASTGlobal]
typecheckFile decls = do
    [typecheckGlob decl | decl <- decls]

typecheckGlob :: ASTGlobal -> ASTGlobal
typecheckGlob (ASTFunction (FunctionDef a b c d e)) = typecheckFunc (FunctionDef a b c d e)
typecheckGlob (ASTGlobalVar type' name initVal) = typecheckGlobalVar (ASTGlobalVar type' name initVal)
typecheckGlob (ASTEnum (EnumDef a b c)) = ASTEnum (EnumDef a b c)
typecheckGlob (ASTStruct z) = ASTStruct z

typecheckGlobalVar :: ASTGlobal -> ASTGlobal
typecheckGlobalVar (ASTGlobalVar type' name (ASTNode ASTNothing _)) = case type' of
    VoidType -> error $ "variable '" ++ name ++ "' has non-object type 'void'"
    t -> ASTGlobalVar type' name (ASTNode ASTNothing VoidType)
typecheckGlobalVar (ASTGlobalVar type' name init) = do
    let init' = typecheckNode init
    let initType = ty init'
    case type' of
        VoidType -> error $ "global variable '" ++ name ++ "' has non-object type 'void'"
        x -> if x == initType then ASTGlobalVar type' name init'
            else error $ "global variable '" ++ name ++ "' has initializer of type '" ++ prettyPrint initType ++ "'"

typecheckFunc :: FunctionDef -> ASTGlobal
typecheckFunc (FunctionDef a b c body d) = do
    let newBody = [typecheckNode x | x <- body]
    ASTFunction $ FunctionDef a b c newBody d

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
            else if (castLevel initType x) == Implicit then
                    ASTNode (ASTVariableDeclaration typ name $ ASTNode (ASTCastExpression init' typ) typ) ty'
                else
                    error $ "variable '" ++ name ++ "' has initializer of type '" ++ prettyPrint initType ++ "'"

typecheckNode (ASTNode (ASTReturnStatement val) retType) = do
    let val' = typecheckNode val
    let valType = ty val'
    if valType == retType then ASTNode (ASTReturnStatement val') retType
    else if (castLevel valType retType) == Implicit then
        ASTNode (ASTReturnStatement (ASTNode (ASTCastExpression val' retType) retType)) retType
    else error $ "cannot return type '" ++ prettyPrint valType ++ "' in place of ' " ++ prettyPrint retType ++ "'"

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
                else if (castLevel lType rType) == Implicit then
                    if typeSize lType > typeSize rType then
                        ASTNode (ASTBinaryExpression l' BinaryAdd (ASTNode (ASTCastExpression r' lType) lType)) lType
                    else ASTNode (ASTBinaryExpression (ASTNode (ASTCastExpression l' rType) rType) BinaryAdd r') rType
                else error $ "binary expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType

typecheckNode (ASTNode (ASTBinaryExpression l BinaryIndex r) ty') = do
    let l' = typecheckNode l
    let r' = typecheckNode r
    let lType = ty l'
    let rType = ty r'

    if isPointerType lType || isArrayType lType then
        if isIntegerType rType then
            (ASTNode (ASTBinaryExpression l' BinaryIndex r') (getElementType lType))
        else error $ "index expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType
    else if isPointerType rType || isArrayType rType then
        if isIntegerType lType then
            (ASTNode (ASTBinaryExpression r' BinaryIndex l') (getElementType rType))
        else error $ "index expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType
    else error $ "index expression has invalid types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType

    where
        getElementType (PointerType p) = p
        getElementType (ArrayType e l) = e

typecheckNode (ASTNode (ASTBinaryExpression l BinaryAssign r) ty') = do
    let l' = typecheckNode l
    let r' = typecheckNode r
    let lType = ty l'
    let rType = ty r'
    if lType == rType then ASTNode (ASTBinaryExpression l' BinaryAssign r') ty'
    else if (castLevel lType rType) == Implicit then
        ASTNode (ASTBinaryExpression l' BinaryAssign (ASTNode (ASTCastExpression r' lType) lType)) ty'
    else error $ "cannot assign " ++ prettyPrint rType ++ " to value of type " ++ show lType

typecheckNode (ASTNode (ASTBinaryExpression l op r) ty') = do
    let l' = typecheckNode l
    let r' = typecheckNode r
    let lType = ty l'
    let rType = ty r'
    let destType = case op of
            BinaryEqual -> BoolType
            BinaryNotEqual -> BoolType
            BinaryLessThan -> BoolType
            BinaryGreaterThan -> BoolType
            BinaryLessEqual -> BoolType
            BinaryGreaterEqual -> BoolType
            _ -> lType

    case op of
        BinaryAddAssign -> do
            if isPointerType lType && isIntegerType rType then (ASTNode (ASTBinaryExpression l' op r') lType)
            else if lType == rType && isIntegerType lType then (ASTNode (ASTBinaryExpression l' op r') lType)
            else error $ "binary expression has differing types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType
        _ -> do
            if lType == rType then (ASTNode (ASTBinaryExpression l' op r') destType)
            else if (castLevel lType rType) == Implicit then
                if typeSize lType > typeSize rType then
                    ASTNode (ASTBinaryExpression l' op (ASTNode (ASTCastExpression r' lType) lType)) destType
                else if destType == lType then
                    ASTNode (ASTBinaryExpression (ASTNode (ASTCastExpression l' rType) rType) op r') rType
                else
                    ASTNode (ASTBinaryExpression (ASTNode (ASTCastExpression l' rType) rType) op r') destType
            else error $ "binary expression has differing types " ++ prettyPrint lType ++ " and " ++ prettyPrint rType

-- todo: actually check types are valid for each op
typecheckNode (ASTNode (ASTUnaryExpression op val) ty') = do
    let val' = typecheckNode val
    case op of
        UnaryRef -> ASTNode (ASTUnaryExpression op val') (PointerType $ ty val')
        UnaryIndirect -> ASTNode (ASTUnaryExpression op val') (getPointeeType $ ty val')

        UnaryMinus  -> ASTNode (ASTUnaryExpression op val') (ty val')

        LogicalNot -> ASTNode (ASTUnaryExpression op val') BoolType
        
        PrefixInc  -> ASTNode (ASTUnaryExpression op val') (ty val')
        PrefixDec  -> ASTNode (ASTUnaryExpression op val') (ty val')
        PostfixInc -> ASTNode (ASTUnaryExpression op val') (ty val')
        PostfixDec -> ASTNode (ASTUnaryExpression op val') (ty val')

typecheckNode (ASTNode (ASTCallExpression callee params) fnType) = do
    let callee' = typecheckNode callee
    let params' = [typecheckNode p | p <- params]
    let argTypes = getArgumentTypes fnType

    -- Only check the parameters before the vararg (if it exists)
    let argTypes' = takeWhile (not . isVarArgType) argTypes

    if (length params') < (length argTypes') then
        error $ "function call requires " ++ show (length argTypes') ++ " parameters, has " ++ show (length params')
    else do
        let p = map checkOne (zip params' argTypes')
        let rest = drop (length argTypes') params'
        ASTNode (ASTCallExpression callee' (p++rest)) (getReturnType fnType)

    where
        checkOne (p, argT) = do
            if (ty p) == argT then p
            else if (castLevel (ty p) argT) == Implicit then
                ASTNode (ASTCastExpression p argT) argT
            else error $ "parameter with type '" ++ prettyPrint (ty p) ++ "' cannot cast to '" ++ prettyPrint argT ++ "'"

typecheckNode (ASTNode (ASTSizeofExpression e) _) = do
    let e' = typecheckNode e
    ASTNode (ASTSizeofExpression e') IntType

typecheckNode (ASTNode (ASTMemberAccess struct id isPointer) _) = do
    let struct' = typecheckNode struct
    let typ = (ty struct')
    case typ of
        (StructType _ ps) -> do
            let m = find (matches id) ps
            case m of
                Nothing -> error $ "could not find member '" ++ id ++ "'"
                Just (_, ty) -> ASTNode (ASTMemberAccess struct' id isPointer) ty
        _ -> error $ "operator '.' is not defined for type " ++ prettyPrint typ

    where
        matches id (id', _) = id == id'

typecheckNode (ASTNode (ASTSwitchStatement val cases) ty') = do
    let val' = typecheckNode val
    let cases' = map typecheckCase cases
    ASTNode (ASTSwitchStatement val' cases') ty'

    where
        typecheckCase (SwitchCase (Valued z) body) = do
            let z' = typecheckNode z
            let body' = map typecheckNode body
            (SwitchCase (Valued z') body')

        typecheckCase (SwitchCase Default body) = do
            let body' = map typecheckNode body
            (SwitchCase Default body')

typecheckNode n = n