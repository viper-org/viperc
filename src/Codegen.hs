{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Codegen where

import AST
import Types

import Control.Applicative
import Control.Monad.State
import Data.Function
import Data.List
import Data.Map as Map
import Data.Monoid ((<>))
import Data.String
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import Data.String.Conversions (cs)
import Data.Word
import LLVM.AST
import qualified LLVM.AST as AST hiding(function)
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.IntegerPredicate as L
import qualified LLVM.AST.Type as AST
import LLVM.AST.Typed (typeOf)
import LLVM.IRBuilder.Module as L;
import LLVM.IRBuilder.Instruction as L;
import LLVM.IRBuilder.Monad as L;
import LLVM.IRBuilder.Constant as L;

-- todo: proper scope-level local variables
data Scope = Scope {
    locals :: Map String AST.Operand,
    consts :: Map String AST.Operand,
    uniqueID :: Int,
    breakTo :: AST.Name,
    continueTo :: AST.Name
}

type LLVM = L.ModuleBuilderT (State Scope)

type Builder = L.IRBuilderT LLVM

data CodegenOperand = None | Some (AST.Operand)

force :: CodegenOperand -> Operand
force c = case c of
    None -> error "!"
    (Some x) -> x

codegenTerm :: Builder() -> Builder()
codegenTerm op = do
    hasTerm <- L.hasTerminator
    unless hasTerm op

nextID :: MonadState Scope m => m (Int)
nextID = do
    id <- gets uniqueID
    modify $ \env -> env { uniqueID = id + 1 }
    pure (id)

getUniqName :: Builder SBS.ShortByteString
getUniqName = do
    id <- nextID
    pure (SBS.toShort (BS.pack $ show id))

addLocal :: MonadState Scope m => String -> AST.Operand -> m ()
addLocal name value = modify $ \env -> env { locals = Map.insert name value (locals env) }

addConst :: MonadState Scope m => String -> AST.Operand -> m ()
addConst name value = modify $ \env -> env { consts = Map.insert name value (consts env) }

getLocal :: String -> Builder CodegenOperand
getLocal s = do
    vars <- gets locals
    case Map.lookup s vars of
        Just var -> pure (Some var)
        Nothing -> pure None

getConst :: String -> Builder Operand
getConst s = do
    vars <- gets consts
    case Map.lookup s vars of
        Just var -> pure var
        Nothing -> error $ "variable '" ++ show s ++ "' does not exist in this scope"

mTypeToLLVM :: MonadState Scope m => Types.Type -> m AST.Type
mTypeToLLVM ty = pure $ typeToLLVM ty

paramNameToName :: ParameterName -> Name
paramNameToName (ParameterName z) = Name z

functionVarArgs
  :: (MonadModuleBuilder m, MonadFix m)
  => Name
  -> [(AST.Type, ParameterName)]
  -> AST.Type
  -> ([Operand] -> IRBuilderT m ())
  -> m Operand
functionVarArgs nm args retType body = mdo
    let args' = [Parameter ty (paramNameToName name) [] | (ty, name) <- args]
    let fn = GlobalDefinition functionDefaults
            { name        = nm
            , parameters  = (args', True)
            , returnType  = retType
            , basicBlocks = blocks
            }
    emitDefn fn
    ((), blocks) <- runIRBuilderT emptyIRBuilder (body [])
    pure $ ConstantOperand (C.GlobalReference (AST.ptr (AST.FunctionType retType (fst <$> args) True)) nm)


codegenFile :: [ASTGlobal] -> AST.Module
codegenFile decls =
    flip evalState (Scope{locals = Map.empty, consts = Map.empty, uniqueID = 0, breakTo = "", continueTo = ""}) $
        L.buildModuleT "hello.c" $ do
            mapM_ codegenDecl decls

codegenDecl :: ASTGlobal -> LLVM()
codegenDecl (ASTFunction (FunctionDef a b c d e)) = codegenFuncDef (FunctionDef a b c d e)
codegenDecl (ASTGlobalVar a b c) = codegenGlobalVar (ASTGlobalVar a b c)
codegenDecl (ASTEnum (EnumDef a b c)) = codegenEnumDef (EnumDef a b c)
codegenDecl (ASTStruct _) = pure()

codegenGlobalVar :: ASTGlobal -> LLVM ()
codegenGlobalVar (ASTGlobalVar type' name init) = do
    c <- codegenNodeConstant init
    glob <- L.global (Name (SBS.toShort (BS.pack name))) (typeToLLVM type') c
    addLocal name glob
    pure ()

codegenEnumDef :: EnumDef -> LLVM ()
codegenEnumDef (EnumDef name base vals) = do
    mapM_ addValue vals

    where
        addValue (name, val) = addConst name (L.int32 $ fromIntegral val)

codegenFuncDef :: FunctionDef -> LLVM ()
codegenFuncDef (FunctionDef typ name args body isProto) | hasVarArgs args = mdo
    addLocal name funct
    scope <- gets locals
    let args' = Data.List.filter (not . isVariArg) args
    let parms = [generateParm type' name' | (type', name') <- args']
    let returnType = getReturnType typ
    funct <- case isProto of
        True -> functionVarArgs (AST.mkName $ cs name) parms (typeToLLVM returnType) emitProto
        False -> functionVarArgs (AST.mkName $ cs name) parms (typeToLLVM returnType) emitBody
    modify $ \env -> env { locals = scope }
    pure()
    where
        generateParm type' name' = ((typeToLLVM type'), ParameterName (SBS.toShort (BS.pack name')))
        emitBody paramOps = do
            z <- L.named L.block "entry"
            mapM_ initParam (zip args paramOps)
            mapM_ codegenNode body

            where
                initParam ((type', name'), parmOp) = do
                    alloca <- L.alloca (typeToLLVM type') Nothing 0
                    L.store alloca 0 parmOp
                    addLocal name' alloca
        emitProto _ = pure()

codegenFuncDef (FunctionDef typ name args body isProto) | not $ hasVarArgs args = mdo
    addLocal name funct
    scope <- gets locals
    let parms = [generateParm type' name' | (type', name') <- args]
    let returnType = getReturnType typ
    funct <- case isProto of
        True -> L.function (AST.mkName $ cs name) parms (typeToLLVM returnType) emitProto
        False -> L.function (AST.mkName $ cs name) parms (typeToLLVM returnType) emitBody
    modify $ \env -> env { locals = scope }
    pure()

    where
        generateParm type' name' = ((typeToLLVM type'), ParameterName (SBS.toShort (BS.pack name')))
        emitBody paramOps = do
            _ <- L.block `L.named` "entry"
            mapM_ initParam (zip args paramOps)
            mapM_ codegenNode body

            where
                initParam ((type', name'), parmOp) = do
                    alloca <- L.alloca (typeToLLVM type') Nothing 0
                    L.store alloca 0 parmOp
                    addLocal name' alloca
        emitProto _ = pure()

codegenNodeLVal :: ASTNode -> Builder AST.Operand
codegenNodeLVal (ASTNode (ASTVariableExpression ident) ty') = do
    var <- getLocal ident
    pure(force var)

codegenNodeLVal (ASTNode (ASTUnaryExpression UnaryIndirect op) _) = do
    z <- codegenNode op
    case z of
        None -> error "!"
        (Some x) -> pure x

codegenNodeLVal (ASTNode (ASTBinaryExpression l BinaryIndex r) _) = do
    case (ty l) of
        (Types.PointerType _) -> do
            left <- codegenNode l
            right <- codegenNode r
            gep <- L.gep (force left) [force right]
            pure gep

        (Types.ArrayType _ _) -> do
            left <- codegenNodeLVal l
            right <- codegenNode r
            gep <- L.gep left [force right, (L.int32 0)]
            load <- L.load gep 0
            pure gep

codegenNodeLVal (ASTNode (ASTMemberAccess struct id isPointer) _) = do
    struc <- codegenNodeLVal struct
    let structType = ty struct
    case structType of
        (StructType _ ps) -> do
            let idx = Data.List.findIndex (matches id) ps
            case idx of
                Just idx' -> do
                    gep <- L.gep struc [(L.int32 $ fromIntegral idx'), (L.int32 $ fromIntegral 0)]
                    pure gep

    where
        matches id (id', _)= id == id'

codegenNodeLVal x = error $ "non-lvalue: " ++ show x

codegenNodeConstant :: ASTNode -> LLVM C.Constant
codegenNodeConstant (ASTNode (ASTIntegerLiteral i) _) = pure $ C.Int (fromIntegral i) (fromIntegral i)

codegenNodeConditional :: ASTNode -> AST.Name -> AST.Name -> Builder CodegenOperand
codegenNodeConditional (ASTNode (ASTBinaryExpression l LogicalAnd r) _) trueBB falseBB = mdo
    codegenNodeConditional l newBB falseBB
    newBB <- L.named L.block "inter"
    codegenNodeConditional r trueBB falseBB
    pure(None)

codegenNodeConditional (ASTNode (ASTBinaryExpression l LogicalOr r) _) trueBB falseBB = mdo
    codegenNodeConditional l trueBB newBB
    newBB <- L.named L.block "inter"
    codegenNodeConditional r trueBB falseBB
    pure(None)
    
codegenNodeConditional (ASTNode (ASTUnaryExpression LogicalNot x) _) trueBB falseBB = mdo
    codegenNodeConditional x falseBB trueBB
    pure(None)

codegenNodeConditional cond trueBB falseBB = do
    cond' <- codegenNode cond
    L.condBr (force cond') trueBB falseBB
    pure(None)

codegenNode :: ASTNode -> Builder CodegenOperand
codegenNode (ASTNode (ASTReturnStatement value) _) = case value of
    (ASTNode ASTNothing _) -> do
        L.retVoid
        pure(None)
    val -> do
        retVal <- codegenNode val
        case retVal of
            None -> error "unexpected node in return" -- todo: proper error
            Some (x) -> L.ret x
        pure (None)

codegenNode (ASTNode (ASTVariableDeclaration type' name initVal) _) = do
    alloca <- L.alloca (typeToLLVM type') Nothing 0
    addLocal name alloca
    case initVal of
        (ASTNode ASTNothing _) -> pure(None)
        val -> do
            x <- codegenNode val
            case x of
                None -> error "unexpected node in variable decl" -- todo: proper error
                Some (v) -> L.store alloca 0 v
            pure (None)

codegenNode (ASTNode (ASTIfStatement cond body (ASTNode ASTNothing _)) _) = mdo
    codegenNodeConditional cond trueBB mergeBB
    trueBB <- L.named L.block "true"
    codegenNode body
    codegenTerm $ L.br mergeBB
    mergeBB <- L.named L.block "merge"
    pure(None)

codegenNode (ASTNode (ASTIfStatement cond body elseBody) _) = mdo
    codegenNodeConditional cond trueBB falseBB
    trueBB <- L.named L.block "true"
    codegenNode body
    codegenTerm $ L.br mergeBB
    falseBB <- L.named L.block "false"
    codegenNode elseBody
    codegenTerm $ L.br mergeBB
    mergeBB <- L.named L.block "merge"
    pure(None)

codegenNode (ASTNode (ASTWhileStatement cond body) _) = mdo
    scope <- gets locals
    bTo <- gets breakTo
    cTo <- gets continueTo

    L.br startBB
    startBB <- L.named L.block "start"
    codegenNodeConditional cond loopBB endBB
    loopBB <- L.named L.block "loop"
    modify $ \env -> env { breakTo = endBB, continueTo = startBB }
    codegenNode body
    codegenTerm $ L.br startBB
    endBB <- L.named L.block "end"
    modify $ \env -> env { locals = scope, breakTo = bTo, continueTo = cTo }
    pure(None)

codegenNode (ASTNode (ASTForStatement init cond iter body) _) = mdo
    scope <- gets locals
    bTo <- gets breakTo
    cTo <- gets continueTo

    _ <- codegenNode init
    L.br condBB
    condBB <- L.named L.block "cond"
    case cond of
        (ASTNode ASTNothing _) -> mdo
            L.br loopBB
            loopBB <- L.named L.block "loop"
            modify $ \env -> env { breakTo = endBB, continueTo = iterBB }
            _ <- codegenNode body
            codegenTerm $ L.br iterBB
            iterBB <- L.named L.block "iter"
            _ <- codegenNode iter
            codegenTerm $ L.br loopBB
            endBB <- L.named L.block "end"
            modify $ \env -> env { locals = scope, breakTo = bTo, continueTo = cTo }
            pure(None)

        c -> mdo
            codegenNodeConditional c loopBB endBB
            loopBB <- L.named L.block "loop"
            modify $ \env -> env { breakTo = endBB, continueTo = iterBB }
            _ <- codegenNode body
            codegenTerm $ L.br iterBB
            iterBB <- L.named L.block "iter"
            _ <- codegenNode iter
            codegenTerm $ L.br condBB
            endBB <- L.named L.block "end"
            modify $ \env -> env { locals = scope, breakTo = bTo, continueTo = cTo }
            pure (None)

codegenNode (ASTNode ASTBreakStatement _) = do
    bTo <- gets breakTo
    codegenTerm $ L.br bTo
    pure (None)

codegenNode (ASTNode ASTContinueStatement _) = do
    cTo <- gets continueTo
    codegenTerm $ L.br cTo
    pure (None)

codegenNode (ASTNode (ASTSwitchStatement value cases) _) = mdo
    bTo <- gets breakTo
    val <- codegenNode value
    modify $ \env -> env { breakTo = mergeBB }
    L.br firstCond
    (firstCond, firstCase) <- emitCases cases mergeBB mergeBB val
    modify $ \env -> env { breakTo = bTo }
    mergeBB <- L.named L.block "merge"
    pure(None)

    where
        emitCases [] m m' val = do
            pure (m, m')
        emitCases ((SwitchCase Default body):xs) m m' val = mdo
            (nextCond, nextCase) <- emitCases xs caseBB m' val -- override the merge block with the default case
            caseBB <- L.named L.block "default"
            mapM_ codegenNode body
            codegenTerm $ L.br nextCase
            pure (nextCond, caseBB)

        emitCases ((SwitchCase (Valued cond) body):xs) m m' val = mdo
            (nextCond, nextCase) <- emitCases xs m m' val
            condBB <- L.named L.block "caseCond"
            c <- codegenNode cond
            cmp <- L.icmp L.EQ (force val) (force c)
            L.condBr cmp caseBB nextCond
            caseBB <- L.named L.block "case"
            mapM_ codegenNode body
            codegenTerm $ L.br nextCase
            pure (condBB, caseBB)
     
codegenNode (ASTNode (ASTCompoundStatement body) _) = do
    oldScope <- gets locals
    mapM_ codegenNode body
    modify $ \env -> env { locals = oldScope }
    pure (None)

codegenNode (ASTNode (ASTIntegerLiteral value) CharType)  = pure (Some(L.int8  (fromIntegral value)))
codegenNode (ASTNode (ASTIntegerLiteral value) ShortType) = pure (Some(L.int32 (fromIntegral value)))
codegenNode (ASTNode (ASTIntegerLiteral value) IntType)   = pure (Some(L.int32 (fromIntegral value)))
codegenNode (ASTNode (ASTIntegerLiteral value) LongType)  = pure (Some(L.int64 (fromIntegral value)))

codegenNode (ASTNode (ASTStringLiteral value) ty') = do
    let value' = value ++ "\0"
    name1 <- getUniqName
    name2 <- getUniqName
    s <- L.globalStringPtr value' (Name name1)
    glob <- L.global (Name name2) (AST.ptr AST.i8) s
    load' <- L.load glob 0
    pure (Some(load'))

codegenNode (ASTNode (ASTVariableExpression name) ty') = do
    var <- getLocal name
    case var of
        None -> do
            c <- Codegen.getConst name
            pure (Some(c))
        Some (v) -> do
            l <- L.load v 0
            pure(Some(l))

codegenNode (ASTNode (ASTBinaryExpression l BinaryAssign r) ty') = do
    left <- codegenNodeLVal l
    right <- codegenNode r
    case right of
        None -> error "unexpected node in binary expression" -- todo: better error
        Some (right') -> do
            L.store left 0 right'
            pure(None) -- Maybe create a load?

codegenNode (ASTNode (ASTBinaryExpression l BinaryIndex r) ty') = do
    case (ty l) of
        (Types.PointerType _) -> do
            left <- codegenNode l
            right <- codegenNode r
            gep <- L.gep (force left) [force right]
            load <- L.load gep 0
            pure(Some(load))

        (Types.ArrayType _ _) -> do
            left <- codegenNodeLVal l
            right <- codegenNode r
            gep <- L.gep left [force right, (L.int32 0)]
            load <- L.load gep 0
            pure(Some(load))

codegenNode (ASTNode (ASTBinaryExpression l op r) ty') = do
    left <- codegenNode l
    right <- codegenNode r
    case left of
        None -> error "unexpected node in binary expression" -- todo: better error
        Some (left') -> do
            case right of
                None -> error "unexpected node in binary expression" -- todo: better error
                Some (right') -> do
                    case op of
                        BinaryAdd -> do
                            op' <- if (isPointerType $ ty l) then L.gep left' [right']
                            else if (isPointerType $ ty r) then L.gep right' [left']
                            else L.add left' right'
                            pure(Some(op'))
                        BinaryAddAssign -> do
                            op' <- if (isPointerType $ ty l) then L.gep left' [right']
                            else L.add left' right'
                            loc <- codegenNodeLVal l
                            _ <- L.store loc 0 op'
                            pure(None)
                        BinarySubAssign -> do
                            op' <- L.sub left' right'
                            loc <- codegenNodeLVal l
                            _ <- L.store loc 0 op'
                            pure(None)
                        BinarySub -> do
                            op' <- L.sub left' right'
                            pure(Some(op'))
                        BinaryMul -> do
                            op' <- L.mul left' right'
                            pure(Some(op'))
                        BinaryDiv -> do
                            op' <- L.sdiv left' right'
                            pure(Some(op'))

                        BinaryEqual -> do
                            op' <- L.icmp L.EQ left' right'
                            pure(Some(op'))
                        BinaryNotEqual -> do
                            op' <- L.icmp L.NE left' right'
                            pure(Some(op'))
                        BinaryLessThan -> do
                            op' <- L.icmp L.SLT left' right'
                            pure(Some(op'))
                        BinaryGreaterThan -> do
                            op' <- L.icmp L.SGT left' right'
                            pure(Some(op'))
                        BinaryLessEqual -> do
                            op' <- L.icmp L.SLE left' right'
                            pure(Some(op'))
                        BinaryGreaterEqual -> do
                            op' <- L.icmp L.SGE left' right'
                            pure(Some(op'))

codegenNode (ASTNode (ASTCallExpression c params) ty) = do
    case c of
        (ASTNode (ASTVariableExpression ident) ty') -> do
            callee <- getLocal ident
            params' <- mapM makeParam params
            call' <- L.call (force callee) params'
            pure(Some(call'))
        _ -> error "unimplemented call expression"

        where
            makeParam p = do
                val <- codegenNode p
                case val of
                    None -> error "Error" -- todo: better error
                    Some x -> pure (x,[])

codegenNode (ASTNode (ASTUnaryExpression operator operand) ty') = do
    case operator of
        UnaryRef -> do
            lval <- codegenNodeLVal operand
            pure (Some lval)

        UnaryIndirect -> do
            lval <- codegenNode operand
            case lval of
                None -> error "Error"
                Some (lval') -> do
                    load' <- L.load lval' 0
                    pure (Some(load'))

        UnaryMinus -> do
            op <- codegenNode operand
            neg <- L.sub (L.int32 0) (force op)
            pure (Some(neg))

        PrefixInc -> do
            op <- codegenNode operand
            inc <- L.add (force op) (L.int32 1)
            lval <- codegenNodeLVal operand
            L.store lval 0 inc
            pure (Some(inc))

        PrefixDec -> do
            op <- codegenNode operand
            dec <- L.sub (force op) (L.int32 1)
            lval <- codegenNodeLVal operand
            L.store lval 0 dec
            pure (Some(dec))

        PostfixInc -> do
            op <- codegenNode operand
            inc <- L.add (force op) (L.int32 1)
            lval <- codegenNodeLVal operand
            L.store lval 0 inc
            pure (op)

        PostfixDec -> do
            op <- codegenNode operand
            dec <- L.sub (force op) (L.int32 1)
            lval <- codegenNodeLVal operand
            L.store lval 0 dec
            pure (op)

        _ -> error $ "unimplemented unary operator " ++ show operator

codegenNode (ASTNode (ASTSizeofType ty) _) = pure $ Some(L.int32 $ fromIntegral(typeBytes ty))
codegenNode (ASTNode (ASTSizeofExpression e) _) = pure $ Some(L.int32 $ fromIntegral (typeBytes (ty e)))

codegenNode (ASTNode ASTNullptr ty) = do
    cast <- L.inttoptr (L.int64 0) (typeToLLVM ty)
    pure (Some(cast))

codegenNode (ASTNode (ASTMemberAccess struct id isPointer) ty') = do
    z <- codegenNodeLVal $ ASTNode (ASTMemberAccess struct id isPointer) ty'
    load <- L.load z 0
    pure (Some(load))

codegenNode (ASTNode (ASTNothing) _) = pure(None)

-- Y -> void (discards the result of the expression)
codegenNode (ASTNode (ASTCastExpression val Types.VoidType) _) = do
    _ <- codegenNode val
    pure (None)

-- Y -> iX
codegenNode (ASTNode (ASTCastExpression val toType) _) | isIntegerType toType = do
    val' <- codegenNode val
    let fromType = ty val
    if toType == fromType then pure val'
    else if (isPointerType fromType) then do
        cast <- L.ptrtoint (force val') (typeToLLVM toType)
        pure (Some(cast))
    else if (typeSize toType) > (typeSize fromType) then do
        sext <- L.sext (force val') (typeToLLVM toType)
        pure (Some(sext))
    else do
        trunc <- L.trunc (force val') (typeToLLVM toType)
        pure (Some(trunc))

-- Y -> X*
codegenNode (ASTNode (ASTCastExpression val toType) _) | isPointerType toType = do
    val' <- codegenNode val
    let fromType = ty val
    if toType == fromType then pure val'
    else if (isPointerType fromType) then do
        cast <- L.bitcast (force val') (typeToLLVM toType)
        pure (Some cast)
    else if (isIntegerType fromType) then do
        cast <- L.inttoptr (force val') (typeToLLVM toType)
        pure (Some(cast))
    else pure(None)