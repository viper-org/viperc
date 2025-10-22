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
data Scope = Scope { locals :: Map String AST.Operand, uniqueID :: Int }

type LLVM = L.ModuleBuilderT (State Scope)

type Builder = L.IRBuilderT LLVM

data CodegenOperand = None | Some (AST.Operand)

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

getLocal :: String -> Builder AST.Operand
getLocal s = do
    vars <- gets locals
    case Map.lookup s vars of
        Just var -> pure var
        Nothing -> error ("'" ++ s ++ "' does not exist in this scope") -- todo: proper error

mTypeToLLVM :: MonadState Scope m => Types.Type -> m AST.Type
mTypeToLLVM ty = pure $ typeToLLVM ty

codegenFile :: [FunctionDef] -> AST.Module
codegenFile decls =
    flip evalState (Scope{locals = Map.empty, uniqueID = 0}) $
        L.buildModuleT "hello.c" $ do
            mapM_ codegenFuncDef decls

codegenFuncDef :: FunctionDef -> LLVM ()
codegenFuncDef (FunctionDef typ name args body isProto) = mdo
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
    pure(var)

codegenNodeLVal (ASTNode (ASTUnaryExpression UnaryIndirect op) _) = do
    z <- codegenNode op
    case z of
        None -> error "!"
        (Some x) -> pure x

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
    cond' <- codegenNode cond
    case cond' of
        None -> error $ "!"
        Some c -> mdo
            L.condBr c trueBB mergeBB
            trueBB <- L.named L.block "true"
            codegenNode body
            L.br mergeBB
            mergeBB <- L.named L.block "merge"
            pure(None)

codegenNode (ASTNode (ASTIfStatement cond body elseBody) _) = mdo
    cond' <- codegenNode cond
    case cond' of
        None -> error $ "!"
        Some c -> mdo
            L.condBr c trueBB falseBB
            trueBB <- L.named L.block "true"
            codegenNode body
            L.br mergeBB
            falseBB <- L.named L.block "false"
            codegenNode elseBody
            L.br mergeBB
            mergeBB <- L.named L.block "merge"
            pure(None)

codegenNode (ASTNode (ASTWhileStatement cond body) _) = mdo
    L.br startBB
    startBB <- L.named L.block "start"
    cond' <- codegenNode cond
    case cond' of
        None -> error $ "!"
        Some c -> mdo
            L.condBr c loopBB endBB
            loopBB <- L.named L.block "loop"
            codegenNode body
            L.br startBB
            endBB <- L.named L.block "end"
            pure(None)

codegenNode (ASTNode (ASTForStatement init cond iter body) _) = mdo
    _ <- codegenNode init
    L.br condBB
    condBB <- L.named L.block "cond"
    cond' <- codegenNode cond
    case cond' of
        None -> mdo
            L.br loopBB
            loopBB <- L.named L.block "loop"
            _ <- codegenNode body
            _ <- codegenNode iter
            L.br loopBB
            endBB <- L.named L.block "end"
            pure(None)
            -- todo: add 
        Some c -> mdo
            L.condBr c loopBB endBB
            loopBB <- L.named L.block "loop"
            _ <- codegenNode body
            _ <- codegenNode iter
            L.br condBB
            endBB <- L.named L.block "end"
            pure (None)
     
codegenNode (ASTNode (ASTCompoundStatement body) _) = do
    oldScope <- gets locals
    mapM_ codegenNode body
    modify $ \env -> env { locals = oldScope }
    pure (None)

codegenNode (ASTNode (ASTIntegerLiteral value) ty') = pure (Some(L.int32 (fromIntegral value)))

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
    l <- L.load var 0
    pure(Some(l))

codegenNode (ASTNode (ASTBinaryExpression l BinaryAssign r) ty') = do
    left <- codegenNodeLVal l
    right <- codegenNode r
    case right of
        None -> error "unexpected node in binary expression" -- todo: better error
        Some (right') -> do
            L.store left 0 right'
            pure(None) -- Maybe create a load?

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

codegenNode (ASTNode (ASTCallExpression c params) ty) = do
    case c of
        (ASTNode (ASTVariableExpression ident) ty') -> do
            callee <- getLocal ident
            params' <- mapM makeParam params
            call' <- L.call callee params'
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
        _ -> error $ "unimplemented unary operator " ++ show operator

codegenNode (ASTNode (ASTNothing) _) = pure(None)