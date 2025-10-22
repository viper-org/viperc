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
codegenFuncDef (FunctionDef returnType name args body isProto) = mdo
    addLocal name funct
    scope <- gets locals
    let parms = [generateParm type' name' | (type', name') <- args]
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

codegenNodeLVal :: ASTNode -> Builder CodegenOperand
codegenNodeLVal (ASTVariableExpression ident) = do
    var <- getLocal ident
    pure(Some(var))

codegenNodeLVal (ASTUnaryExpression UnaryIndirect op) = do
    codegenNode op

codegenNode :: ASTNode -> Builder CodegenOperand
codegenNode (ASTReturnStatement value) = case value of
    ASTNothing -> do
        L.retVoid
        pure(None)
    val -> do
        retVal <- codegenNode val
        case retVal of
            None -> error "unexpected node in return" -- todo: proper error
            Some (x) -> L.ret x
        pure (None)

codegenNode (ASTVariableDeclaration type' name initVal) = do
    alloca <- L.alloca (typeToLLVM type') Nothing 0
    addLocal name alloca
    case initVal of
        ASTNothing -> pure(None)
        val -> do
            x <- codegenNode val
            case x of
                None -> error "unexpected node in variable decl" -- todo: proper error
                Some (v) -> L.store alloca 0 v
            pure (None)

codegenNode (ASTIntegerLiteral value) = pure (Some(L.int32 (fromIntegral value)))

codegenNode (ASTStringLiteral value) = do
    let value' = value ++ "\0"
    name1 <- getUniqName
    name2 <- getUniqName
    s <- L.globalStringPtr value' (Name name1)
    glob <- L.global (Name name2) (AST.ptr AST.i8) s
    load' <- L.load glob 0
    pure (Some(load'))

codegenNode (ASTVariableExpression name) = do
    var <- getLocal name
    l <- L.load var 0
    pure(Some(l))

codegenNode (ASTBinaryExpression l BinaryAssign r) = do
    left <- codegenNodeLVal l
    right <- codegenNode r
    case left of
            None -> error "unexpected node in binary expression" -- todo: better error
            Some (left') -> do
                case right of
                    None -> error "unexpected node in binary expression" -- todo: better error
                    Some (right') -> do
                        L.store left' 0 right'
                        pure(None) -- Maybe create a load?

codegenNode (ASTBinaryExpression l op r) = do
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
                                op' <- L.add left' right'
                                pure(Some(op'))
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

codegenNode (ASTCallExpression c params) = do
    case c of
        (ASTVariableExpression ident) -> do
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

codegenNode (ASTUnaryExpression operator operand) = do
    case operator of
        UnaryRef -> do
            codegenNodeLVal operand

        UnaryIndirect -> do
            lval <- codegenNode operand
            case lval of
                None -> error "Error"
                Some (lval') -> do
                    load' <- L.load lval' 0
                    pure (Some(load'))
        _ -> error $ "unimplemented unary operator " ++ show operator