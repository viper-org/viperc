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
import qualified LLVM.AST.Type as AST
import LLVM.AST.Typed (typeOf)
import LLVM.IRBuilder.Module as L;
import LLVM.IRBuilder.Instruction as L;
import LLVM.IRBuilder.Monad as L;
import LLVM.IRBuilder.Constant as L;

-- todo: proper scope-level local variables
data Scope = Scope { locals :: Map String AST.Operand }

type LLVM = L.ModuleBuilderT (State Scope)

type Builder = L.IRBuilderT LLVM

data CodegenOperand = None | Some (AST.Operand)

addLocal :: MonadState Scope m => String -> AST.Operand -> m ()
addLocal name value = modify $ \env -> env { locals = Map.insert name value (locals env) }

getLocal :: String -> Builder AST.Operand
getLocal s = do
    vars <- gets locals
    case Map.lookup s vars of
        Just var -> pure var
        Nothing -> error ("'" ++ s ++ "' does not exist in this scope") -- todo: proper error

codegenFile :: [FunctionDef] -> AST.Module
codegenFile decls =
    flip evalState (Scope{locals = Map.empty}) $
        L.buildModuleT "hello.c" $ do
            mapM_ codegenFuncDef decls

codegenFuncDef :: FunctionDef -> LLVM ()
codegenFuncDef (FunctionDef returnType name body) = mdo
    scope <- get
    funct <- do
        L.function (AST.mkName $ cs name) [] (typeToLLVM returnType) emitBody
    put scope
    pure()

    where
        emitBody z = do
            _ <- L.block `L.named` "entry"
            mapM_ codegenNode body

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

codegenNode (ASTVariableExpression name) = do
    var <- getLocal name
    l <- L.load var 0
    pure(Some(l))

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
                            BinaryMul -> do
                                op' <- L.mul left' right'
                                pure(Some(op'))