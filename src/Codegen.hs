{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen where

import Parser

import Control.Applicative
import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as Map
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

data Scope = Scope {}

type LLVM = L.ModuleBuilderT (State Scope)

type Builder = L.IRBuilderT LLVM

data CodegenOperand = None | Some (AST.Operand)

codegenFile :: [FunctionDef] -> AST.Module
codegenFile decls =
    flip evalState (Scope{}) $
        L.buildModuleT "hello.c" $ do
            mapM_ codegenFuncDef decls

codegenFuncDef :: FunctionDef -> LLVM ()
codegenFuncDef (FunctionDef name body) = mdo
    funct <- do
        L.function (AST.mkName $ cs name) [] AST.i32 emitBody
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

codegenNode (ASTIntegerLiteral value) = pure (Some(L.int32 (fromIntegral value)))