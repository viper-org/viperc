module Types where

import qualified LLVM.AST.Type as AST

data Type = VoidType | CharType | ShortType | IntType | LongType | PointerType Type
    deriving (Eq, Show)

typeToLLVM :: Type -> AST.Type
typeToLLVM VoidType  = AST.void
typeToLLVM CharType  = AST.i8
typeToLLVM ShortType = AST.i16
typeToLLVM IntType   = AST.i32
typeToLLVM LongType  = AST.i64
typeToLLVM (PointerType p) = AST.ptr $ typeToLLVM p

isObjectType :: Type -> Bool
isObjectType VoidType = False
isObjectType _ = True