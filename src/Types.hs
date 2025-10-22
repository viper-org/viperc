module Types where

import qualified LLVM.AST.Type as AST

type ReturnType = Type
type ParameterType = Type

data Type = VoidType | CharType | ShortType | IntType | LongType | BoolType
          | PointerType Type | FunctionType ReturnType [ParameterType]
    deriving (Eq, Show)

typeToLLVM :: Type -> AST.Type
typeToLLVM VoidType  = AST.void
typeToLLVM CharType  = AST.i8
typeToLLVM ShortType = AST.i16
typeToLLVM IntType   = AST.i32
typeToLLVM LongType  = AST.i64
typeToLLVM BoolType  = AST.i1
typeToLLVM (PointerType p) = AST.ptr $ typeToLLVM p
typeToLLVM (FunctionType r ps) = AST.FunctionType (typeToLLVM r) [typeToLLVM p | p <- ps] False

isIntegerType :: Type -> Bool
isIntegerType CharType = True
isIntegerType ShortType = True
isIntegerType IntType = True
isIntegerType LongType = True
isIntegerType _ = False

isPointerType :: Type -> Bool
isPointerType (PointerType _) = True
isPointerType _ = False

isObjectType :: Type -> Bool
isObjectType VoidType = False
isObjectType (FunctionType _ _) = False
isObjectType _ = True

getReturnType :: Type -> Type
getReturnType (FunctionType r _) = r
getReturnType x = error $ "Attempt to get return type of non-function type " ++ show x


prettyPrint :: Type -> String
prettyPrint CharType = "char"
prettyPrint ShortType = "short"
prettyPrint IntType = "int"
prettyPrint LongType = "long"
prettyPrint VoidType = "void"
prettyPrint BoolType = "bool"
prettyPrint (PointerType p) = (prettyPrint p) ++ "*"
prettyPrint (FunctionType r ps) = (prettyPrint r) ++ (foldl (++) "(" [prettyPrint p | p <- ps]) ++ ")"