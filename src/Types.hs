module Types where

import qualified LLVM.AST.Type as AST

data CastLevel = Implicit | Explicit | Disallowed
    deriving (Eq, Show)

type ReturnType = Type
type ParameterType = Type
type Length = Int

data Type = VoidType | CharType | ShortType | IntType | LongType | BoolType
          | PointerType Type | ArrayType Type Length
          | FunctionType ReturnType [ParameterType]
    deriving (Eq, Show)

-- From -> To
-- TODO: add more casts
castLevel :: Type -> Type -> CastLevel
castLevel x y | x == y = Implicit
castLevel x y | isIntegerType x && isIntegerType y = Implicit
castLevel (PointerType VoidType) y | isPointerType y = Implicit
castLevel x (PointerType VoidType) | isPointerType x = Implicit
castLevel VoidType _ = Disallowed
castLevel x y = Disallowed

typeToLLVM :: Type -> AST.Type
typeToLLVM VoidType  = AST.void
typeToLLVM CharType  = AST.i8
typeToLLVM ShortType = AST.i16
typeToLLVM IntType   = AST.i32
typeToLLVM LongType  = AST.i64
typeToLLVM BoolType  = AST.i1
typeToLLVM (PointerType p) = AST.ptr $ typeToLLVM p
typeToLLVM (ArrayType p len) = AST.ArrayType (fromIntegral len) (typeToLLVM p)
typeToLLVM (FunctionType r ps) = AST.FunctionType (typeToLLVM r) [typeToLLVM p | p <- ps] False

typeSize :: Type -> Int
typeSize VoidType = 0
typeSize CharType = 8
typeSize ShortType = 16
typeSize IntType = 32
typeSize LongType = 64
typeSize BoolType = 1
typeSize (PointerType _) = 64
typeSize (ArrayType e n) = (typeSize e) * n
typeSize (FunctionType _ _) = 0

typeBytes :: Type -> Int
typeBytes ty = (typeSize ty) `div` 8

isIntegerType :: Type -> Bool
isIntegerType CharType = True
isIntegerType ShortType = True
isIntegerType IntType = True
isIntegerType LongType = True
isIntegerType _ = False

isArrayType :: Type -> Bool
isArrayType (ArrayType _ _) = True
isArrayType _ = False

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

getArgumentTypes :: Type -> [Type]
getArgumentTypes (FunctionType _ a) = a
getArgumentTypes x = error $ "Attempt to get argument types of non-function type " ++ show x

getPointeeType :: Type -> Type
getPointeeType (PointerType p) = p
getPointeeType x = error $ "Attempt to get pointee type of non-pointer type " ++ show x


prettyPrint :: Type -> String
prettyPrint CharType = "char"
prettyPrint ShortType = "short"
prettyPrint IntType = "int"
prettyPrint LongType = "long"
prettyPrint VoidType = "void"
prettyPrint BoolType = "bool"
prettyPrint (PointerType p) = (prettyPrint p) ++ "*"
prettyPrint (FunctionType r ps) = (prettyPrint r) ++ (foldl (++) "(" [prettyPrint p | p <- ps]) ++ ")"