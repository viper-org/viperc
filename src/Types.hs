module Types where

import qualified LLVM.AST.Type as AST

data CastLevel = Implicit | Explicit | Disallowed
    deriving (Eq, Show)

type ReturnType = Type
type ParameterType = Type
type Length = Int

data Type = VoidType | CharType | ShortType | IntType | LongType | BoolType
          | PointerType Type | ArrayType Type Length
          | FunctionType ReturnType [ParameterType] | VarArgType
          | AliasType String Type
    deriving (Show)

instance Eq Type where
    (VoidType) == (VoidType) = True
    (CharType) == (CharType) = True
    (ShortType) == (ShortType) = True
    (IntType) == (IntType) = True
    (LongType) == (LongType) = True
    (BoolType) == (BoolType) = True
    (PointerType p) == (PointerType q) = p == q
    (ArrayType l1 n1) == (ArrayType l2 n2) = l1 == l2 && n1 == n2
    (FunctionType r ps) == (FunctionType s qs) = r == s && ps == qs
    (AliasType _ z) == (AliasType _ y) = z == y
    (AliasType _ z) == x = z == x
    _ == _ = False

-- From -> To
-- TODO: add more casts
castLevel :: Type -> Type -> CastLevel
castLevel x y | x == y = Implicit
castLevel x y | isIntegerType x && isIntegerType y = Implicit
castLevel (PointerType VoidType) y | isPointerType y = Implicit
castLevel x (PointerType VoidType) | isPointerType x = Implicit
castLevel VoidType _ = Disallowed
castLevel x y = Disallowed

isVarArgType :: Type -> Bool
isVarArgType VarArgType = True
isVarArgType _ = False


typeToLLVM :: Type -> AST.Type
typeToLLVM VoidType  = AST.void
typeToLLVM CharType  = AST.i8
typeToLLVM ShortType = AST.i16
typeToLLVM IntType   = AST.i32
typeToLLVM LongType  = AST.i64
typeToLLVM BoolType  = AST.i1
typeToLLVM (PointerType p) = AST.ptr $ typeToLLVM p
typeToLLVM (ArrayType p len) = AST.ArrayType (fromIntegral len) (typeToLLVM p)
typeToLLVM (FunctionType r ps) = AST.FunctionType (typeToLLVM r) [typeToLLVM p | p <- ps] (or [isVarArgType p | p <- ps])
typeToLLVM (AliasType _ z) = typeToLLVM z

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
typeSize VarArgType = 0
typeSize (AliasType _ z) = typeSize z

typeBytes :: Type -> Int
typeBytes ty = (typeSize ty) `div` 8

isIntegerType :: Type -> Bool
isIntegerType CharType = True
isIntegerType ShortType = True
isIntegerType IntType = True
isIntegerType LongType = True
isIntegerType (AliasType _ z) = isIntegerType z
isIntegerType _ = False

isArrayType :: Type -> Bool
isArrayType (ArrayType _ _) = True
isArrayType (AliasType _ z) = isArrayType z
isArrayType _ = False

isPointerType :: Type -> Bool
isPointerType (PointerType _) = True
isPointerType (AliasType _ z) = isPointerType z
isPointerType _ = False

isObjectType :: Type -> Bool
isObjectType VoidType = False
isObjectType (FunctionType _ _) = False
isObjectType VarArgType = False
isObjectType (AliasType _ z) = isObjectType z
isObjectType _ = True

getReturnType :: Type -> Type
getReturnType (FunctionType r _) = r
getReturnType (AliasType _ z) = getReturnType z
getReturnType x = error $ "Attempt to get return type of non-function type " ++ show x

getArgumentTypes :: Type -> [Type]
getArgumentTypes (FunctionType _ a) = a
getArgumentTypes (AliasType _ z) = getArgumentTypes z
getArgumentTypes x = error $ "Attempt to get argument types of non-function type " ++ show x

getPointeeType :: Type -> Type
getPointeeType (PointerType p) = p
getPointeeType (AliasType _ z) = getPointeeType z
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
prettyPrint VarArgType = "..."
prettyPrint (AliasType n _) = n