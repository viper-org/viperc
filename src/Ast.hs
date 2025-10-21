module AST where

import Types

type Name = String
type ReturnValue = ASTNode
type InitialValue = ASTNode
type Left = ASTNode
type Right = ASTNode
type Callee = ASTNode

data BinaryOperator = BinaryAdd | BinarySub | BinaryMul | BinaryDiv
                    | BinaryAssign
    deriving (Eq, Show)

data ASTNode = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Type Name InitialValue
             | ASTIntegerLiteral Int
             | ASTVariableExpression Name
             | ASTBinaryExpression Left BinaryOperator Right
             | ASTCallExpression Callee
             deriving (Eq, Show)

data FunctionDef = FunctionDef {
    returnType :: Type,
    name :: String,
    args :: [(Type, String)],
    body :: [ASTNode]
} deriving (Eq, Show)