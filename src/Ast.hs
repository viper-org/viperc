module AST where

import Types

type Name = String
type ReturnValue = ASTNode
type InitialValue = ASTNode
type Left = ASTNode
type Right = ASTNode

data BinaryOperator = BinaryAdd | BinaryMul
    deriving (Eq, Show)

data ASTNode = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Type Name InitialValue
             | ASTIntegerLiteral Int
             | ASTVariableExpression Name
             | ASTBinaryExpression Left BinaryOperator Right
             deriving (Eq, Show)

data FunctionDef = FunctionDef {
    returnType :: Type,
    name :: String,
    body :: [ASTNode]
} deriving (Eq, Show)