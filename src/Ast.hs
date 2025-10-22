module AST where

import Types

type Name = String
type ReturnValue = ASTNode
type InitialValue = ASTNode
type Condition = ASTNode
type Body = ASTNode
type ElseBody = ASTNode
type Left = ASTNode
type Right = ASTNode
type Callee = ASTNode
type Param = ASTNode

data BinaryOperator = BinaryAdd | BinarySub | BinaryMul | BinaryDiv
                    | BinaryEqual | BinaryNotEqual
                    | BinaryAssign
    deriving (Eq, Show)

data UnaryOperator = UnaryRef | UnaryIndirect
    deriving (Eq, Show)

data ASTNodeClass = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Type Name InitialValue
             | ASTIfStatement Condition Body ElseBody
             | ASTWhileStatement Condition Body
             | ASTCompoundStatement [ASTNode]
             | ASTIntegerLiteral Int
             | ASTStringLiteral String
             | ASTVariableExpression Name
             | ASTBinaryExpression Left BinaryOperator Right
             | ASTUnaryExpression UnaryOperator ASTNode
             | ASTCallExpression Callee [Param]
             deriving (Eq, Show)

data ASTNode = ASTNode {
    clas :: ASTNodeClass,
    ty :: Type
} deriving (Eq, Show)

data FunctionDef = FunctionDef {
    fnType :: Type,
    name :: String,
    args :: [(Type, String)],
    body :: [ASTNode],
    prototype :: Bool
} deriving (Eq, Show)