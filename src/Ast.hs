module AST where

import Types

type ReturnValue = ASTNode
type InitialValue = ASTNode
type Condition = ASTNode
type Body = ASTNode
type ElseBody = ASTNode
type Init = ASTNode
type Iter = ASTNode
type Left = ASTNode
type Right = ASTNode
type Callee = ASTNode
type Param = ASTNode

data BinaryOperator = BinaryAdd | BinarySub | BinaryMul | BinaryDiv
                    | BinaryEqual | BinaryNotEqual
                    | BinaryAssign
                    | BinaryAddAssign | BinarySubAssign
    deriving (Eq, Show)

data UnaryOperator = UnaryRef | UnaryIndirect
    deriving (Eq, Show)

data ASTNodeClass = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Type String InitialValue
             | ASTIfStatement Condition Body ElseBody
             | ASTWhileStatement Condition Body
             | ASTForStatement Init Condition Iter Body
             | ASTBreakStatement
             | ASTContinueStatement
             | ASTCompoundStatement [ASTNode]
             | ASTIntegerLiteral Int
             | ASTStringLiteral String
             | ASTVariableExpression String
             | ASTBinaryExpression Left BinaryOperator Right
             | ASTUnaryExpression UnaryOperator ASTNode
             | ASTCallExpression Callee [Param]
             | ASTCastExpression ASTNode Type
             deriving (Eq, Show)

data ASTNode = ASTNode {
    clas :: ASTNodeClass,
    ty :: Type
} deriving (Eq, Show)

data ASTGlobal = ASTFunction FunctionDef
               | ASTGlobalVar Type String InitialValue
    deriving (Eq, Show)

data FunctionDef = FunctionDef {
    fnType :: Type,
    name :: String,
    args :: [(Type, String)],
    body :: [ASTNode],
    prototype :: Bool
} deriving (Eq, Show)