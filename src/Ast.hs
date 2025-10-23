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
                    | BinaryEqual | BinaryNotEqual | BinaryLessThan | BinaryGreaterThan | BinaryLessEqual | BinaryGreaterEqual
                    | BinaryIndex
                    | LogicalAnd | LogicalOr
                    | BinaryAssign
                    | BinaryAddAssign | BinarySubAssign
    deriving (Eq, Show)

data UnaryOperator = UnaryRef | UnaryIndirect
                   | UnaryMinus | LogicalNot
                   | PrefixInc | PrefixDec | PostfixInc | PostfixDec
    deriving (Eq, Show)

data SwitchCaseValue = Valued ASTNode | Default
    deriving (Eq, Show)

data SwitchCase = SwitchCase {
    caseValue :: SwitchCaseValue,
    caseBody :: [ASTNode]
} deriving (Eq, Show)

data ASTNodeClass = ASTNothing
             | ASTReturnStatement ReturnValue
             | ASTVariableDeclaration Type String InitialValue
             | ASTIfStatement Condition Body ElseBody
             | ASTWhileStatement Condition Body
             | ASTForStatement Init Condition Iter Body
             | ASTSwitchStatement ASTNode [SwitchCase]
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
             | ASTSizeofExpression ASTNode
             | ASTSizeofType Type
             | ASTNullptr
             deriving (Eq, Show)

data ASTNode = ASTNode {
    clas :: ASTNodeClass,
    ty :: Type
} deriving (Eq, Show)

data ASTGlobal = ASTFunction FunctionDef
               | ASTGlobalVar Type String InitialValue
               | ASTEnum EnumDef
    deriving (Eq, Show)

data EnumDef = EnumDef {
    enumName :: String,
    enumBase :: Type,
    enumVals :: [(String, Int)]
} deriving (Eq, Show)

data FunctionDef = FunctionDef {
    fnType :: Type,
    fnName :: String,
    args :: [(Type, String)],
    body :: [ASTNode],
    prototype :: Bool
} deriving (Eq, Show)

isVariArg :: (Type, String) -> Bool
isVariArg (z, _) = isVarArgType z

hasVarArgs :: [(Type, String)] -> Bool
hasVarArgs args = or [isVariArg x | x <- args]