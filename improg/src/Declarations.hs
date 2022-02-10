{- |
Module      : Declarations
Description : This module contains all type declarations and type class instantiations needed to tokenize, parse, compile and interpret F programs.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Declarations where


---------------------------------------- TOKEN GENERATION ----------------------------------------
-- | A token is a sequence of characters with some inherent meaning, in our case it is either a boolean value, a reserved F-keyword, a name, or a number.
data Token
    = BooleanToken BoolF
    | KeywordToken Keyword
    | NameToken String
    | NumberToken Int
    deriving (Eq, Show)

-- | 'BoolF' adresses the fact that boolean values in F are lowercase, unlike those in Haskell.
newtype BoolF 
    = BoolF Bool 
    deriving Eq

-- | A keyword is a sequence of characters reserved by F.
data Keyword
    = And
    | Assign
    | Divide
    | Else
    | Equals
    | If
    | In
    | Less
    | Let
    | LBracket
    | Minus
    | Not
    | Or
    | Plus
    | RBracket
    | Semicolon
    | Times
    | Then
    deriving Eq

instance Show BoolF where
    show (BoolF True)  = "true"
    show (BoolF False) = "false"

instance Show Keyword where
    show And       = "&"
    show Assign    = "="
    show Divide    = "/"
    show Else      = "else"
    show Equals    = "=="
    show If        = "if"
    show In        = "in"
    show Less      = "<"
    show Let       = "let"
    show LBracket  = "("
    show Minus     = "-"
    show Not       = "not"
    show Or        = "|"
    show Plus      = "+"
    show RBracket  = ")"
    show Semicolon = ";"
    show Times     = "*"
    show Then      = "then"


---------------------------------------- PARSING ----------------------------------------
{- | We define a parser to be a parametrized function that takes a list of tuples. These tuples are (token, integer)-pairs, assigning each identified token its line number in the source code for error handling purposes. This list of tuples is then mapped to either an error message if parsing was unsuccessful, or a tuple containing parsed output and a list of (token, integer)-pairs left to be parsed by another parser.
-}
type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

-- | According to the F grammar, a definition consists of a function name, a list of formal parameters, and its defining expression.
data Def
    = Def Expr [Expr] Expr
    deriving (Eq, Show)

-- | A local definition consists of a name and its defining expression.
data LocalDef
    = LocalDef Expr Expr
    deriving (Eq, Show)

-- | All possible expressions are defined in the 'Expr' type.
data Expr
    = Add Expr Expr
    | AtomicExpr AtomicExpr
    | BinaryMin Expr Expr
    | Div Expr Expr
    | Equal Expr Expr
    | Func Expr Expr
    | IfThenElse Expr Expr Expr
    | LessThan Expr Expr
    | LetIn [LocalDef] Expr
    | LogicalAnd Expr Expr
    | LogicalNot Expr
    | LogicalOr Expr Expr
    | Mult Expr Expr
    | UnaryMin Expr
    deriving Eq

{- | An atomic expression is the most basic kind of expression. It is either a name, a boolean value, an integer (with range [ -2^29, 2^29 - 1] at least), or an expression in parentheses.
-}
data AtomicExpr
    = Var String
    | LitBool BoolF
    | LitNum Int
    | Expr Expr
    deriving Eq

instance Show Expr where
    show (Add e1 e2)           = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (AtomicExpr e)        = show e
    show (BinaryMin e1 e2)     = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Div e1 e2)           = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
    show (Equal e1 e2)         = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (Func e1 e2)          = "(FuncApp (" ++ show e1 ++ ") (" ++ show e2 ++ "))"
    show (IfThenElse e1 e2 e3) = "if (" ++ show e1 ++ ") then (" ++ show e2 ++ ") else (" ++ show e3 ++ ")"
    show (LessThan e1 e2)      = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (LetIn e1 e2)         = "let " ++ show e1 ++ " in " ++ show e2
    show (LogicalAnd e1 e2)    = "(" ++ show e1 ++ " & " ++ show e2 ++ ")"
    show (LogicalNot e)        = "(Not " ++ show e ++ ")"
    show (LogicalOr e1 e2)     = "(" ++ show e1 ++ " | " ++ show e2 ++ ")"
    show (Mult e1 e2)          = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (UnaryMin e)          = "(Minus " ++ show e ++ ")"

instance Show AtomicExpr where
    show (Var s)        = "Var " ++ show s
    show (LitBool bool) = "Bool " ++ show bool
    show (LitNum n)     = "Num " ++ show n
    show (Expr e)       = show e


---------------------------------------- TRANSLATION AND INTERPRETATION OF F PROGRAMS ----------------------------------------
{- | The abstract machine contains four types of stores: 'Code' holds a translated program as a sequence of MF instructions, 'Stack' contains references to expressions that need further evaluation, 'Global' contains non-local function definitions and 'Heap' contains expressions represented as graphs.
-}
newtype Code = Code [Instruction]

newtype Stack = Stack [StackCell]

newtype Global = Global [(String, Int)]

newtype Heap = Heap [HeapCell]

-- | A heap is a list of typed heap cells.
data HeapCell
    = APP Int Int
    | DEF
    {
        fname :: String,
        arity :: Int,
        caddr :: Int
    }
    | IND Int
    | PRE Operator Int
    | UNINITIALIZED
    | VALBool Int
    | VALNum Int
    deriving Show

-- | A stack is a list of stack cells. Stack cells contain references to code cells or heap cells, each represented with an integer.
newtype StackCell
    = StackCell Int
    deriving Show

{- | 'State' models the abstract machine. It contains a program counter, a stack pointer, and the already introduced stores code, stack, global and heap. In case of a faulty execution, it can also take on an error state that returns an error message to the user.
-}
data State
    = State
    {
        pc :: Int,
        sp :: Int,
        code :: Code,
        stack :: Stack,
        global :: Global,
        heap :: Heap
    }
    | ErrorState String

{- | MF takes as input a list of instructions and interprets them. 'Instruction' can take on values that each correspond to a certain functionality specified in MF.hs.
-}
data Instruction
    = Alloc
    | Call
    | Error String
    | FuncUpdate Int
    | Halt
    | Makeapp
    | Operator Int
    | OpUpdate
    | Pushfun String
    | Pushparam Int
    | Pushpre Operator
    | Pushval String Int
    | Reset
    | Return
    | Slide Int
    | SlideLet Int
    | Unwind
    | UpdateLet Int
    deriving (Eq, Show)

data Operator
    = AndOp
    | BinaryMinOp
    | DivideOp
    | EqualsOp
    | IfOp
    | LessOp
    | NotOp
    | OrOp
    | PlusOp
    | TimesOp
    | UnaryMinOp
    deriving Eq

-- | Customized show functions are necessary to properly print interim states during execution.
instance Show Code where
    show (Code ccells) = "+--------------+\n| Instructions |\n+--------------+\n" ++ formatCells ccells "c"

instance Show Stack where
    show (Stack scells) = "Stack:\n" ++ formatCells scells "s"

instance Show Global where
    show (Global gcells) = "+--------------------+\n| Global environment |\n+--------------------+\n" ++ formatCells gcells "g"

instance Show Heap where
    show (Heap hcells) = "Heap:\n" ++ formatCells hcells "h"

-- | 'formatCells' is used to produce readable output of the used stores.
formatCells :: (Show a) => [a] -> String -> String
formatCells xs prefix = formatCells' xs 0 ""
  where
    formatCells' :: (Show a) => [a] -> Int -> String -> String
    formatCells' (x : xs) n acc = formatCells' xs (n + 1) (acc ++ prefix ++ show n ++ ": " ++ show x ++ "\n")
    formatCells' [] _ acc       = acc

instance Show State where
    show (ErrorState error)          = error
    show s@State{code = Code ccells} =
        "+-------+\n| State |\n+-------+\n" ++
        "I:  " ++ show (ccells !! pc s) ++
        "\nSP: " ++ show (sp s) ++
        "\nPC: " ++ show (pc s) ++
        "\n" ++ show (stack s) ++
        show (heap s)

instance Show Operator where
    show AndOp       = "&"
    show BinaryMinOp = "-"
    show DivideOp    = "/"
    show EqualsOp    = "=="
    show IfOp        = "if-then-else"
    show LessOp      = "<"
    show NotOp       = "not"
    show OrOp        = "|"
    show PlusOp      = "+"
    show TimesOp     = "*"
    show UnaryMinOp  = "-"
