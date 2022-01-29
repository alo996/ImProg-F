{-# LANGUAGE NamedFieldPuns #-}
-- This module contains all necessary types, aliases and type class instantiations.
module Declarations where

----------------------------------------------------------------------- LEXICAL ANALYSIS ------------------------------------------------------------------------
-- | A token is a sequence of characters with some inherent structure (e.g. a reserved F-keyword, a name, a multi-character number).
data Token
    = BooleanToken BoolF
    | KeywordToken Keyword
    | NameToken String
    | NumberToken Int
    deriving Eq

-- | A keyword is a sequence of characters that is reserved by F.
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

-- | 'BoolF' adresses the fact that boolean values in F are lowercase, unlike in Haskell.
newtype BoolF = BoolF Bool deriving Eq

instance Show Token where
    show (BooleanToken bool)    = show bool
    show (KeywordToken keyword) = show keyword
    show (NameToken name)       = show name
    show (NumberToken num)      = show num

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

instance Show BoolF where
    show (BoolF True)  = "true"
    show (BoolF False) = "false"

----------------------------------------------------------------------- SYNTACTICAL ANALYSIS ----------------------------------------------------------------------
{- | We define a parser to be a parametrized function that takes a list of tuples. These tuples are (token, integer)-pairs, assigning each identified token its line number in the source code for error handling purposes. This list of tuples is then mapped to either an error message if parsing was unsuccessful, or a tuple containing parsed output and a list of (token, integer)-pairs left to be parsed by another parser.
-}
type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

-- | According to the F grammar, a definition constitutes of a function name, a list of formal parameters and its defining expression.
data Def
    = Def Expr [Expr] Expr
    deriving (Eq, Show)

-- | A local definition consists of a name and its defining expression.
data LocalDef
    = LocalDef Expr Expr
    deriving (Eq, Show)

-- A lot of stuff can be an expression, for example '2 + 2'. All possible combinations are summed up in the Expr type.
data Expr
    = Add Expr Expr
    | Func Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | UnaryMin Expr
    | BinaryMin Expr Expr
    | Equal Expr Expr
    | LessThan Expr Expr
    | LogicalAnd Expr Expr
    | LogicalOr Expr Expr
    | LogicalNot Expr
    | LetIn [LocalDef] Expr
    | IfThenElse Expr Expr Expr
    | AtomicExpr AtomicExpr
    deriving Eq

instance Show Expr where
    show (Add e1 e2)           = show e1 ++ "+" ++ show e2
    show (Func e1 e2)          = show e1 ++ " " ++ show e2
    show (Mult e1 e2)          = show e1 ++ "*" ++ show e2
    show (Div e1 e2)           = show e1 ++ "/" ++ show e2
    show (UnaryMin e)          = "-" ++ show e
    show (BinaryMin e1 e2)     = show e1 ++ " - " ++ show e2
    show (Equal e1 e2)         = show e1 ++ " == " ++ show e2
    show (LessThan e1 e2)      = show e1 ++ " < " ++ show e2
    show (LogicalAnd e1 e2)    = show e1 ++ " & " ++ show e2
    show (LogicalOr e1 e2)     = show e1 ++ " | " ++ show e2
    show (LogicalNot e)        = "!" ++ show e
    show (LetIn e1 e2)         = "let " ++ show e1 ++ " in " ++ show e2
    show (IfThenElse e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (AtomicExpr e)        = show e

-- An atomic expression is the most basic kind of expression. It is either a name, a numeral or a boolean.
data AtomicExpr
    = Var String
    | LitBool BoolF
    | LitNum Int
    | Expr Expr
    deriving Eq

instance Show AtomicExpr where
    show (Var s)                = show s
    show (LitBool (BoolF bool)) = show bool
    show (LitNum n)             = show n
    show (Expr e)               = show e

type Var = String

---------------------------------------- MF-CODE GENERATION AND INTERPRETATION ----------------------------------------
{-
We have four kinds of stores to deal with: code (for instructions), stack (for references),
heap (for applications) and global (for definitions of non-local functions).
-}

newtype Code = Code [Instruction] deriving Eq

instance Show Code where
    show (Code ccells)   = "Code: " ++ showCells ccells "c"

newtype Stack = Stack [StackCell] deriving Eq

instance Show Stack where
    show (Stack scells)  = "Stack: " ++ showCells scells "s"

newtype Global = Global [(String, Int)] deriving Eq
instance Show Global where
    show (Global gcells) = "Global: " ++ showCells gcells "g"

newtype Heap = Heap [HeapCell] deriving Eq

instance Show Heap where
    show (Heap hcells)   = "Heap: " ++ showCells hcells "h"

showCells :: (Show a) => [a] -> String -> String
showCells xs prefix = showCells' xs 0 "" where
    showCells' (x : xs) ind acc = showCells' xs (ind + 1) (acc ++ "\n   " ++ prefix ++ show ind ++ ": " ++ show x)
    showCells' [] _ acc         = acc

{-
A machine state consists of a program counter, a list of generated instructions, a stack, a heap and a global environment (see Zhu's example).
Do we also need a stack pointer? If so, why didn't Zhu include one?
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

instance Show State where
    show State{pc, sp, code = Code ccells, stack, global, heap} = "+———----+\n| State |\n+———----+\n" ++ "I:  " ++ show (ccells !! pc) ++ "\nSP: " ++ show sp ++ "\nPC: " ++ show pc ++ "\n" ++ show ccells ++ "\n" ++ show stack ++ "\n" ++ show heap ++ "\n" ++ show global
    show (ErrorState error)                                     = error


-- The instruction type has several constructors, each one indicating some kind of functionality.
data Instruction
    = Reset
    | Pushfun String
    | Pushval String Int
    | Pushparam Int
    | Pushpre Keyword
    | Makeapp
    | Slide Int
    | Unwind
    | Call
    | Return
    | Halt
    | Operator Int
    | Alloc
    | FuncUpdate Int
    | OpUpdate
    | UpdateLet Int
    | SlideLet Int
    | Error String
    deriving (Eq, Show)

{-A heap is a list of heap cells.
As the global environment is not changed during runtime, it can be stored at one end of the data store consisting of stack, heap and global environment after compiling.
-}
data HeapCell
    = APP Int Int
    | VALNum Int
    | VALBool Int
    | DEF
    {
        fname :: String,
        arity :: Int,
        caddr :: Int
    }
    | IND Int
    | PRE Keyword Int
    | UNINITIALIZED
    deriving (Show, Eq)

-- A stack is a list of stack cells.
newtype StackCell
    = StackCell Int
    deriving (Show, Eq)