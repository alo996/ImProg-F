{-# LANGUAGE NamedFieldPuns #-}
-- This module contains all necessary types, aliases and type class instantiations.
module Declarations where

    ---------------------------------------- LEXICAL ANALYSIS ----------------------------------------
    {-
    A token (or symbol) is a sequence of characters with some inherent structure
    (e.g. a reserved F-keyword, a name, a multi-character number).
    -}
    data Token
        = BooleanToken BoolF
        | KeywordToken Keyword
        | NameToken String
        | NumberToken Int
        deriving Eq

    {-
    A keyword is a sequence of characters that is 'reserved' by F. It cannot be used as a name, because F assumes it has a
    predefined meaning (e.g. '+' is meant to be addition).
    -}
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

    -- BoolF adresses the fact that boolean values in F are lowercase.
    newtype BoolF = BoolF Bool deriving Eq

    -- We need customized representations of our tokens, depending on their type.
    instance Show Token where
        show (BooleanToken bool)    = show bool
        show (KeywordToken keyword) = show keyword
        show (NameToken name)       = show name
        show (NumberToken num)      = show num

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


    ---------------------------------------- SYNTACTICAL ANALYSIS ----------------------------------------
    {-
    A parser is a parametrized function, that takes a list of tuples.
    Each tuple contains a token and its line in the source code for error-handling purposes (e.g. [(NameToken f, 1), (NameToken x, 1)]).
    This list of tuples, that represents the tokenized source code, will then be mapped to either an error message (the 'Left' case) if the source code makes no sense,
    or it will be mapped to a tuple, containing the grammatical equivalent of some tokens, and the rest of the tokenized input (the 'Right' case,
    e.g. "expr8 $ tokenize "y * 8; f x = 3;" --> Right (AtomicExpr (Var "y"),[(*,1),(8,1),(;,1),("f",1),("x",1),(=,1),(3,1),(;,1)]))
    -}
    type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

    data Def
        = Def Expr [Expr] Expr
        deriving (Eq, Show)

    -- A local definition consists of two expressions.
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
        show (Add e1 e2)           = show e1 ++ " + " ++ show e2
        show (Func e1 e2)          = "Func " ++ show e1 ++ " " ++ show e2
        show (Mult e1 e2)          = show e1 ++ " * " ++ show e2
        show (Div e1 e2)           = show e1 ++ " / " ++ show e2
        show (UnaryMin e)          = "UnaryMin " ++ show e
        show (Equal e1 e2)         = show e1 ++ " == " ++ show e2
        show (LessThan e1 e2)      = show e1 ++ " < " ++ show e2
        show (LogicalAnd e1 e2)    = show e1 ++ " & " ++ show e2
        show (LogicalOr e1 e2)     = show e1 ++ " | " ++ show e2
        show (LogicalNot e)        = " ! " ++ show e
        show (LetIn e1 e2)         = "let" ++ show e1 ++ " in " ++ show e2
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
    data Store a
        = Code [a]
        | Stack [a]
        | Heap [a]
        deriving Eq

    instance (Show a) => Show (Store a) where
      show (Code ccells)  = "Code: " ++ showCells ccells
      show (Stack scells) = "Stack: " ++ showCells scells
      show (Heap hcells)  = "Heap: " ++ showCells hcells

    showCells :: (Show a) => [a] -> String
    showCells xs = showCells' xs 0 "" where
      showCells' (x : xs) ind acc = showCells' xs (ind + 1) (acc ++ "\n   " ++ show ind ++ ": " ++ show x)
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
            code :: Store Instruction,
            stack :: Store StackCell,
            heap :: Store HeapCell
        }
        | ErrorState String

    instance Show State where
        show State{pc, sp, code, stack, heap} = show code ++ "\nSP: " ++ show sp ++ "\nPC: " ++ show pc ++ "\n" ++ show stack ++ "\n" ++ show heap
        show (ErrorState error)               = error 

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
        deriving Show

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
