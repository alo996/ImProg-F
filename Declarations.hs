{- |
Module      : Declarations
Description : This module contains all type declarations and type class instantiations needed to tokenize, parse, compile and interpret F programs.
-}
{-# LANGUAGE NamedFieldPuns #-}
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

-- | All possible expressions are summed up in the 'Expr' type.
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

{- | An atomic expression is the most basic kind of expression. It is either a name, a boolean value, an integer (with range [ -2^29, 2^29 - 1] at least) or an expression in paranthesis.
-}
data AtomicExpr
    = Var String
    | LitBool BoolF
    | LitNum Int
    | Expr Expr
    deriving Eq

instance Show Expr where
    show (Add e1 e2)           = show e1 ++ "+" ++ show e2
    show (AtomicExpr e)        = show e
    show (BinaryMin e1 e2)     = show e1 ++ " - " ++ show e2
    show (Div e1 e2)           = show e1 ++ "/" ++ show e2
    show (Equal e1 e2)         = show e1 ++ " == " ++ show e2
    show (Func e1 e2)          = show e1 ++ " " ++ show e2
    show (IfThenElse e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (LessThan e1 e2)      = show e1 ++ " < " ++ show e2
    show (LetIn e1 e2)         = "let " ++ show e1 ++ " in " ++ show e2
    show (LogicalAnd e1 e2)    = show e1 ++ " & " ++ show e2
    show (LogicalNot e)        = "!" ++ show e
    show (LogicalOr e1 e2)     = show e1 ++ " | " ++ show e2
    show (Mult e1 e2)          = show e1 ++ "*" ++ show e2
    show (UnaryMin e)          = "-" ++ show e

instance Show AtomicExpr where
    show (Var s)        = show s
    show (LitBool bool) = show bool
    show (LitNum n)     = show n
    show (Expr e)       = show e

---------------------------------------------------------------- MF-CODE GENERATION AND INTERPRETATION ------------------------------------------------------------
{- | The abstract machine contains four types of stores: 'Code' contains a translated program as a sequence of MF instructions, 'Stack' contains references to expressions that need further evaluation, 'Global' contains non-local function definitions and 'Heap' contains expressions represented graphs. 
-}
newtype Code = Code [Instruction]

newtype Stack = Stack [StackCell]

newtype Global = Global [(String, Int)]

newtype Heap = Heap [HeapCell]

-- | A heap is a list of heap cells. Heap cells either contain 
data HeapCell
    = APP Int Int
    | DEF
    {
        fname :: String,
        arity :: Int,
        caddr :: Int
    }
    | IND Int
    | PRE Keyword Int
    | UNINITIALIZED
    | VALBool Int
    | VALNum Int
    deriving Show

-- | A stack is a list of stack cells. Stack cells contain references to code cells or heap cells, each simply represented with an integer. 
newtype StackCell
    = StackCell Int
    deriving Show

-- | 'State' models the abstract machine. It contains a program counter, a stack pointer and the already introduced stores code, stack, global and heap. In case of a faulty execution, it can also take on an error state, that returns an error message to the user.
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

{- | MF takes as input a list of instructions to which an F program was translated into. 'Instruction' can take on values that each correspond to a certain functionality specified in MF.hs.
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
    | Pushpre Keyword
    | Pushval String Int
    | Reset
    | Return
    | Slide Int
    | SlideLet Int
    | Unwind
    | UpdateLet Int
    deriving (Eq, Show)

instance Show Code where
    show (Code ccells) = "Code: " ++ showCells ccells "c"

instance Show Stack where
    show (Stack scells) = "Stack: " ++ showCells scells "s"

instance Show Global where
    show (Global gcells) = "Global: " ++ showCells gcells "g"

instance Show Heap where
    show (Heap hcells) = "Heap: " ++ showCells hcells "h"

-- | 'showCells' is used to produce readable output of the stores used in MF.
showCells :: (Show a) => [a] -> String -> String
showCells xs prefix = showCells' xs 0 "" 
  where
    showCells' :: (Show a) => [a] -> Int -> String -> String
    showCells' (x : xs) n acc = showCells' xs (n + 1) (acc ++ "\n   " ++ prefix ++ show n ++ ": " ++ show x)
    showCells' [] _ acc       = acc

instance Show State where
    show State{pc, sp, code = Code ccells, stack, global, heap} = "+———----+\n| State |\n+———----+\n" ++ "I:  " ++ show (ccells !! pc) ++ "\nSP: " ++ show sp ++ "\nPC: " ++ show pc ++ "\n" ++ show ccells ++ "\n" ++ show stack ++ "\n" ++ show heap ++ "\n" ++ show global
    show (ErrorState error)                                     = error