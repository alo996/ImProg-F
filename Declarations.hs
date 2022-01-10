-- This module contains all necessary types, aliases and type class instantiations.
module Declarations where 
    -- LEXICAL ANALYSIS
    {-
    A token (or symbol) is a sequence of characters with some inherent structure 
    (e.g. a reserved F-keyword, a name, a multi-character number).
    -}
    data Token 
        = BooleanToken BoolF 
        | KeywordToken Keyword 
        | NameToken String 
        | NumberToken Int

    
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
    newtype BoolF = BoolF Bool

    {-
    We may need notions of (In-) equality for the token type.
        instance Eq Token where
        (==) (BooleanToken (BoolF bool1)) (BooleanToken (BoolF bool2)) = bool1 == bool2
        (==) (KeywordToken key1) (KeywordToken key2)                   = key1 == key2
        (==) (NameToken name1) (NameToken name2)                       = name1 == name2
        (==) (NumberToken num1) (NumberToken num2)                     = num1 == num2
        (==) _ _                                                       = False
    -}
            
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

    -- SYNTACTICAL ANALYSIS
    {-
    A parser is a parametrized function, that takes a list of tuples. 
    Each tuple contains a token and its line in the source code for error-handling purposes (e.g. [(NameToken f, 1), (NameToken x, 1)]).
    This list of tuples, that represents the tokenized source code, will then be mapped to either an error message (the 'Left' case) if the source code makes no sense,
    or it will be mapped to a tuple, containing the grammatical equivalent of some tokens, and the rest of the tokenized input (the 'Right' case, 
    e.g. "expr8 $ tokenize "y * 8; f x = 3;" --> Right (AtomicExpr (Var "y"),[(*,1),(8,1),(;,1),("f",1),("x",1),(=,1),(3,1),(;,1)]))
    -}
    type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])
    
    {-
    NEW: A definition consists of one expression (the function name), a list of expressions (the function's formal arguments) and the defining expression on the right hand side of the '=',
    e.g.  f    x1 x2 x3 = x1 + x2
         Expr   [Expr]     Expr
    -}
    
    data Def 
        = Def Expr [Expr] Expr  -- f x1 x2 x3 = x1
        deriving Show

    -- A local definition consists of two expressions.
    data LocalDef 
        = LocalDef Expr Expr 
        deriving Show

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
        deriving Show

    instance Eq LocalDef where
        (==) (LocalDef e1 e2) (LocalDef e3 e4) = e1 == e3 && e2 == e4

    instance Eq Expr where
        (==) (Add e1 e2) (Add e3 e4)                     = e1 == e3 && e2 == e4
        (==) (Func e1 e2) (Func e3 e4)                   = e1 == e3 && e2 == e4
        (==) (Mult e1 e2) (Mult e3 e4)                   = e1 == e3 && e2 == e4
        (==) (Div e1 e2) (Div e3 e4)                     = e1 == e3 && e2 == e4
        (==) (UnaryMin e1) (UnaryMin e2)                 = e1 == e2
        (==) (Equal e1 e2) (Equal e3 e4)                 = e1 == e3 && e2 == e4
        (==) (LessThan e1 e2) (LessThan e3 e4)           = e1 == e3 && e2 == e4
        (==) (LogicalAnd e1 e2) (LogicalAnd e3 e4)       = e1 == e3 && e2 == e4
        (==) (LogicalOr e1 e2) (LogicalOr e3 e4)         = e1 == e3 && e2 == e4
        (==) (LogicalNot e1) (LogicalNot e2)             = e1 == e2
        (==) (LetIn e1 e2) (LetIn e3 e4)                 = e1 == e3 && e2 == e4
        (==) (IfThenElse e1 e2 e3) (IfThenElse e4 e5 e6) = e1 == e4 && e2 == e5 && e3 == e6
        (==) (AtomicExpr a1) (AtomicExpr a2)             = a1 == a2
        (==) _ _                                         = False

    instance Eq AtomicExpr where
        (==) (Var s1) (Var s2)         = s1 == s2 
        (==) (LitBool b1) (LitBool b2) = b1 == b2
        (==) (LitNum n1) (LitNum n2)   = n1 == n2 -- LitNum 3 == Litnum 4 
        (==) (Expr e1) (Expr e2)       = e1 == e2
        (==) _ _                       = False 

    instance Eq BoolF where
        (==) (BoolF b1) (BoolF b2) = b1 == b2

    -- An atomic expression is the most basic kind of expression. It is either a name, a numeral or a boolean.
    data AtomicExpr 
        = Var String
        | LitBool BoolF 
        | LitNum Int 
        | Expr Expr 
        deriving Show

    type Var = String

    -- F-CODE DEVELOPMENT
    {-
    We have four kinds of stores to deal with: code (for instructions), stack (for references), 
    heap (for applications) and global (for definitions of non-local functions).
    -} 
    data Store a 
        = Code [a] 
        | Stack [a]
        | Heap [a] 
        deriving (Show, Eq)

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
        deriving Show

    -- The instruction type has several constructors, each one indicating some kind of functionality.
    data Instruction 
        = Reset 
        | Pushfun String 
        | Pushval String Int 
        | Pushparam Int
        | Makeapp 
        | Slide Int 
        | Reduce 
        | Return
        | Halt 
        | Error String
        deriving Show

    {-A heap is a list of heap cells. 
    As the global environment is not changed during runtime, it can be stored at one end of the data store consisting of stack, heap and global environment after compiling.
    -}
    data HeapCell 
        = APP Int Int 
        | VALNum Int Int
        | VALBool Int Bool
        | DEF 
        {
            fname :: String, 
            arity :: Int, 
            caddr :: Int
        }
        deriving (Show, Eq)

    -- A stack is a list of stack cells.
    newtype StackCell 
        = StackCell Int 
        deriving (Show, Eq)