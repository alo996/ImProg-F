-- This module contains all necessary types, aliases and type class instantiations.
module Declarations where 

    data Token 
        = BooleanToken BoolF 
        | KeywordToken Keyword 
        | NameToken String 
        | NumberToken Int 

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

    -- BoolF adresses the fact that boolean values in F are lowercase.
    newtype BoolF = BoolF Bool

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

    type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

    newtype Prog = Prog [Def] deriving Show
    
    data Def = Def [Expr] Expr deriving Show

    type LocalDefs = [LocalDef]

    data LocalDef = LocalDef Expr Expr deriving Show

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
        | LetIn LocalDefs Expr
        | IfThenElse Expr Expr Expr
        | AtomicExpr AtomicExpr
        deriving Show

    data AtomicExpr 
        = Var String
        | LitBool BoolF 
        | LitNum Int 
        | Expr Expr 
        deriving Show

    type Var = String