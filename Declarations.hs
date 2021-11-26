-- This module contains all necessary types and aliases.

module Declarations where 

    data Token =
        BooleanToken BoolF |
        KeywordToken Keyword |
        NameToken String |
        NumberToken Int
        deriving Show

    data Keyword = 
        And |
        Assign |
        Divide |
        Else |
        Equals |
        If |
        In |
        Less |
        Let |
        LBracket |
        Minus | 
        Not | 
        Or | 
        Plus |
        RBracket |
        Semicolon |  
        Times |
        Then               

    newtype BoolF = BoolF Bool 

    instance Show BoolF where
        show (BoolF True) = "true"
        show (BoolF _)    = "false"

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

{-
    type ComparisonOp = Keyword
    newtype Variable =  Var String
    data Literal = Bool | Integer
    data AtomicExpr = Variable | Literal | Expression
    type Expr8 = [AtomicExpr]
    type RestExpr7 = [Expr8]
    data Expr7 = Expr7 Expr8 RestExpr7
    type Expr6 = Expr7
    type RestExpr5 = [Expr6]
    data Expr5 = Expr5 Expr6 RestExpr5
    data Expr4 = Expr4 Expr5 [(ComparisonOp, Expr5)]
    type Expr3 = Expr4

    data Definition = Definition [Variable] Expression
    data Expression = Expr Int

    data SyntaxTree = SyntaxTree [(Token)] deriving (Show)
    data SyntaxTree = Tree Expression SyntaxTree SyntaxTree | Leaf Expression

    data Expression =
            Atom AtomExp
          | Not Expression
          | Minus Expression
          | Popen Expression      --open paranthesis
          | Pclose Expression     --close paranthesis
          | Add Expression Expression
          | Subtract Expression Expression
          | Times Expression Expression
          | Divide Expression Expression
          deriving Show

    data Variable = Name String
    data ComparisonOperator = 
    data BinSym = And | Or | Equals | Less | Plus | Minus | Times | Divide
    data AtomExp = VarExp NameToken | NumExp NumberToken | BoolExp BoolToken deriving (Show, Eq)
    -}

    
