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

    -- Adress the fact that boolean values in F are lowercase.
    newtype BoolF = BoolF Bool 

    instance Show BoolF where
        show (BoolF True) = "true"
        show (BoolF False)    = "false"

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