-- This module contains all necessary types and aliases.

module Declarations where

    data Token =
        BooleanToken Bool |
        KeywordToken Keyword |
        NameToken String |
        NumberToken String
        deriving Show

    data Keyword = And |
                   Assign |
                   Divide |
                   Else |
                   Equals |
                   If |
                   In |
                   LessThan |
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
                             

    instance Show Keyword where
        show And       = "&"
        show Assign    = "="
        show Divide    = "/"
        show Else      = "else"
        show Equals    = "=="
        show If        = "if"
        show In        = "in"
        show LessThan  = "<"
        show Let       = "let"
        show LBracket  = "("
        show Minus     = "-"
        show Not       = "not"
        show Or        = "|"
        show Plus      = "+"
        show RBracket  = ")"
        show Semicolon = ";"
        show Then      = "then"
        show Times     = "*"