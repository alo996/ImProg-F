-- This module contains all necessary types and aliases.

module Declarations where

    data Token =
        BooleanToken |
        MetaToken |
        NameToken String |
        NumberToken Int

    data BooleanToken = False | True deriving Show
    
    data MetaToken = Semicolon | 
                     Assign | 
                     Let | 
                     In | 
                     If | 
                     Then | 
                     Else | 
                     LBracket | 
                     RBracket | 
                     And | 
                     Not | 
                     Or | 
                     Equals |
                     Plus |
                     Minus | 
                     Times |
                     Divide

    instance Show MetaToken where
        show Semicolon = ";"
        show Assign    = "="
        show Let       = "let"
        show In        = "in"
        show If        = "if"
        show Then      = "then"
        show Else      = "else"
        show LBracket  = "("
        show RBracket  = ")"
        show And       = "&"
        show Not       = "not"
        show Or        = "|"
        show Equals    = "=="
        show Plus      = "+"
        show Minus     = "-"
        show Times     = "*"
        show Divide    = "/"