-- This module contains all necessary types and aliases.

module Declarations where

    data Token =
        BooleanToken Bool |
        KeywordToken Keyword |
        NameToken String |
        NumberToken Int
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
        show Times     = "*"
        show Then      = "then"


    data SyntaxTree = SyntaxTree [(Token)] deriving (Show)


    data Expression = Expression
          | Keyword KeywordToken
          | Value NumberToken
          | Name NameToken
          | Not Expression
          | Minus Expression
          | Popen Expression      --open paranthesis
          | Pclose Expression     --close paranthesis
          | Atom AtomExp
          | Add Expression Expression
          | Subtract Expression Expression
          | Times Expression Expression
          | Divide Expression Expression
          deriving Show


    --  data BinSym = And | Or | Equals | Less | Plus | Minus | Times | Divide

      --data UnSym =  Not | Minus deriving (Show)

    -- data AtomExp = NameToken NumberToken | NameToken Keyword |NameToken BoolToken deriving (Show, Eq)