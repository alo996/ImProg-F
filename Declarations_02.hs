-- This module contains all necessary types and aliases.

module Declarations

where

    data Token =
        BooleanToken Bool |
        KeywordToken Keyword |
        NameToken String |
        NumberToken Integer
        deriving Show

    data Keyword = Semicolon |
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
                        Greater |
                        Less |
                        GreaterEqu |
                        LessEqu |
                        Plus |
                        Minus |
                        Times |
                        Divide

    instance Show Keyword where
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
        show Greater   = ">"
        show Less      = "<"
        show GreaterEqu= ">="
        show LessEqu   = "<="
        show Plus      = "+"
        show Minus     = "-"
        show Times     = "*"
        show Divide    = "/"

        instance Show NameToken where
          show (NameToken s) = s

        instance Show NumberToken where
          show (NumberToken s) = s

        instance Show BoolToken where
          show (BoolToken s) = s

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
