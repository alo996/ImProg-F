-- This module contains all functionality to pursue lexical analysis in F.

module Tokenizer where
    
    import Data.Char
    import Declarations
    
    -- 'tokenize' receives user input and transforms it to a list of tuples, each containing a token its respective line in the source code
    tokenize :: String -> [(Token, Int)]
    tokenize "" = error "Can not compile empty program."
    tokenize program = tokenize' program [] 1

    -- helper method that recursively iterates through the user input and returns the intended output specified in 'tokenize'
    tokenize' :: String -> [(Token, Int)] -> Int -> [(Token, Int)]
    -- base case
    tokenize' [] tokenAcc lineAcc = reverse tokenAcc

    -- next character is a whitespace or newline
    tokenize' (' ' : xs) tokenAcc lineAcc         = tokenize' xs tokenAcc lineAcc
    tokenize' ('\t' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc lineAcc
    tokenize' ('\r' : '\n' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\n' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\r' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)

    -- next characters form a multiple character keyword or a boolean value
    tokenize' ('=' : '=' : xs) tokenAcc lineAcc                         = tokenize' xs ((KeywordToken Equals , lineAcc) : tokenAcc) lineAcc
    tokenize' ('e' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc
    tokenize' ('f' : 'a' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc       = tokenize' xs ((BooleanToken $ BoolF False, lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'f' : xs) tokenAcc lineAcc                         = tokenize' xs ((KeywordToken If , lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'n' : xs) tokenAcc lineAcc                         = tokenize' xs ((KeywordToken In , lineAcc) : tokenAcc) lineAcc
    tokenize' ('l' : 'e' : 't' : xs) tokenAcc lineAcc                   = tokenize' xs ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc
    tokenize' ('n' : 'o' : 't' : xs) tokenAcc lineAcc                   = tokenize' xs ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'h' : 'e' : 'n' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'r' : 'u' : 'e' : xs) tokenAcc lineAcc             = tokenize' xs ((BooleanToken $ BoolF True, lineAcc) : tokenAcc) lineAcc

    -- next character forms a keyword
    tokenize' (x:xs) tokenAcc lineAcc = case x of
        '&' -> tokenize' xs ((KeywordToken And , lineAcc) : tokenAcc) lineAcc
        '=' -> tokenize' xs ((KeywordToken Assign , lineAcc) : tokenAcc) lineAcc
        '/' -> tokenize' xs ((KeywordToken Divide , lineAcc) : tokenAcc) lineAcc
        '<' -> tokenize' xs ((KeywordToken Less , lineAcc) : tokenAcc) lineAcc
        '(' -> tokenize' xs ((KeywordToken LBracket , lineAcc) : tokenAcc) lineAcc
        '-' -> tokenize' xs ((KeywordToken Minus , lineAcc) : tokenAcc) lineAcc
        '|' -> tokenize' xs ((KeywordToken Or , lineAcc) : tokenAcc) lineAcc
        '+' -> tokenize' xs ((KeywordToken Plus, lineAcc) : tokenAcc) lineAcc
        ')' -> tokenize' xs ((KeywordToken RBracket , lineAcc) : tokenAcc) lineAcc
        ';' -> tokenize' xs ((KeywordToken Semicolon , lineAcc) : tokenAcc) lineAcc
        '*' -> tokenize' xs ((KeywordToken Times, lineAcc) : tokenAcc) lineAcc
        _   -> if isAlphaNum x
                    then if isDigit x
                        then tokenizeNumbers (x:xs) tokenAcc lineAcc ""
                        else tokenizeNames (x:xs) tokenAcc lineAcc ""
                    else error $ "Syntax error: illegal character on input " ++ show x ++ " in line " ++ show lineAcc

    -- tokenizes numbers or names
    tokenizeNumbers, tokenizeNames :: String -> [(Token, Int)] -> Int -> String -> [(Token, Int)]
    tokenizeNumbers [x] tokenAcc lineAcc number            = reverse $ (NumberToken (read (number ++ [x]) :: Int), lineAcc) : tokenAcc
    tokenizeNumbers (x : y : xs) tokenAcc lineAcc number   = if isDigit y
                                                                then tokenizeNumbers (y : xs) tokenAcc lineAcc (number ++ [x])
                                                                else tokenize' (y : xs) ((NumberToken (read (number ++ [x]) :: Int), lineAcc) : tokenAcc) lineAcc
    tokenizeNumbers _ _ _ _                                = undefined

    tokenizeNames [x] tokenAcc lineAcc name                = reverse $ (NameToken $ name ++ [x], lineAcc) : tokenAcc
    tokenizeNames (x : y : xs) tokenAcc lineAcc name       = if isAlphaNum y
                                                                then tokenizeNames (y : xs) tokenAcc lineAcc (name ++ [x]) 
                                                                else tokenize' (y : xs) ((NameToken $ name ++ [x], lineAcc) : tokenAcc) lineAcc
    tokenizeNames _ _ _ _                                  = undefined