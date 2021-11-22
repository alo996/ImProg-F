{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- This module contains all functionality to pursue lexical analysis in F.

module Tokenizer where
    
    import Declarations
    
    -- 'tokenize' receives user input and transforms it to a list of tuples, each containing a token according to F grammar and its respective line in the source code
    tokenize :: String -> [(Token, Int)]
    tokenize "" = error "Can not compile empty program."
    tokenize program = tokenize' program [] 1

    -- helper method that recursively iterates through the user input and returns the intended output specified in 'tokenize'
    tokenize' :: String -> [(Token, Int)] -> Int -> [(Token, Int)]
    -- base case
    tokenize' [] tokenAcc lineAcc = reverse tokenAcc

    -- next character is a whitespace or newline
    tokenize' (' ' : xs) tokenAcc lineAcc  = tokenize' xs tokenAcc lineAcc
    tokenize' ('\n' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc (lineAcc + 1)

    -- next characters form a multiple character keyword or a boolean value
    tokenize' ('=' : '=' : xs) tokenAcc lineAcc                         = tokenize' xs ((KeywordToken Equals , lineAcc) : tokenAcc) lineAcc
    tokenize' ('e' : 'l' : 's' : 'e' : ' ' : xs) tokenAcc lineAcc       = tokenize' xs ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc
    tokenize' ['e', 'l', 's', 'e'] tokenAcc lineAcc                     = reverse $ (KeywordToken Else , lineAcc) : tokenAcc
    tokenize' ('F' : 'a' : 'l' : 's' : 'e' : ' ' : xs) tokenAcc lineAcc = tokenize' xs ((BooleanToken False , lineAcc) : tokenAcc) lineAcc
    tokenize' ['F', 'a', 'l', 's', 'e'] tokenAcc lineAcc                = reverse $ (BooleanToken False , lineAcc) : tokenAcc
    tokenize' ('i' : 'f' : ' ' : xs) tokenAcc lineAcc                   = tokenize' xs ((KeywordToken If , lineAcc) : tokenAcc) lineAcc
    tokenize' ['i', 'f'] tokenAcc lineAcc                               = reverse $ (KeywordToken If , lineAcc) : tokenAcc
    tokenize' ('i' : 'n' : ' ' : xs) tokenAcc lineAcc                   = tokenize' xs ((KeywordToken In , lineAcc) : tokenAcc) lineAcc
    tokenize' ['i', 'n'] tokenAcc lineAcc                               = reverse $ (KeywordToken In , lineAcc) : tokenAcc
    tokenize' ('l' : 'e' : 't' : ' ' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc
    tokenize' ['l', 'e', 't'] tokenAcc lineAcc                          = reverse $ (KeywordToken Let , lineAcc) : tokenAcc
    tokenize' ('n' : 'o' : 't' : ' ' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc
    tokenize' ['n', 'o', 't'] tokenAcc lineAcc                          = reverse $ (KeywordToken Not , lineAcc) : tokenAcc
    tokenize' ('t' : 'h' : 'e' : 'n' : ' ' : xs) tokenAcc lineAcc       = tokenize' xs ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc
    tokenize' ['t', 'h', 'e', 'n'] tokenAcc lineAcc                     = reverse $ (KeywordToken Then , lineAcc) : tokenAcc
    tokenize' ('T' : 'r' : 'u' : 'e' : ' ' : xs) tokenAcc lineAcc       = tokenize' xs ((BooleanToken True , lineAcc) : tokenAcc) lineAcc
    tokenize' ['T', 'r', 'u', 'e'] tokenAcc lineAcc                     = reverse $ (BooleanToken True , lineAcc) : tokenAcc
    
    -- next character forms a keyword
    tokenize' (x:xs) tokenAcc lineAcc = case x of
        '&' -> tokenize' xs ((KeywordToken And , lineAcc) : tokenAcc) lineAcc
        '=' -> tokenize' xs ((KeywordToken Assign , lineAcc) : tokenAcc) lineAcc
        '/' -> tokenize' xs ((KeywordToken Divide , lineAcc) : tokenAcc) lineAcc
        '<' -> tokenize' xs ((KeywordToken LessThan , lineAcc) : tokenAcc) lineAcc
        '(' -> tokenize' xs ((KeywordToken LBracket , lineAcc) : tokenAcc) lineAcc
        '-' -> tokenize' xs ((KeywordToken Minus , lineAcc) : tokenAcc) lineAcc
        '|' -> tokenize' xs ((KeywordToken Or , lineAcc) : tokenAcc) lineAcc
        '+' -> tokenize' xs ((KeywordToken Plus, lineAcc) : tokenAcc) lineAcc
        ')' -> tokenize' xs ((KeywordToken RBracket , lineAcc) : tokenAcc) lineAcc
        ';' -> tokenize' xs ((KeywordToken Semicolon , lineAcc) : tokenAcc) lineAcc
        '*' -> tokenize' xs ((KeywordToken Times, lineAcc) : tokenAcc) lineAcc
        _   -> if x `elem` ['0' .. '9'] 
                    then tokenizeNumbers (x:xs) tokenAcc lineAcc ""
                    else tokenizeNames (x:xs) tokenAcc lineAcc ""

    -- tokenizes numbers or strings
    tokenizeNumbers, tokenizeNames :: String -> [(Token, Int)] -> Int -> String -> [(Token, Int)]
    tokenizeNumbers [x] tokenAcc lineAcc number            = reverse $ (NumberToken $ stringToInt $ number ++ [x], lineAcc) : tokenAcc
    tokenizeNumbers (x : ' ' : xs) tokenAcc lineAcc number = tokenize' xs ((NumberToken $ stringToInt $ number ++ [x], lineAcc) : tokenAcc) lineAcc
    tokenizeNumbers (x : y : xs) tokenAcc lineAcc number   = if y `elem` ['0' .. '9']
                                                                then tokenizeNumbers (y:xs) tokenAcc lineAcc (number ++ [x])
                                                                else tokenize' (y:xs) ((NumberToken $ stringToInt $ number ++ [x], lineAcc) : tokenAcc) lineAcc
    
    tokenizeNames [x] tokenAcc lineAcc name                = reverse $ (NameToken $ name ++ [x], lineAcc) : tokenAcc
    tokenizeNames (x : ' ' : xs) tokenAcc lineAcc name     = tokenize' xs ((NameToken $ name ++ [x], lineAcc) : tokenAcc) lineAcc
    tokenizeNames (x : y : xs) tokenAcc lineAcc name       = if not $ validateChar y 
                                                                then tokenizeNames (y:xs) tokenAcc lineAcc (name ++ [x])
                                                                else tokenize' (y:xs) ((NameToken $ name ++ [x], lineAcc) : tokenAcc) lineAcc
    
    -- checks whether a character is a non-letter keyword, a whitespace or a number  
    validateChar :: Char -> Bool 
    validateChar x = elem x $ [';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '<', '\n'] ++ ['0' .. '9']

    -- transfroms a string to an integer
    stringToInt :: [Char] -> Int
    stringToInt xs = read xs :: Int