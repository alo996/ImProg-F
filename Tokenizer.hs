-- This module contains all functionality to pursue lexical analysis in F.

module Tokenizer where
    
    import Declarations
    import Data.List
    
    {-
    tokenize receives user input and transforms it to a list of tuples, each containing a token according to F grammar and its respective line in the program
    -}
    tokenize :: String -> [(Token, Int)]
    tokenize "" = error "Can not compile empty program."
    tokenize program = tokenize' program [] 1

    {-
    tokenize' receives user input and transforms it to a list of tuples, each containing a token according to F grammar and its respective line in the program
    -}   
    tokenize', tokenize'' :: String -> [(Token, Int)] -> Int -> [(Token, Int)]
    -- base case
    tokenize' [] tokenAcc lineAcc = reverse tokenAcc

    -- next character is a whitespace, newline or return
    tokenize' (' ' : xs) tokenAcc lineAcc  = tokenize' xs tokenAcc lineAcc
    tokenize' ('\n' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\r' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc lineAcc

    -- next characters form a keyword or boolean value
    tokenize' ('=' : '=' : xs) tokenAcc lineAcc                         = tokenize' xs ((KeywordToken Equals , lineAcc) : tokenAcc) lineAcc
    tokenize' ('n' : 'o' : 't' : ' ' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc
    tokenize' ('n' : 'o' : 't' : []) tokenAcc lineAcc                   = (KeywordToken Not , lineAcc) : tokenAcc
    tokenize' ('l' : 'e' : 't' : ' ' : xs) tokenAcc lineAcc             = tokenize' xs ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc
    tokenize' ('l' : 'e' : 't' : []) tokenAcc lineAcc                   = (KeywordToken Let , lineAcc) : tokenAcc
    tokenize' ('i' : 'n' : ' ' : xs) tokenAcc lineAcc                   = tokenize' xs ((KeywordToken In , lineAcc) : tokenAcc) lineAcc
    tokenize' ('T' : 'r' : 'u' : 'e' : ' ' : xs) tokenAcc lineAcc       = tokenize' xs ((BooleanToken True , lineAcc) : tokenAcc) lineAcc
    tokenize' ('T' : 'r' : 'u' : 'e' : []) tokenAcc lineAcc             = (BooleanToken True , lineAcc) : tokenAcc
    tokenize' ('F' : 'a' : 'l' : 's' : 'e' : ' ' : xs) tokenAcc lineAcc = tokenize' xs ((BooleanToken False , lineAcc) : tokenAcc) lineAcc
    tokenize' ('F' : 'a' : 'l' : 's' : 'e' : []) tokenAcc lineAcc       = (BooleanToken False , lineAcc) : tokenAcc
    
    -- next character forms a keyword
    tokenize' (x:xs) tokenAcc lineAcc = case x of
        ';' -> tokenize' xs ((KeywordToken Semicolon , lineAcc) : tokenAcc) lineAcc
        '=' -> tokenize' xs ((KeywordToken Assign , lineAcc) : tokenAcc) lineAcc
        '(' -> tokenize' xs ((KeywordToken LBracket , lineAcc) : tokenAcc) lineAcc
        ')' -> tokenize' xs ((KeywordToken RBracket , lineAcc) : tokenAcc) lineAcc
        '&' -> tokenize' xs ((KeywordToken And , lineAcc) : tokenAcc) lineAcc
        '|' -> tokenize' xs ((KeywordToken Or , lineAcc) : tokenAcc) lineAcc
        '+' -> tokenize' xs ((KeywordToken Plus, lineAcc) : tokenAcc) lineAcc
        '-' -> tokenize' xs ((KeywordToken Minus , lineAcc) : tokenAcc) lineAcc
        '*' -> tokenize' xs ((KeywordToken Times, lineAcc) : tokenAcc) lineAcc
        '/' -> tokenize' xs ((KeywordToken Divide , lineAcc) : tokenAcc) lineAcc
        x   -> tokenize'' (x:xs) tokenAcc lineAcc

    -- next characters from a number
    tokenize'' (x:xs) tokenAcc lineAcc = if x `elem` ['0' .. '9'] 
                                            then tokenizeNumbers (x:xs) tokenAcc lineAcc ""
                                            else tokenizeNames (x:xs) tokenAcc lineAcc ""

    -- tokenizes numbers
    tokenizeNumbers, tokenizeNames :: String -> [(Token, Int)] -> Int -> String -> [(Token, Int)]
    tokenizeNumbers (x : []) tokenAcc lineAcc number       = (NumberToken [x], lineAcc) : tokenAcc
    tokenizeNumbers (x : ' ' : xs) tokenAcc lineAcc number = tokenize' xs ((NumberToken (number ++ [x]), lineAcc) : tokenAcc) lineAcc
    tokenizeNumbers (x : y : xs) tokenAcc lineAcc number   = if y `elem` ['0' .. '9']
                                                                then tokenizeNumbers (y:xs) tokenAcc lineAcc (number ++ [x])
                                                                else tokenize' (y:xs) ((NumberToken (number ++ [x]), lineAcc) : tokenAcc) lineAcc
    
    tokenizeNames (x : []) tokenAcc lineAcc name        = (NameToken (name ++ [x]), lineAcc) : tokenAcc
    tokenizeNames (x: ' ' : xs) tokenAcc lineAcc name   = tokenize' xs ((NameToken (name ++ [x]), lineAcc) : tokenAcc) lineAcc
    tokenizeNames (x : y : xs) tokenAcc lineAcc name    = if not $ validateChar y 
                                                              then tokenizeNames (y:xs) tokenAcc lineAcc (name ++ [x])
                                                              else tokenize' (y:xs) ((NameToken (name ++ [x]), lineAcc) : tokenAcc) lineAcc
    
    validateChar :: Char -> Bool 
    validateChar x = elem x $ [';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '\n', '\r'] ++ ['0' .. '9']