-- This module contains all functionality to pursue lexical analysis in F.

module Tokenizer where

    {-
    if3==3 vs if3 x=2
    truefalse vs true3false
    true=false

    IF : muss alleine stehen oder ohne Leerzeichen folgt Bedingung in Klammern

    either a keyword made up of letters (beispiel: if else etc.)
    is directly followed by a whitespace OR
    a keyword not made up of letters (beispiel: + = - etc.) -> then it is accepted as KeywordToken

    true/false:
    - if true==false ; should be tokenized as (if)(true)(==)(false)
    - if(true==false) ; should be tokenized as (if)(()(true)(==)(false)())
    - let true3 = false3 ; should be tokenized as (let)(true3)(=)(false3)
    - truefalse x = 1 ; should be tokenized as (truefalse)(x)(=)(1)
    if/then/else:
    - if3 x = 3 ;
    - ifthenelse x = 3;
    - if(3==3)then(2)else(3) ;
    - if true then3else4 ;
    - if true then true else false ;
    let/in:
    - let3 ; NameToken
    - in3 ; NameToken
    - let(x=3)in(x*x) ; should be tokenized as (let)(()(x)(=)(3)(in)(()(x)(*)(x)


    tokenize "iffalse"
    evtl 32a fall gleich im Tokenizer abfangen
    evtl main zu keyword?
    truefalseletthen ok, true=false ok
    -}

    import Data.Char
    import Declarations

    -- tokenize receives user input and transforms it to a list of tuples, each containing a token its respective line in the source code
    tokenize :: String -> [(Token, Int)]
    tokenize "" = error "Can not compile empty program."
    tokenize program = tokenize' program [] 1

    -- tokenize' deals with spaces and tokenizes keywords and booleans
    tokenize' :: String -> [(Token, Int)] -> Int -> [(Token, Int)]
    tokenize' [] tokenAcc lineAcc = reverse tokenAcc

    -- next character is a space
    tokenize' (' ' : xs) tokenAcc lineAcc         = tokenize' xs tokenAcc lineAcc
    tokenize' ('\t' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc lineAcc
    tokenize' ('\r' : '\n' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\n' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\r' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)

    -- next characters form a multiple character keyword or a boolean value
    tokenize' ('=' : '=' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken Equals , lineAcc) : tokenAcc) lineAcc
    tokenize' ('e' : 'l' : 's' : 'e' : a : xs) tokenAcc lineAcc       = if (validateChar a) then tokenize' (a:xs) ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('e' : 'l' : 's' : 'e' : a : xs) tokenAcc lineAcc ""
    tokenize' ('f' : 'a' : 'l' : 's' : 'e' : a : xs) tokenAcc lineAcc = if (validateChar a) then tokenize' (a:xs) ((BooleanToken $ BoolF False, lineAcc) : tokenAcc) lineAcc else tokenizeNames ('f' : 'a' : 'l' : 's' : 'e' : a : xs) tokenAcc lineAcc ""
    tokenize' ('i' : 'f' : a : xs) tokenAcc lineAcc                   = if (validateChar a) then tokenize' (a:xs) ((KeywordToken If , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('i' : 'f' : a : xs) tokenAcc lineAcc ""
    tokenize' ('i' : 'n' : a : xs) tokenAcc lineAcc                   = if (validateChar a) then tokenize' (a:xs) ((KeywordToken In , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('i' : 'n' : a : xs) tokenAcc lineAcc ""
    tokenize' ('l' : 'e' : 't' : a : xs) tokenAcc lineAcc             = if (validateChar a) then tokenize' (a:xs) ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('l' : 'e' : 't' : a : xs) tokenAcc lineAcc ""
    tokenize' ('n' : 'o' : 't' : a : xs) tokenAcc lineAcc             = if (validateChar a) then tokenize' (a:xs) ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('n' : 'o' : 't' : a : xs) tokenAcc lineAcc ""
    tokenize' ('t' : 'h' : 'e' : 'n' : a : xs) tokenAcc lineAcc       = if (validateChar a) then tokenize' (a:xs) ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc else tokenizeNames ('t' : 'h' : 'e' : 'n' : a : xs) tokenAcc lineAcc ""
    tokenize' ('t' : 'r' : 'u' : 'e' : a : xs) tokenAcc lineAcc       = if (validateChar a) then tokenize' (a:xs) ((BooleanToken $ BoolF True, lineAcc) : tokenAcc) lineAcc else tokenizeNames ('t' : 'r' : 'u' : 'e' : a : xs) tokenAcc lineAcc ""

    -- if keyword is at the end
    tokenize' ('e' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc           = tokenize' xs ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc
    tokenize' ('f' : 'a' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc     = tokenize' xs ((BooleanToken $ BoolF False , lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'f' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken If , lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'n' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken In , lineAcc) : tokenAcc) lineAcc
    tokenize' ('l' : 'e' : 't' : xs) tokenAcc lineAcc                 = tokenize' xs ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc
    tokenize' ('n' : 'o' : 't' : xs) tokenAcc lineAcc                 = tokenize' xs ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'h' : 'e' : 'n' : xs) tokenAcc lineAcc           = tokenize' xs ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'r' : 'u' : 'e' : xs) tokenAcc lineAcc           = tokenize' xs ((BooleanToken $ BoolF True , lineAcc) : tokenAcc) lineAcc

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

    validateChar :: Char -> Bool
    validateChar x = elem x $ [' ', ';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '<', '\n']
