-- This module contains all functionality to pursue lexical analysis in F.
module Tokenizer where
    import Data.Char ( isAlphaNum, isDigit )
    import Declarations

    -- tokenize receives user input and transforms it to a list of tuples, each containing a token and its respective line in the source code.
    tokenize :: String -> [(Token, Int)]
    tokenize ""      = error "Can not compile empty program."
    tokenize program = tokenize' program [] 1

    -- tokenize' deals with spaces and tokenizes keywords and booleans.
    tokenize' :: String -> [(Token, Int)] -> Int -> [(Token, Int)]
    tokenize' [] tokenAcc lineAcc = reverse tokenAcc

    -- Next character is a space.
    tokenize' (' ' : xs) tokenAcc lineAcc         = tokenize' xs tokenAcc lineAcc
    tokenize' ('\t' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc lineAcc
    tokenize' ('\r' : '\n' : xs) tokenAcc lineAcc = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\n' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)
    tokenize' ('\r' : xs) tokenAcc lineAcc        = tokenize' xs tokenAcc (lineAcc + 1)

    -- Next characters form a multiple character keyword or a boolean value, followed by a reserved special character or space.
    tokenize' token@('=' : '=' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken Equals , lineAcc) : tokenAcc) lineAcc
    tokenize' token@('e' : 'l' : 's' : 'e' : x : xs) tokenAcc lineAcc       = if validateChar x then tokenize' (x : xs) ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc         else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('f' : 'a' : 'l' : 's' : 'e' : x : xs) tokenAcc lineAcc = if validateChar x then tokenize' (x : xs) ((BooleanToken $ BoolF False, lineAcc) : tokenAcc) lineAcc else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('i' : 'f' : x : xs) tokenAcc lineAcc                   = if validateChar x then tokenize' (x : xs) ((KeywordToken If , lineAcc) : tokenAcc) lineAcc           else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('i' : 'n' : x : xs) tokenAcc lineAcc                   = if validateChar x then tokenize' (x : xs) ((KeywordToken In , lineAcc) : tokenAcc) lineAcc           else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('l' : 'e' : 't' : x : xs) tokenAcc lineAcc             = if validateChar x then tokenize' (x : xs) ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc          else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('n' : 'o' : 't' : x : xs) tokenAcc lineAcc             = if validateChar x then tokenize' (x : xs) ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc          else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('t' : 'h' : 'e' : 'n' : x : xs) tokenAcc lineAcc       = if validateChar x then tokenize' (x : xs) ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc         else tokenizeNames token tokenAcc lineAcc ""
    tokenize' token@('t' : 'r' : 'u' : 'e' : x : xs) tokenAcc lineAcc       = if validateChar x then tokenize' (x : xs) ((BooleanToken $ BoolF True, lineAcc) : tokenAcc) lineAcc  else tokenizeNames token tokenAcc lineAcc ""

    -- If multiple character keyword is either at the end of an identifier or the program.
    tokenize' ('e' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc           = tokenize' xs ((KeywordToken Else , lineAcc) : tokenAcc) lineAcc
    tokenize' ('f' : 'a' : 'l' : 's' : 'e' : xs) tokenAcc lineAcc     = tokenize' xs ((BooleanToken $ BoolF False , lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'f' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken If , lineAcc) : tokenAcc) lineAcc
    tokenize' ('i' : 'n' : xs) tokenAcc lineAcc                       = tokenize' xs ((KeywordToken In , lineAcc) : tokenAcc) lineAcc
    tokenize' ('l' : 'e' : 't' : xs) tokenAcc lineAcc                 = tokenize' xs ((KeywordToken Let , lineAcc) : tokenAcc) lineAcc
    tokenize' ('n' : 'o' : 't' : xs) tokenAcc lineAcc                 = tokenize' xs ((KeywordToken Not , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'h' : 'e' : 'n' : xs) tokenAcc lineAcc           = tokenize' xs ((KeywordToken Then , lineAcc) : tokenAcc) lineAcc
    tokenize' ('t' : 'r' : 'u' : 'e' : xs) tokenAcc lineAcc           = tokenize' xs ((BooleanToken $ BoolF True , lineAcc) : tokenAcc) lineAcc

    -- Next character is a single character keyword.
    tokenize' (x : xs) tokenAcc lineAcc = case x of
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
                    else error $ "Syntax error: illegal character on input " ++ show x ++ " in line " ++ show lineAcc ++ "."

    -- Tokenize integers or identifiers.
    tokenizeNumbers, tokenizeNames :: String -> [(Token, Int)] -> Int -> String -> [(Token, Int)]
    tokenizeNumbers [x] tokenAcc lineAcc number            = reverse $ (NumberToken (read (number ++ [x]) :: Int), lineAcc) : tokenAcc
    tokenizeNumbers (x : y : xs) tokenAcc lineAcc number   = if isDigit y
                                                                then tokenizeNumbers (y : xs) tokenAcc lineAcc (number ++ [x])
                                                                else tokenize' (y : xs) ((NumberToken (read (number ++ [x]) :: Int), lineAcc) : tokenAcc) lineAcc
    tokenizeNumbers _ _ _ _                                = undefined -- This case can never be reached.

    tokenizeNames [x] tokenAcc lineAcc name                = reverse $ (NameToken $ name ++ [x], lineAcc) : tokenAcc
    tokenizeNames (x : y : xs) tokenAcc lineAcc name       = if isAlphaNum y
                                                                then tokenizeNames (y : xs) tokenAcc lineAcc (name ++ [x])
                                                                else tokenize' (y : xs) ((NameToken $ name ++ [x], lineAcc) : tokenAcc) lineAcc
    tokenizeNames _ _ _ _                                  = undefined -- This case can never be reached.

    -- Helper function validateChar checks whether a character either belongs to the set of reserved special characters of F or is a space.
    validateChar :: Char -> Bool
    validateChar x = x `elem` [';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '<', ' ', '\n', '\t', '\r']