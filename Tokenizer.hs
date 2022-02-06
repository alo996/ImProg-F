{- |
Module      : Tokenizer
Description : This module contains all functionality to pursue lexical analysis in F.
-}
module Tokenizer where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Declarations
    (BoolF(BoolF),
    Token(..),
    Keyword(Times, Equals, Else, If, In, Let, Not, Then, And, Assign,
            Divide, Less, LBracket, Minus, Or, Plus, RBracket, Semicolon))


-- |'tokenize' receives user input and, if successful, transforms it into a list of tuples, each containing a token and its line number in the source code.
tokenize :: String -> Either String [(Token, Int)]
tokenize ""    = Left "Runtime error: Can not compile empty program."
tokenize input = tokenize' input [] 1

{- | 'tokenize'' deals with spaces and tokenizes keywords and booleans. It receives user input, a list of already tokenized (token, line number)-pairs and an integer corresponding to the current line in the source code. It either returns an extended list of (token, line number)-pairs or an error message.
-}
tokenize' :: String -> [(Token, Int)] -> Int -> Either String [(Token, Int)]
tokenize' [] tAcc lAcc                                         = Right $ reverse tAcc
-- Checks whether next character is a space.
tokenize' (' ' : xs) tAcc lAcc                                 = tokenize' xs tAcc lAcc
tokenize' ('\t' : xs) tAcc lAcc                                = tokenize' xs tAcc lAcc
tokenize' ('\r' : '\n' : xs) tAcc lAcc                         = tokenize' xs tAcc (lAcc + 1)
tokenize' ('\n' : xs) tAcc lAcc                                = tokenize' xs tAcc (lAcc + 1)
tokenize' ('\r' : xs) tAcc lAcc                                = tokenize' xs tAcc (lAcc + 1)
-- Checks whether next characters form a keyword or a boolean value, followed by a reserved special character or a space.
tokenize' ('=' : '=' : xs) tAcc lAcc                           = tokenize' xs ((KeywordToken Equals, lAcc) : tAcc) lAcc
tokenize' tok@('e' : 'l' : 's' : 'e' : x : xs) tAcc lAcc       = keyCheck x (KeywordToken Else) (x : xs) tok tAcc lAcc
tokenize' tok@('f' : 'a' : 'l' : 's' : 'e' : x : xs) tAcc lAcc = keyCheck x (BooleanToken $ BoolF False) (x : xs) tok tAcc lAcc
tokenize' tok@('i' : 'f' : x : xs) tAcc lAcc                   = keyCheck x (KeywordToken If) (x : xs) tok tAcc lAcc
tokenize' tok@('i' : 'n' : x : xs) tAcc lAcc                   = keyCheck x (KeywordToken In) (x : xs) tok tAcc lAcc
tokenize' tok@('l' : 'e' : 't' : x : xs) tAcc lAcc             = keyCheck x (KeywordToken Let) (x : xs) tok tAcc lAcc
tokenize' tok@('n' : 'o' : 't' : x : xs) tAcc lAcc             = keyCheck x (KeywordToken Not) (x : xs) tok tAcc lAcc
tokenize' tok@('t' : 'h' : 'e' : 'n' : x : xs) tAcc lAcc       = keyCheck x (KeywordToken Then) (x : xs) tok tAcc lAcc
tokenize' tok@('t' : 'r' : 'u' : 'e' : x : xs) tAcc lAcc       = keyCheck x (BooleanToken $ BoolF True) (x : xs) tok tAcc lAcc
-- A multiple character keyword is either at the end of an identifier or the program.
tokenize' ('e' : 'l' : 's' : 'e' : xs) tAcc lAcc               = tokenize' xs ((KeywordToken Else, lAcc) : tAcc) lAcc
tokenize' ('f' : 'a' : 'l' : 's' : 'e' : xs) tAcc lAcc         = tokenize' xs ((BooleanToken $ BoolF False , lAcc) : tAcc) lAcc
tokenize' ('i' : 'f' : xs) tAcc lAcc                           = tokenize' xs ((KeywordToken If, lAcc) : tAcc) lAcc
tokenize' ('i' : 'n' : xs) tAcc lAcc                           = tokenize' xs ((KeywordToken In, lAcc) : tAcc) lAcc
tokenize' ('l' : 'e' : 't' : xs) tAcc lAcc                     = tokenize' xs ((KeywordToken Let, lAcc) : tAcc) lAcc
tokenize' ('n' : 'o' : 't' : xs) tAcc lAcc                     = tokenize' xs ((KeywordToken Not, lAcc) : tAcc) lAcc
tokenize' ('t' : 'h' : 'e' : 'n' : xs) tAcc lAcc               = tokenize' xs ((KeywordToken Then, lAcc) : tAcc) lAcc
tokenize' ('t' : 'r' : 'u' : 'e' : xs) tAcc lAcc               = tokenize' xs ((BooleanToken $ BoolF True, lAcc) : tAcc) lAcc
-- Checks whether the next character is a keyword.
tokenize' ('&' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken And , lAcc) : tAcc) lAcc
tokenize' ('=' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Assign, lAcc) : tAcc) lAcc
tokenize' ('/' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Divide, lAcc) : tAcc) lAcc
tokenize' ('<' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Less, lAcc) : tAcc) lAcc
tokenize' ('(' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken LBracket, lAcc) : tAcc) lAcc
tokenize' ('-' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Minus, lAcc) : tAcc) lAcc
tokenize' ('|' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Or, lAcc) : tAcc) lAcc
tokenize' ('+' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Plus, lAcc) : tAcc) lAcc
tokenize' (')' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken RBracket, lAcc) : tAcc) lAcc
tokenize' (';' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Semicolon, lAcc) : tAcc) lAcc
tokenize' ('*' : xs) tAcc lAcc                                 = tokenize' xs ((KeywordToken Times, lAcc) : tAcc) lAcc
-- The next character is not a space, a keyword or a part of a keyword. Checks for digits and letters, otherwise returns an error.
tokenize' (x : xs) tAcc lAcc
    | isAlphaNum x = if isDigit x
                        then tokenizeNumber (x : xs) tAcc lAcc ""
                        else tokenizeName (x : xs) tAcc lAcc ""
    | otherwise    = Left $ "Lexical error in line " ++ show lAcc ++ ": Illegal character " ++ show x ++ "." 

{- | Tokenizes integers or identifiers. Both 'tokenizeNumber' and 'tokenizeName' additionally take an accumulator to recursively concatenate integers or letters to already processed input until some condition is met.
-}
tokenizeNumber, tokenizeName :: String -> [(Token, Int)] -> Int -> String -> Either String [(Token, Int)]
tokenizeNumber [x] tAcc lAcc num = Right $ reverse $ (NumberToken (read (num ++ [x]) :: Int), lAcc) : tAcc
tokenizeNumber (x : y : ys) tAcc lAcc num
    | isDigit y = tokenizeNumber (y : ys) tAcc lAcc (num ++ [x])
    | isAlpha y = Left $ "Lexical error in line " ++ show lAcc ++ ": Identifiers must not begin with a digit."
    | otherwise = tokenize' (y : ys) ((NumberToken (read (num ++ [x]) :: Int), lAcc) : tAcc) lAcc
tokenizeNumber _ _ _ _              = Left "Lexical error."

tokenizeName [x] tAcc lAcc name = Right $ reverse $ (NameToken $ name ++ [x], lAcc) : tAcc
tokenizeName (x : y : ys) tAcc lAcc name
    | isAlphaNum y = tokenizeName (y : ys) tAcc lAcc (name ++ [x])
    | otherwise    = tokenize' (y : ys) ((NameToken $ name ++ [x], lAcc) : tAcc) lAcc
tokenizeName _ _ _ _            = Left "Lexical error."


---------------------------------------- HELPER FUNCTIONS FOR TOKENIZER ----------------------------------------
{- | 'keyCheck' validates whether a string is a keyword or just part of a longer name, based on its following character. 
It takes the character following the assumed keyword, the assumed keyword, the remaining input with and without the assumed keyword, the already processed list of (token, line number)-pairs and the line number.
-}
keyCheck :: Char -> Token -> String -> String -> [(Token, Int)] -> Int -> Either String [(Token, Int)]
keyCheck c checktok rest input tAcc lAcc
    | charCheck c = tokenize' rest ((checktok, lAcc) : tAcc) lAcc 
    | otherwise   = tokenizeName input tAcc lAcc ""
  where
    -- 'charCheck' validates whether a character either belongs to the set of reserved special characters of F or is a space.
    charCheck :: Char -> Bool
    charCheck c = c `elem` [';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '<', ' ', '\n', '\t', '\r']

tokensToString :: [(Token, Int)] -> String
tokensToString toks = foldl (++) "+-----------+\n| Tokens |\n+-----------+\n" (map (\ (tok, _) -> show tok ++ "\n") toks)
