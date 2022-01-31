{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Compiler
import Declarations
import MF 
import Parser 
import Tokenizer


main :: IO String
main = undefined

main' :: String -> IO ()
main' input =
    case tokenize input of
        Right toks -> case program toks of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error                           -> putStrLn error
                s@State{pc, sp, code, stack, global, heap} -> putStrLn $ resultToString $ interpret s
            Left error -> putStrLn error
        Left error -> putStrLn error