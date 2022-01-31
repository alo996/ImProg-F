{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Compiler (compileProgram)
import Debug.Trace
import Declarations (State(State, ErrorState))
import MF (interpret, resultToString)
import Parser (program)
import Tokenizer (tokenize)


main :: IO ()
main = do
    putStrLn "Please enter an F program and hit enter (end with an empty line):"
    input <- getLines
    case tokenize input of
        Right toks -> case program toks of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error -> putStrLn error >> anotherOne
                s@State{}        -> putStrLn (resultToString $ interpret s) >> anotherOne
            Left error -> putStrLn error >> anotherOne
        Left error -> putStrLn error >> anotherOne
      where
        anotherOne :: IO ()
        anotherOne = do
            putStrLn "\nAnother one? [y/n]"
            response <- getLine
            case response of
                "y" -> putStrLn "\n" >> main
                "n" -> putStrLn "Goodbye!"  
                _   -> anotherOne
        getLines :: IO String
        getLines = do
            x <- getLine
            if x == ""
                then return []
                else do
                    xs <- getLines
                    return $ x ++ "\n" ++ xs

main' :: String -> IO ()
main' input =
    case tokenize input of
        Right toks -> case trace (show toks) program toks of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error -> putStrLn error
                s@State{}        -> putStrLn $ resultToString $ interpret s
            Left error -> putStrLn error
        Left error -> putStrLn error