{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Compiler
import Declarations
import MF 
import Parser 
import Data.Text
import Tokenizer


main :: IO ()
main = do
    putStrLn "Please enter an F program and hit enter (end with an empty line):"
    input <- getLines
    case tokenize input of
        Right toks -> case program toks of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error -> do
                    putStrLn error
                    subroutine
                s@State{}        -> do
                    putStrLn $ resultToString $ interpret s
                    subroutine
            Left error -> do
                putStrLn error
                subroutine
        Left error -> do
            putStrLn error 
            subroutine
      where
        subroutine :: IO ()
        subroutine = do
            putStrLn "\nAnother one? [y/n]"
            response <- getLine
            putStrLn "\n"
            case response of
                "y" -> main
                "n" -> putStrLn "Goodbye!"  
                _   -> subroutine
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
        Right toks -> case program toks of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error -> putStrLn error
                s@State{}        -> putStrLn $ resultToString $ interpret s
            Left error -> putStrLn error
        Left error -> putStrLn error