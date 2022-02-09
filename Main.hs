{- |
Module      : Main
Description : This module is the main program of this project containing functions to enter F code and returning results at different levels from tokenizer to MF.
-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Environment
import Control.Monad
import Compiler (compileProgram)
import Declarations (State(ErrorState, code))
import MF (interpret, resultToString, interpretVerbose)
import Parser (defsToString, program)
import Tokenizer (tokenize, tokensToString)
import GHC.IO.Encoding
import System.Timeout


-- | 'main' is the program loop which asks for F code and prints the results and interim results of the compiler process.
-- | flags: -tokens -parse -instructions -states
main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- getArgs
    putStrLn "Please enter an F program and hit enter (end with an empty line):"
    input <- getLines
    case tokenize input of
        Right toks -> do
            when ("-tokens" `elem` args) $ do putStrLn $ tokensToString toks
            case program toks of
                Right ast -> do
                    when ("-parse" `elem` args) $ do putStrLn $ defsToString $ fst ast
                    case compileProgram (fst ast) of
                        ErrorState error -> putStrLn error >> anotherOne
                        state            -> do
                            when ("-instructions" `elem` args) $ do print (code state)
                            if ("-states" `elem` args) then putStrLn (resultToString $ interpret state True) >> anotherOne else putStrLn (resultToString $ interpret state False) >> anotherOne
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

-- | 'main' ' takes a F code string as an input and prints the results to the console. This function also prints out all interim states of MF.
main' :: String -> IO ()
main' input =
    case tokenize input of
        Right toks -> do
            putStrLn $ tokensToString toks
            case program toks of
                Right ast -> do
                    putStrLn $ defsToString $ fst ast
                    case compileProgram (fst ast) of
                        ErrorState error -> putStrLn error
                        state            -> do
                            print $ code state
                            putStrLn $ interpretVerbose state
                Left error -> putStrLn error
        Left error -> putStrLn error
