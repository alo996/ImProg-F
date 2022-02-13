{- |
Module      : Main
Description : This module contains the entry point to the implementation.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad (when)
import GHC.IO.Encoding (utf8, setLocaleEncoding)
import System.Environment (getArgs)

import Compiler (compileProgram)
import Declarations (State(ErrorState, code, State))
import MF (interpret, resultToString, interpretVerbose)
import Parser (defsToString, program)
import Tokenizer (tokenize, tokensToString)


{- | Call 'main' either with stack or manually to enter F programs and execute them. 
Look under the hood by setting one or more command line flags: -tokens, -ast, -instructions, -states
-}
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
                    when ("-ast" `elem` args) $ do putStrLn $ defsToString $ fst ast
                    case compileProgram (fst ast) of
                        ErrorState error -> putStrLn error >> anotherOne
                        state            -> do
                            when ("-instructions" `elem` args) $ do print (code state)
                            if "-states" `elem` args 
                                then putStrLn (interpretVerbose state) >> anotherOne 
                                else putStrLn (resultToString $ interpret state) >> anotherOne
                            when ("-tokens" `notElem` args && "-ast" `notElem` args && "-instructions" `notElem` args && "-states" `notElem` args ) $ do putStrLn "\nNo flags used or incorrect flag name. Flag options:\n\n-tokens\n-ast\n-instructions\n-states\n\n"
                Left error -> putStrLn error >> anotherOne
        Left error -> putStrLn error >> anotherOne
  where
    -- 'anotherOne' allows the user to either input another program or abort.
    anotherOne :: IO ()
    anotherOne = do
        putStrLn "\nAnother one? [y/n]"
        response <- getLine
        case response of
            "y" -> putStrLn "\n" >> main
            "n" -> putStrLn "Goodbye!"
            _   -> anotherOne
    -- 'getLines' reads one or more lines as input from the terminal.
    getLines :: IO String
    getLines = do
        x <- getLine
        if x == ""
            then return []
            else do
                xs <- getLines
                return $ x ++ "\n" ++ xs
