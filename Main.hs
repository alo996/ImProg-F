{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Compiler (compileProgram)
import Declarations
import MF (interpret, resultToString, interpretVerbose)
import Parser (defsToString, program)
import Tokenizer (tokenize, tokensToString)


main :: IO ()
main = do
    putStrLn "Please enter an F program and hit enter (end with an empty line):"
    input <- getLines
    case tokenize input of
        Right toks -> do 
            putStrLn $ tokensToString toks
            case program toks of
                Right ast -> do
                    putStrLn $ defsToString $ fst ast
                    case compileProgram (fst ast) of
                        ErrorState error -> putStrLn error >> anotherOne
                        state            -> do
                            print (code state)
                            putStrLn (resultToString $ interpret state) >> anotherOne
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