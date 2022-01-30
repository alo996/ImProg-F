{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Compiler ( compileProgram )
import Declarations (State (State, ErrorState, code))
import MF ( interpret, resultToString )
import Parser ( program )
import Tokenizer ( tokenize )


main :: IO String
main = undefined

main' :: String -> IO ()
main' input =
    case tokenize input of
        Right tokens -> case program tokens of
            Right ast  -> case compileProgram (fst ast) of
                ErrorState error   -> putStrLn error
                state@State{code}  -> do
                    print code
                    putStrLn $ resultToString $ interpret state
            Left error -> putStrLn error
        Left error -> putStrLn error