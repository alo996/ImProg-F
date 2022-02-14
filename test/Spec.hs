{- |
Module      : Main
Description : This module contains tests of incorrect F programs to demonstrate error handling in our implementation.
-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Compiler (compileProgram)
import Declarations (State(ErrorState, code, State))
import MF (interpret, resultToString, interpretVerbose)
import Parser (defsToString, program)
import Tokenizer (tokenize, tokensToString)


-- | Test a set of incorrect F programs.
main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [tokenizerTest1, tokenizerTest2, tokenizerTest3, parserTest1, parserTest2, parserTest3,compilerTest1, compilerTest2, interpreterTest1, interpreterTest2, interpreterTest3, interpreterTest4])

-- | User did not enter anything.
tokenizerTest1 :: TestTree
tokenizerTest1 = testCase "Testing tokenizer"
  (assertEqual "Should return a lexical error."
  (Left "Runtime error: Can not compile empty program.")
  (tokenize ""))

-- | An unknown character was used.
tokenizerTest2 :: TestTree
tokenizerTest2 = testCase "Testing tokenizer"
  (assertEqual "Should return a lexical error." 
  (Left "Lexical error in line 1: Illegal character '$'.")
  (tokenize "main x = if x == 1 then $ true else false"))

-- | An identifier begins with one or more integers.
tokenizerTest3 :: TestTree
tokenizerTest3 = testCase "Testing tokenizer"
  (assertEqual "Should return a lexical error." 
  (Left "Lexical error in line 1: Identifiers must not begin with a digit.")
  (tokenize "main = 3f x; 3f x = x;"))

-- | The user forgot a semicolon at the end of a definition.
parserTest1 :: TestTree
parserTest1 = testCase "Testing parser"
  (assertEqual "Should return a parse error."
  ("Syntax error at end of program: Keyword ';' expected.")
  (parseString "main = f 3; f x = 3 * x"))

-- | 'If-then-else' construct was used incorrectly.
parserTest2 :: TestTree
parserTest2 = testCase "Testing parser"
  (assertEqual "Should return a parse error."
  ("Syntax error in line 1: Keyword 'else' expected but found ';'.")
  (parseString "main = f 4; f x = if x == 3 then true;"))

-- | Mismatched brackets.
parserTest3 :: TestTree
parserTest3 = testCase "Testing parser"
  (assertEqual "Should return a parse error."
  ("Syntax error in line 1: Keyword ';' expected but found ')'.")
  (parseString "main = f 4; f x = (-1) * x);"))

-- | Function 'main' was defined incorrectly.
compilerTest1 :: TestTree
compilerTest1 = testCase "Testing compiler"
  (assertEqual "Should return a runtime error."
  ("Runtime error: Function 'main' is not correctly defined.")
  (compileString "main x = 3 * x;"))

-- | Multiple definitions of a function.
compilerTest2:: TestTree
compilerTest2 = testCase "Testing compiler"
  (assertEqual "Should return a runtime error."
  ("Runtime error: Multiple declarations of function 'f'.")
  (compileString "main = f 4; f x = 2 * x; f y = 2 * y;"))

-- | A called function has not been defined.
interpreterTest1:: TestTree
interpreterTest1 = testCase "Testing interpreter"
  (assertEqual "Should return a runtime error."
  ("Runtime error: Function 'y' not found.")
  (interpretString "main = f 4; f x = g y;"))

-- | Incorrect types of operands.
interpreterTest2:: TestTree
interpreterTest2 = testCase "Testing interpreter"
  (assertEqual "Should return a runtime error."
  ("Runtime error: Integers 3 and 4 can not be evaluated with operator &.")
  (interpretString "main = f 4; f x = 3 & x;"))

-- | No boolean value after 'if' expression.
interpreterTest3:: TestTree
interpreterTest3 = testCase "Testing interpreter"
  (assertEqual "Should return a runtime error."
  ("Runtime error: 'If' expression expects boolean value.")
  (interpretString "main = f 4; f x = if 2 then true else false;"))

-- | Division by zero.
interpreterTest4:: TestTree
interpreterTest4 = testCase "Testing interpreter"
  (assertEqual "Should return a runtime error."
  ("Runtime error: Division by zero.")
  (interpretString "main = 3 / 0;"))


---------------------------------------- HELPER FUNCTIONS FOR TEST SUITE ----------------------------------------
parseString :: String -> String
parseString x = case tokenize x of
    Right toks -> case program toks of
        Right prog -> show prog
        Left error -> error
    Left error -> error

compileString :: String -> String
compileString x = case tokenize x of
    Right toks -> case program toks of
        Right prog -> case compileProgram (fst prog) of
            ErrorState error -> error
            s                -> show s
        Left error -> error
    Left error -> error

interpretString :: String -> String
interpretString x = case tokenize x of
    Right toks -> case program toks of
        Right prog -> case compileProgram (fst prog) of
            ErrorState error -> error
            s                -> case interpret s of
                ErrorState error -> error
                s            -> show s
        Left error -> error
    Left error -> error
