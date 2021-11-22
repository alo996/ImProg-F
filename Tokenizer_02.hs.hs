module Tokenizer
(tokenizeInterface)
where

    import Declarations
    import Data.List
    import Data.Char

    -- data Token = VariableToken | KeywordToken | NumberToken | BoolToken | NewLine
    -- data VariableToken = Name String
    -- data KeywordToken = Sem | Equ | Add | Sub | Let | If | Elese | Then | LBracket | RBracket-- .....
    -- data NumberToken = Number Int
    -- data BoolToken = Boolean Bool
    -- !! structural typing does not work this way


    isKeyword:: Char -> Bool -- is cahracter a keyword or probably a name
    isKeyword c = isAlphaNum c || c `elem` [';', '=', '(', ')', '&', '|', '+', '-', '*', '/', '\n', '<', '>']


    tokenizeInterface:: String -> [(Token,Int)]  -- Token and line number
    tokenizeInterface "" = [] -- empty Code
    tokenizeInterface xs =  reverse $ tokenize (filter (\a -> a `notElem` ['\r']) xs) "" [] 1

    -- tokenizes parameters are: the input string, a name accumulator, a line counter and a result accumulator.
    tokenize :: String -> String -> [(Token,Int)] -> Int -> [(Token,Int)]
    --          input     nameAcc   tokenAcc     lineCounter   result

    -- NEW LINE
    tokenize ('\n' : xs) "" tokenAcc lineCounter = tokenize xs "" tokenAcc (lineCounter + 1)
    tokenize ('\r' : xs) "" tokenAcc lineCounter = tokenize xs "" tokenAcc (lineCounter + 1)

    -- EMPTY input, NO NAME MODE
    tokenize "" "" tokenAcc lineCounter = tokenAcc

    -- WHITESPACE ignored
    tokenize (' ':xs) "" tokenAcc lineCounter = tokenize xs "" tokenAcc lineCounter -- nichts besonderes (kein Name abschließen)



    -- PATTERNS
    tokenize ('=' : '=' : xs) "" tokenAcc lineCounter                   = tokenize xs "" ((KeywordToken Equals , lineCounter) : tokenAcc) lineCounter
    tokenize ('>' : '=' : xs) "" tokenAcc lineCounter                   = tokenize xs "" ((KeywordToken Equals , lineCounter) : tokenAcc) lineCounter
    tokenize ('<' : '=' : xs) "" tokenAcc lineCounter                   = tokenize xs "" ((KeywordToken Equals , lineCounter) : tokenAcc) lineCounter
    tokenize ('n' : 'o' : 't' : xs) "" tokenAcc lineCounter             = tokenize xs "" ((KeywordToken Not , lineCounter) : tokenAcc) lineCounter
    tokenize ('l' : 'e' : 't' : xs) "" tokenAcc lineCounter             = tokenize xs "" ((KeywordToken Let , lineCounter) : tokenAcc) lineCounter
    tokenize ('i' : 'f' : xs) "" tokenAcc lineCounter                   = tokenize xs ""((KeywordToken If , lineCounter) : tokenAcc) lineCounter
    tokenize ('T' : 'r' : 'u' : 'e' : xs) "" tokenAcc lineCounter       = tokenize xs "" ((BooleanToken True , lineCounter) : tokenAcc) lineCounter
    tokenize ('F' : 'a' : 'l' : 's' : 'e' : xs) "" tokenAcc lineCounter = tokenize xs "" ((BooleanToken False , lineCounter) : tokenAcc) lineCounter
    -- tokenize ('\' : 'n' : xs) "" tokenAcc lineCounter = tokenize xs "" ((BooleanToken False , lineCounter) : tokenAcc) lineCounter

    -- SINGLE TOKENS
    tokenize (x:xs) "" tokenAcc lineCounter = case x of
          ';' -> tokenize xs "" ((KeywordToken Semicolon , lineCounter) : tokenAcc) lineCounter
          '=' -> tokenize xs "" ((KeywordToken Assign , lineCounter) : tokenAcc) lineCounter
          '(' -> tokenize xs "" ((KeywordToken LBracket , lineCounter) : tokenAcc) lineCounter
          ')' -> tokenize xs "" ((KeywordToken RBracket , lineCounter) : tokenAcc) lineCounter
          '&' -> tokenize xs "" ((KeywordToken And , lineCounter) : tokenAcc) lineCounter
          '|' -> tokenize xs "" ((KeywordToken Or , lineCounter) : tokenAcc) lineCounter
          '+' -> tokenize xs "" ((KeywordToken Plus, lineCounter) : tokenAcc) lineCounter
          '-' -> tokenize xs "" ((KeywordToken Minus , lineCounter) : tokenAcc) lineCounter
          '*' -> tokenize xs "" ((KeywordToken Times, lineCounter) : tokenAcc) lineCounter
          '/' -> tokenize xs "" ((KeywordToken Divide , lineCounter) : tokenAcc) lineCounter
          x -> tokenize xs (x:"") tokenAcc lineCounter -- NAME MODE beginning

    tokenize (x:xs) nameAcc tokenAcc lineCounter = case x of
          ';' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Semicolon , lineCounter) : tokenAcc) lineCounter
          '=' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Assign , lineCounter) : tokenAcc) lineCounter
          '(' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken LBracket , lineCounter) : tokenAcc) lineCounter
          ')' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken RBracket , lineCounter) : tokenAcc) lineCounter
          '&' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken And , lineCounter) : tokenAcc) lineCounter
          '|' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Or , lineCounter) : tokenAcc) lineCounter
          '+' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Plus, lineCounter) : tokenAcc) lineCounter
          '-' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Minus , lineCounter) : tokenAcc) lineCounter
          '*' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Times, lineCounter) : tokenAcc) lineCounter
          '/' -> tokenize xs "" ((decideNameOrNum nameAcc lineCounter) : (KeywordToken Divide , lineCounter) : tokenAcc) lineCounter
          x -> tokenize xs (x:"") tokenAcc lineCounter -- NAME MODE beginning



    -- NAME MODE
    tokenize (' ':xs) nameAcc tokenAcc lineCounter = tokenize xs "" ((decideNameOrNum nameAcc lineCounter):tokenAcc) lineCounter -- NAME MODE whitespace
    tokenize "" nameAcc tokenAcc lineCounter = (decideNameOrNum nameAcc lineCounter):tokenAcc
    tokenize (x:xs) nameAcc tokenAcc lineCounter = tokenize xs (x:nameAcc) tokenAcc lineCounter -- NAME MODE add to nameAcc


    decideNameOrNum nameAcc lineCounter = if all (\x -> isDigit x) nameAcc
        then (NumberToken $ (numToString $ reverse nameAcc) , lineCounter) -- NAME MODE empty input
        else (NameToken $ reverse nameAcc, lineCounter)

    numToString :: [Char] -> Integer
    numToString numString = read numString :: Integer


        -- NAME MODE name zwischen number string unterscheiden und numberstring zu Num umwandeln! --> NumberToken Num statt String
