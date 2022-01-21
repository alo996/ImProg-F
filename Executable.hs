module Executable where
    import Declarations
    import Tokenizer
    import Parser
    import Store 
    import Compiler
    import MiniMF
    import Debug.Trace

    main :: String -> IO String
    main input = 
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> case compileProgram (fst ast) of
                    ErrorState error -> return error
                    state            -> return $ show $ interpret state
                Left error -> return error
            Left error -> return error

    main' :: String -> IO String
    main' input = case tokenize input of
        Right tokens -> case program tokens of
            Right ast  -> case compileProgram (fst ast) of
                    ErrorState error -> return error
                    state            -> return $ show state
            Left error -> return error
        Left error -> return error
    
    main'' :: String -> IO String
    main'' input = case tokenize input of
        Right tokens -> case program tokens of
            Right ast  -> return $ show ast
            Left error -> return error
        Left error -> return error
