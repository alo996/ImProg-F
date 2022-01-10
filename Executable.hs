module Executable where
    import Declarations
    import Tokenizer
    import Parser
    import Store 
    import Compiler
    import MiniMF
    import Debug.Trace

    execute :: IO String
    execute = do
        print "Please enter F program:"
        input <- getLine
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> case compileProgram (fst ast) of
                    ErrorState error -> return error
                    state            -> return $ show $ interpret state
                Left error -> return error
            Left error -> return error

    execute' :: IO String
    execute' = do 
        print "Please enter F program:"
        input <- getLine
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> case compileProgram (fst ast) of
                    ErrorState error -> return error
                    state            -> return $ show state
                Left error -> return error
            Left error -> return error