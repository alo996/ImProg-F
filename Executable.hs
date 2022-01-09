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
                Right ast  -> return $ show $ compileProgram $ fst ast 
                Left error -> return error
            Left error -> return error

    execute' :: IO String
    execute' = do 
        print "F program"
        input <- getLine
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> return $ show ast
                Left error -> return error
            Left error -> return error
        