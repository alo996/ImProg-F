{-# LANGUAGE NamedFieldPuns #-}
module Executable where
    import Declarations
    import Tokenizer
    import Parser
    import Store
    import Compiler
    import MF

    main :: String -> IO ()
    main input =
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> case compileProgram (fst ast) of
                    ErrorState error   -> putStrLn error
                    state@State{code}  -> do
                      print code
                      putStrLn "\nInitial Machine State:"
                      putStrLn $ result $ interpret state
                Left error -> putStrLn error
            Left error -> putStrLn error