module Compiler where
    --compileProgram :: [Definition] -> State
    -- a program consists of multiple definitions
    -- we have to translate them one by one

    --compileDefinition
    -- page 77 in the script: ÜbDef
    -- translates one definition into a list of instructions
    -- calls translateExpression to translate the body

    --compileExpression
    -- page 78 in the script: ÜbKons
    -- translates one expression into a list of instructions

    -- pay attention to the local environment Pos (page 77)