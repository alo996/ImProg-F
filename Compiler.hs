{-# LANGUAGE NamedFieldPuns #-}
module Compiler where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe

    {-
    If successful, the parser output is of the form Right ([Def], []). In the executable, we access the first component of the tuple, which is our parsed F-program.
    This list of definitions is the input of the compiler. 
    -}

    -- Compile a whole program. 'compileProgram' takes a program (a list of definitions) and returns the the initial machine state that is then interpreted by MiniMF.
    compileProgram :: [Def] -> State
    compileProgram defs = let s = State {pc = 0, sp = -1, code = Code [Reset, Pushfun "main", Reduce, Halt], stack = Stack [], heap = Heap []} in compileProgram' defs s where
        -- 'compileProgram'' is a helper method to recursively go through all definitions of the program, compile each one and then return the initial state. 
        compileProgram' (d : ds) state = compileProgram' ds (compileDefinition d state)
        compileProgram' [] state       = state -- initial state

    -- Compile a definition. 'compileDefinition' takes the definition to compile and the current machine state and returns the updated machine state with the new stack and global environment.
    compileDefinition :: Def -> State -> State
    compileDefinition (Def e1@(AtomicExpr (Var fname)) es e2) s@State{code = Code ccells, heap = Heap hcells} =
        let localenv = createPos es in
        s {code = Code (ccells ++ compileExpression e2 0 localenv ++ [Slide (length localenv + 1), Reduce, Return]), heap = Heap (hcells ++ [DEF fname (length localenv) (depth (code s))])}
    compileDefinition def state = ErrorState $ "Compile error: compileDefinition called with " ++ show def ++ " and " ++ show state ++ "."

    compileLocalDefinitions :: [LocalDef] -> State -> State
    compileLocalDefinitions ldefs s@State{code = Code ccells} = let pos' = createPos' ldefs
                                                                    n = length ldefs in 
                                                                        compileLocalDefinitions' ldefs s{code = Code (ccells ++ replicate' [Alloc, Alloc, Makeapp] n)} n pos' where
                                                                            compileLocalDefinitions' ((LocalDef e1 e2) : defs) s@State{code = Code ccells} offset pos' = s {code = Code (ccells ++ (compileExpression e2 offset pos') ++ [])}
                                                                            compileLocalDefinitions' [] s _ _                                                  = s
                                                                            compileLocalDefinitions' _ _ _ _                                                    = ErrorState "error"
    compileLocalDefinitions _ _                               = ErrorState "error"

    -- Compile an expression. 'compileExpression' takes the expression to compile, an offset for the local environment (see pos+i(x) in the script) and a local environment.
    compileExpression :: Expr -> Int -> [(Expr, Int)] -> [Instruction]
    compileExpression e num pos = case e of
            AtomicExpr (LitBool (BoolF False)) -> [Pushval "Bool" 0]
            AtomicExpr (LitBool (BoolF True))  -> [Pushval "Bool" 1]
            AtomicExpr (LitNum num)            -> [Pushval "Int" num]
            AtomicExpr (Var name)              -> case posInd e num pos of
                Right ind  -> [Pushparam ind]
                Left error -> [Error error]
            AtomicExpr (Expr expr)             -> compileExpression expr num pos
            Func (AtomicExpr (Var fname)) e2   -> compileExpression e2 num pos ++ [Pushfun $ fname, Makeapp]
            Func e1 e2                         -> compileExpression e2 num pos ++ compileExpression e1 (num + 1) pos ++ [Makeapp]
            _                                  -> [Error $ "Compile error: Invalid MiniF expression " ++ show e ++ "."]

    -- Create a local environment for a given list of formal parameters.
    createPos :: [Expr] -> [(Expr, Int)]
    createPos es = zip es [1..]

    -- Get the index of the first occurence of a formal parameter in a given local environment.
    posInd :: Expr -> Int -> [(Expr, Int)] -> Either String Int
    posInd e offset pos = case lookup e pos of
        Just ind -> return (ind + offset)
        _        -> Left $ "Compile error: Local environment " ++ show pos ++ " does not contain formal parameter " ++ show e ++ " at position " ++ show pos ++ "."

    -- Extend a local environment by adding let bindings to its front.
    createPos' :: [LocalDef] -> [(Expr, Int)] -> [(Expr, Int)]
    createPos' ldefs pos = posList' ldefs pos (length ldefs - 2) where
        posList' ((LocalDef e1 e2) : ldefs) pos ind = posList' ldefs ((e1, ind) : pos) (ind - 1)
        posList' [] pos ind                         = pos

    posInd' :: Expr -> Int -> [(Expr, Int)] -> [(Expr, Int)] -> Either String Int
    posInd' e offset pos' pos = case lookup e pos' of
        Just ind -> return (ind + offset)
        _        -> posInd e offset pos
    
    replicate' :: [a] -> Int -> [a]
    replicate' xs n 
        | n > 1     = xs ++ replicate' xs (n - 1)
        | otherwise = xs