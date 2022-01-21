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
    compileProgram defs = let s = State {pc = 0, sp = -1, code = Code [Reset, Pushfun "main", Call, Halt, Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator 2, OpUpdate, Return, Pushparam 1, Unwind, Call, Operator 3, OpUpdate, Unwind, Call, Return, Pushparam 1, Unwind, Call, Operator 1, OpUpdate, Return], stack = Stack [], heap = Heap []} in compileProgram' defs s where
        -- 'compileProgram'' is a helper method to recursively go through all definitions of the program, compile each one and then return the initial state. 
        compileProgram' (d : ds) state = compileProgram' ds (compileDefinition d state)
        compileProgram' [] state       = state -- initial state

    -- Compile a definition. 'compileDefinition' takes the definition to compile and the current machine state and returns the updated machine state with the new stack and global environment.
    compileDefinition :: Def -> State -> State
    compileDefinition (Def e1@(AtomicExpr (Var fname)) es e2) s@State{code = Code ccells, heap = Heap hcells} =
        let localenv = createPos es in
        s {code = Code (ccells ++ compileExpression e2 0 localenv ++ [FuncUpdate (length localenv), Slide (length localenv + 1), Unwind, Call, Return]), heap = Heap (hcells ++ [DEF fname (length localenv) (depth (code s))])}
    compileDefinition def state = ErrorState $ "Compile error: compileDefinition called with " ++ show def ++ " and " ++ show state ++ "."

    compileLocalDefinitions :: [LocalDef] -> Expr -> [(Expr, Int)] -> [Instruction]
    compileLocalDefinitions ldefs e pos = 
        let n = length ldefs in let pos' = createPos' ldefs pos n in
            compileLocalDefinitions' ldefs e (replicate' [Alloc, Alloc, Makeapp] n) pos' n n where
                compileLocalDefinitions' :: [LocalDef] -> Expr -> [Instruction] -> [(Expr, Int)] -> Int -> Int -> [Instruction]
                compileLocalDefinitions' ((LocalDef e1 e2) : defs) e cells pos' n acc = compileLocalDefinitions' defs e (compileExpression e2 n pos' ++ [UpdateLet $ acc-1]) pos' n (acc-1)
                compileLocalDefinitions' [] e ccells pos' n _                         = ccells ++ compileExpression e 0 pos' ++ [SlideLet n]

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
            Func (AtomicExpr (Var fname)) e2   -> compileExpression e2 num pos ++ [Pushfun fname, Makeapp]
            Func e1 e2                         -> compileExpression e2 num pos ++ compileExpression e1 (num + 1) pos ++ [Makeapp]
            LetIn localdefs e                  -> compileLocalDefinitions localdefs e pos
            _                                  -> [Error "Illegal MF instruction"]

            {-
                data LocalDef 
                = LocalDef Expr Expr 
                deriving (Eq, Show)
            -}

    -- Create a local environment for a given list of formal parameters.
    createPos :: [Expr] -> [(Expr, Int)]
    createPos es = zip es [1..]

    -- Get the index of the first occurence of a formal parameter in a given local environment.
    posInd :: Expr -> Int -> [(Expr, Int)] -> Either String Int
    posInd e offset pos = case lookup e pos of
        Just ind -> return (ind + offset)
        _        -> Left $ "Compile error: Local environment " ++ show pos ++ " does not contain formal parameter " ++ show e ++ " at position " ++ show pos ++ "."

    -- Increment all indices in a local environment by an offset n.
    posInc :: [(Expr, Int)] -> Int -> [(Expr, Int)]
    posInc pos n = map (\ (a, b) -> (a, b + n)) pos

    -- Extend a local environment by adding let bindings to its front.
    createPos' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
    createPos' ldefs pos n = let pos' = posInc pos n in posList' ldefs pos' (length ldefs - 2) where
        posList' ((LocalDef e1 e2) : ldefs) pos ind = posList' ldefs ((e1, ind) : pos) (ind - 1)
        posList' [] pos ind                         = pos
    
    -- Replicate a list n-1 times and concatenate them.
    replicate' :: [a] -> Int -> [a]
    replicate' xs n 
        | n > 1     = xs ++ replicate' xs (n - 1)
        | otherwise = xs