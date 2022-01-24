module Compiler where
    import Declarations
    import Store

    -- Compile a whole program. 'compileProgram' takes a program (a list of definitions) and returns the the initial machine state that is then interpreted by MiniMF.
    compileProgram :: [Def] -> State
    compileProgram defs = let s = State {pc = 0, sp = -1, code = Code [Reset, Pushfun "main", Call, Halt, Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator 2, OpUpdate, Return, Pushparam 1, Unwind, Call, Operator 3, OpUpdate, Unwind, Call, Return, Pushparam 1, Unwind, Call, Operator 1, OpUpdate, Return], stack = Stack [], global = Global [], heap = Heap []} in compileProgram' defs s where
        -- 'compileProgram'' is a helper method to recursively go through all definitions of the program, compile each one and then return the initial state. 
        compileProgram' (d : ds) state = compileProgram' ds (compileDefinition d state)
        compileProgram' [] state       = state -- initial state

    -- Compile a definition. 'compileDefinition' takes the definition to compile and the current machine state and returns the updated machine state with the new stack and global environment.
    compileDefinition :: Def -> State -> State
    compileDefinition (Def (AtomicExpr (Var fname)) es e) s@State{code = Code ccells, global = Global gcells, heap = Heap hcells} =
        let localenv = createPos es; defcell = [DEF fname (length localenv) (depth (code s))] in
        s {code = Code (ccells ++ compileExpression e localenv ++ [FuncUpdate (length localenv), Slide (length localenv + 1), Unwind, Call, Return]), global = Global (gcells ++ defcell), heap = Heap (hcells ++ defcell)}
    compileDefinition def state = ErrorState "Error in 'compileDefinition'"

    -- Compile an expression. 'compileExpression' takes the expression to compile and a local environment.
    compileExpression :: Expr -> [(Expr, Int)] -> [Instruction]
    compileExpression e pos = case e of
        AtomicExpr (LitBool (BoolF False)) -> [Pushval "Bool" 0]
        AtomicExpr (LitBool (BoolF True))  -> [Pushval "Bool" 1]
        AtomicExpr (LitNum num)            -> [Pushval "Int" num]
        AtomicExpr (Var name)              -> case posInd e pos of
            Right 0    -> [Pushfun name]
            Right ind  -> [Pushparam ind]
            Left error -> [Error error]
        AtomicExpr (Expr expr)             -> compileExpression expr pos
        Func (AtomicExpr (Var fname)) e2   -> compileExpression e2 pos ++ [Pushfun fname, Makeapp]
        Func e1 e2                         -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Makeapp]
        LetIn localdefs e                  -> compileLocalDefinitions localdefs e pos
        Add e1 e2                          -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Plus, Makeapp, Makeapp]
        Mult e1 e2                         -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Times, Makeapp, Makeapp]
        Div e1 e2                          -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Divide, Makeapp, Makeapp]
        Equal e1 e2                        -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Equals, Makeapp, Makeapp]
        LessThan e1 e2                     -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Less, Makeapp, Makeapp]
        LogicalAnd e1 e2                   -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre And, Makeapp, Makeapp]
        LogicalOr e1 e2                    -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre Or, Makeapp, Makeapp]
        UnaryMin e                         -> compileExpression e pos ++ [Pushpre Minus, Makeapp]
        LogicalNot e                       -> compileExpression e pos ++ [Pushpre Not, Makeapp]
        IfThenElse e1 e2 e3                -> compileExpression e3 pos ++ compileExpression e2 (posInc pos 1) ++ compileExpression e1 (posInc pos 2) ++ [Pushpre If, Makeapp, Makeapp, Makeapp]

    compileLocalDefinitions :: [LocalDef] -> Expr -> [(Expr, Int)] -> [Instruction]
    compileLocalDefinitions ldefs e pos = 
        let n = length ldefs in let pos' = createPos' ldefs pos n in
            compileLocalDefinitions' ldefs e (replicate' [Alloc, Alloc, Makeapp] n) pos' n n where
                compileLocalDefinitions' :: [LocalDef] -> Expr -> [Instruction] -> [(Expr, Int)] -> Int -> Int -> [Instruction]
                compileLocalDefinitions' ((LocalDef e1 e2) : defs) e ccells pos' n acc = compileLocalDefinitions' defs e (ccells ++ compileExpression e2 pos' ++ [UpdateLet $ acc-1]) pos' n (acc-1)
                compileLocalDefinitions' [] e ccells pos' n _                          = ccells ++ compileExpression e pos' ++ [SlideLet n]

    -- Create a local environment for a given list of formal parameters.
    createPos :: [Expr] -> [(Expr, Int)]
    createPos es = zip es [1..]

    -- Get the index of the first occurence of a formal parameter in a given local environment.
    posInd :: Expr -> [(Expr, Int)] -> Either String Int
    posInd e []  = return 0
    posInd e pos = case lookup e pos of
        Just ind -> return ind
        _        -> Left $ "Compile error: Local environment " ++ show pos ++ " does not contain formal parameter " ++ show e ++ "."

    -- Increment all indices in a local environment by an offset n.
    posInc :: [(Expr, Int)] -> Int -> [(Expr, Int)]
    posInc pos n = map (\ (a, b) -> (a, b + n)) pos

    -- Extend a local environment by adding let bindings to its front.
    createPos' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
    createPos' ldefs pos n = let pos' = posInc pos n in posList' ldefs pos' (length ldefs - 2) where
        posList' ((LocalDef e1 e2) : ldefs) pos ind = posList' ldefs ((e1, ind) : pos) (ind - 1)
        posList' [] pos ind                         = pos
    
    -- Replicate a list n-1 times and concatenate.
    replicate' :: [a] -> Int -> [a]
    replicate' xs n 
        | n > 1     = xs ++ replicate' xs (n - 1)
        | otherwise = xs
