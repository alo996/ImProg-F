{-# LANGUAGE NamedFieldPuns #-}
module MF where
    import Declarations
    import Tokenizer
    import Parser
    import Compiler
    import Store
    import Data.Bits
    import Debug.Trace
    
    ---------------------------------------- HAUPTZYKLUS ----------------------------------------
    -- 'interpret' recursively executes a set of instructions either a HALT instruction or an error occurs.
    -- trace (show (ccells !! pc) ++ ", pc = " ++ show pc ++ ", sp = " ++ show sp ++ ", stack = " ++ show stack ++ ", heap = " ++ show heap) use this to debug
    interpret :: State -> State
    interpret s@State{pc, sp, code = Code ccells, stack, global, heap} = case trace (show s) accessCode (Code ccells) pc of
        Right instruction -> case instruction of
            Halt -> s
            _    -> case run instruction s of
                ErrorState error -> ErrorState error
                state            -> interpret state
        Left error        -> ErrorState error
    interpret (ErrorState error) = ErrorState error

    -- Given an instruction, 'run' executes the respective MF function.
    run :: Instruction -> State -> State
    run Reset state               = reset state
    run (Pushfun fname) state     = pushfun state fname
    run (Pushval typ field) state = case typ of
        "Bool" -> pushval state 2 field
        _      -> pushval state 1 field
    run (Pushparam addr) state    = pushparam state addr
    run (Pushpre kw) state        = pushpre state kw
    run Makeapp state             = makeapp state
    run (Slide n) state           = slide state n
    run Unwind state              = unwind state
    run Call state                = call state
    run Return state              = return'' state
    run Halt state                = halt state
    run (Operator n) state        = operator state n
    run Alloc state               = alloc state
    run (FuncUpdate n) state      = funcUpdate state n
    run OpUpdate state            = opUpdate state
    run (UpdateLet n) state       = updateLet state n
    run (SlideLet n) state        = slideLet state n
    run (Error error) state       = ErrorState error

    result :: State -> String
    result State{pc, sp, code, stack, global, heap} = case accessStack stack sp of
        Right (StackCell addr) -> case accessHeap heap addr of
            Right (VALNum n)  -> "---> Result: " ++ show n
            Right (VALBool b) -> case b of
              0 -> "---> Result: False"
              _ -> "---> Result: True"
            Left error -> "---> Error: " ++ error
            _          -> "---> Internal error."
        Left error -> "---> Error: " ++ error
    result (ErrorState error)                       = "---> Error: " ++ error

    ---------------------------------------- HELPER FUNCTIONS ----------------------------------------
    -- 'address' takes a function name, looks for the corresponding DEF-cell in the global environment and returns the address of the similar DEF-cell in the heap.
    address :: Global -> String -> Either String Int
    address (Global gcells) f = address' gcells f where
        address' ((fname, pos) : xs) f = if fname == f then return pos else address' xs f
        address' [] f                  = Left $ "Runtime error: Global environment does not contain definition of function '" ++ show f ++ "'."

    -- 'add2arg' takes the address of an APP-cell and delivers the heap address of its argument.
    add2arg :: Heap -> Int -> Either String Int
    add2arg h@(Heap hcells) addr = if (addr >= 0) && (addr < length hcells) then let APP addr1 addr2 = hcells !! addr in return addr2 else Left $ "Runtime error: Pushparam expected heap address, found code address " ++ show addr ++ " instead."

    -- 'arity'' returns the number of formal arguments of an operator.
    arity' :: Keyword -> Int
    arity' kw = case kw of
        If     -> 3
        And    -> 2
        Or     -> 2
        Equals -> 2
        Less   -> 2
        Plus   -> 2
        Minus  -> 1
        Times  -> 2
        Divide -> 2
        Not    -> 1
        _      -> 0  -- not nice!!!!

    -- 'new...' functions create different types of heap cells.
    newAPP, newVAL :: Heap -> Int -> Int -> (Int, Heap)
    newAPP h@(Heap hcells) a b = (length hcells, pushHeap h (APP a b))

    newVAL h@(Heap hcells) t w
            | t == 2    = (length hcells, pushHeap h (VALBool w))
            | otherwise = (length hcells, pushHeap h (VALNum w))

    newIND :: Heap -> Int -> (Int, Heap)
    newIND h@(Heap hcells) a = (length hcells, pushHeap h (IND a))

    newPRE :: Heap -> Keyword -> Int -> (Int, Heap)
    newPRE h@(Heap hcells) kw n = (length hcells, pushHeap h $ PRE kw $ arity' kw)

    newUNI :: Heap -> (Int, Heap)
    newUNI h@(Heap hcells) = (length hcells, pushHeap h UNINITIALIZED)


    ---------------------------------------- MF FUNCTIONS ----------------------------------------
    reset :: State -> State
    reset s = s {sp = -1, pc = pc s + 1}

    pushfun :: State -> String -> State
    pushfun s name = case address (global s) name of
        Right int  -> s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell int)}
        Left error -> ErrorState error

    pushval :: State -> Int -> Int -> State
    pushval s t w = let tuple = newVAL (heap s) t w in s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell (fst tuple)), heap = snd tuple}

    pushparam :: State -> Int -> State
    pushparam s n = case accessStack (stack s) (sp s - n - 1) of
        Right (StackCell addr) -> case add2arg (heap s) addr of
            Right addr -> case saveStack (stack s) (StackCell addr) (sp s + 1) of
                Right stack -> s {pc = pc s + 1, sp = sp s + 1, stack = stack}
                Left error  -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error

    makeapp :: State -> State
    makeapp s = case accessStack (stack s) (sp s) of
        Right (StackCell a) -> case accessStack (stack s) (sp s - 1) of
            Right (StackCell b) -> let tuple = newAPP (heap s) a b in
                case saveStack (stack s) (StackCell (fst tuple)) (sp s - 1) of
                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                    Left error           -> ErrorState error
            Left error         -> ErrorState error
        Left error         -> ErrorState error

    slide :: State -> Int -> State
    slide s n = case accessStack (stack s) (sp s - 1) of
        Right scell_1 -> case saveStack (stack s) scell_1 (sp s - n - 1) of
            Right stack_1 -> case accessStack stack_1 (sp s) of
                Right scell_2 -> case saveStack stack_1 scell_2 (sp s - n) of
                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
                    Left error           -> ErrorState error
                Left error    -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error

    halt :: State -> State
    halt s = s

    value :: Heap -> Int -> Either String HeapCell
    value heap adr1 = case accessHeap heap adr1 of
        Right (IND adr2) -> value heap adr2
        Right hcell      -> Right hcell
        Left error       -> Left error

    unwind :: State -> State
    unwind s = case accessStack (stack s) (sp s) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (APP adr1 adr2) -> s {sp = sp s + 1, stack = pushStack (stack s) (StackCell adr1)}
            Right hcell           -> s {pc = pc s + 1}
            Left error            -> ErrorState error
        Left error             -> ErrorState error

    call :: State -> State
    call s = case accessStack (stack s) (sp s) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (DEF _ _ adr) -> s {pc = adr, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
            Right (PRE op 2)    -> s {pc = 4, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
            Right (PRE If 3)    -> s {pc = 13, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
            Right (PRE op 1)    -> s {pc = 21, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
            Right (VALNum _)    -> s {pc = pc s + 1}
            Right (VALBool _)   -> s {pc = pc s + 1}
            Left error          -> ErrorState error
            _                   -> ErrorState "error"
        Left error             -> ErrorState error

    return'' :: State -> State
    return'' s = case accessStack (stack s) (sp s - 1) of
        Right (StackCell addr) -> case accessStack (stack s) (sp s) of
            Right (StackCell addr1) -> case saveStack (stack s) (StackCell addr1) (sp s - 1) of
                Right (Stack scells) -> s {pc = addr, sp = sp s - 1, stack = Stack (take (sp s) scells)}
                Left error           -> ErrorState error
            Left error             -> ErrorState error
        Left error            -> ErrorState error

    pushpre :: State -> Keyword -> State
    pushpre s kw = let tuple = newPRE (heap s) kw (arity' kw) in
        case saveStack (stack s) (StackCell (fst tuple)) (sp s + 1) of
                Right stack -> s{pc = pc s + 1, sp = sp s + 1, stack = stack, heap = snd tuple}
                Left error   -> ErrorState error

    funcUpdate :: State -> Int -> State
    funcUpdate s arg = case accessStack (stack s) (sp s) of
        Right (StackCell addr) -> let hcell = IND addr in case accessStack (stack s) (sp s - arg - 2) of
            Right (StackCell addr1) -> case saveHeap (heap s) hcell addr1 of
                Right heap -> s {pc = pc s + 1, heap = heap}
                Left error -> ErrorState error
            Left error              -> ErrorState error
        Left error             -> ErrorState error

    opUpdate :: State -> State
    opUpdate s = case accessStack (stack s) (sp s) of
        Right (StackCell addr1) -> case accessHeap (heap s) addr1 of
            Right hcell -> case accessStack (stack s) (sp s - 2) of
                Right (StackCell addr2) -> case saveHeap (heap s) hcell addr2 of
                    Right heap -> case accessStack (stack s) (sp s - 2) of
                        Right aux -> case accessStack (stack s) (sp s - 1) of
                            Right scell -> case saveStack (stack s) scell (sp s - 2) of
                                Right stack -> case saveStack stack aux (sp s - 1) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                                    Left error           -> ErrorState error
                                Left error             -> ErrorState error
                            Left error            -> ErrorState error
                        Left error            -> ErrorState error
                    Left error            -> ErrorState error
                Left error            -> ErrorState error
            Left error            -> ErrorState error
        Left error            -> ErrorState error

    operator :: State -> Int -> State
    operator s op = case op of
        1 -> case accessStack (stack s) (sp s - 2) of
            Right (StackCell addr1) -> case value (heap s) addr1 of
                Right (PRE op 1) -> case accessStack (stack s) (sp s - 1) of
                    Right scell -> case saveStack (stack s) scell (sp s - 2) of
                        Right stack   -> case accessStack stack (sp s) of
                            Right (StackCell addr2) -> case value (heap s) addr2 of
                                Right (VALBool w) -> case op of
                                    Not -> if fromEnum w == 1
                                                then let tuple = newVAL (heap s) 1 1 in case saveStack stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    _                    -> ErrorState "Runtime error: Cannot execute Operator 1."
                                                else let tuple = newVAL (heap s) 1 0 in case saveStack stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    _                    -> ErrorState "Runtime error: Cannot execute Operator 1."
                                    _   -> ErrorState "Runtime error: Cannot execute Operator 1."
                                Right (VALNum w)  -> case op of
                                    Minus -> let tuple = newVAL (heap s) 0 (- w) in case saveStack stack (StackCell (fst tuple)) (sp s - 1) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                        _     -> ErrorState "Runtime error: Cannot execute Operator 1."
                                    _         -> ErrorState "Runtime error: Cannot execute Operator 1."
                                _                 -> ErrorState "Runtime error: Cannot execute Operator 1."
                            Left error -> ErrorState error
                        Left error -> ErrorState error
                    Left error  -> ErrorState error
                _               -> ErrorState "Runtime error: Cannot execute Operator 1."
            Left error  -> ErrorState error
        2 -> case accessStack (stack s) (sp s - 3) of
            Right (StackCell addr) -> case value (heap s) addr of
                Right (PRE op 2) -> case accessStack (stack s) (sp s - 2) of
                    Right scell -> case saveStack (stack s) scell (sp s - 4) of
                        Right stack -> case accessStack stack (sp s - 1) of
                            Right (StackCell addr2) -> let hcell1 = value (heap s) addr2 in
                                case accessStack stack (sp s) of
                                    Right (StackCell addr3) -> let hcell2 = value (heap s) addr3 in
                                        case hcell1 of
                                            Right (VALBool bool1) -> case hcell2 of
                                                Right (VALBool bool2) -> case op of
                                                    And    -> let tuple = newVAL (heap s) 2 (bool1 .&. bool2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Or     -> let tuple = newVAL (heap s) 2 (bool1 .|. bool2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Equals -> let tuple = newVAL (heap s) 2 (0 .&. xor bool1 bool2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Less   -> let tuple = newVAL (heap s) 2 (fromEnum $ bool1 < bool2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState error
                                                    _      -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                _                     -> ErrorState "Runtime error: Cannot execute Operator 2."
                                            Right (VALNum num1) -> case hcell2 of
                                                Right (VALNum num2) -> case op of
                                                    Equals -> let tuple = newVAL (heap s) 2 (fromEnum $ num1 == num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Less   -> let tuple = newVAL (heap s) 2 (fromEnum $ num1 < num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Plus   -> let tuple = newVAL (heap s) 1 (num1 + num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Times  -> let tuple = newVAL (heap s) 1 (num1 * num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Minus  -> let tuple = newVAL (heap s) 1 (num1 - num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                    Divide -> let tuple = newVAL (heap s) 1 (num1 `div` num2) in
                                                        case saveStack stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error           -> ErrorState error
                                                    _      -> ErrorState "Runtime error: Cannot execute Operator 2."
                                                _                    -> ErrorState "Runtime error: Cannot execute Operator 2."
                                            _                   -> ErrorState "Runtime error: Cannot execute Operator 2."
                                    Left error          -> ErrorState "Runtime error: Cannot execute Operator 2."
                            Left error          -> ErrorState "Runtime error: Cannot execute Operator 2."
                        Left error          -> ErrorState "Runtime error: Cannot execute Operator 2."
                    Left error          -> ErrorState "Runtime error: Cannot execute Operator 2."
                _                -> ErrorState "Runtime error: Cannot execute Operator 2."
            Left error          -> ErrorState error
        3 -> case accessStack (stack s) (sp s) of
            Right (StackCell addr) -> case trace ("operator 3 calls " ++ show (value (heap s) addr)) value (heap s) addr of
                Right (VALBool bool) -> case bool of
                    1  -> case accessStack (stack s) (sp s - 4) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case saveStack (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case accessStack stack (sp s - 1) of
                                    Right scell -> case saveStack stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $ (take $ sp s - 2) scells}
                                        _                    -> ErrorState "Runtime error: Cannot execute Operator 3."
                                    Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                                Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                            Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                        Left error              -> ErrorState "Runtime error: Cannot execute Operator 3."
                    0 -> case accessStack (stack s) (sp s - 5) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case saveStack (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case accessStack stack (sp s - 1) of
                                    Right scell -> case saveStack stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $(take $ sp s - 3) scells}
                                        _                    -> ErrorState "Runtime error: Cannot execute Operator 3."
                                    Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                                Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                            Left error  -> ErrorState "Runtime error: Cannot execute Operator 3."
                        Left error              -> ErrorState "Runtime error: Cannot execute Operator 3."
                    _ -> ErrorState "Runtime error: Cannot execute Operator 3."
                _                    -> ErrorState "Runtime error: Cannot execute Operator 3."

            Left error              -> ErrorState "Runtime error: Cannot execute Operator 3."
        _ -> ErrorState "Runtime error: Cannot execute Operator 3."

    alloc :: State -> State
    alloc s = let tuple = newUNI (heap s) in s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell (fst tuple)), heap = snd tuple}

    updateLet :: State -> Int -> State
    updateLet s@State{stack = Stack scells} n = case accessStack (stack s) (sp s - n - 1) of
        Right (StackCell addr) -> case add2arg (heap s) addr of
            Right addr1 -> case accessStack (stack s) (sp s) of
                Right (StackCell addr2) -> case saveHeap (heap s) (IND addr2) addr1 of
                    Right heap -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                    Left error -> ErrorState error
                Left error              -> ErrorState error
            Left error  -> ErrorState error
        Left error             -> ErrorState error
    updateLet _ _           = ErrorState "error"

    slideLet :: State -> Int -> State
    slideLet s n = case accessStack (stack s) (sp s) of
        Right scell -> case saveStack (stack s) scell (sp s - n) of
            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
            Left error           -> ErrorState error
        Left error  -> ErrorState error