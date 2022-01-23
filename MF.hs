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
    interpret s@State{pc, sp, code = Code ccells, stack, heap} = case access (Code ccells) pc of
        Right instruction -> case instruction of
            Halt -> s
            _    -> case run instruction s of
                ErrorState error -> ErrorState error
                state            -> interpret state
        Left error        -> ErrorState error
    interpret (ErrorState error) = ErrorState error
    interpret _                  = ErrorState "Compile error: Function 'interpret' called with wrong argument."

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
    run Return state              = return' state
    run Halt state                = halt state
    run (Operator n) state        = operator state n
    run Alloc state               = alloc state
    run (FuncUpdate n) state      = funcUpdate state n
    run OpUpdate state            = opUpdate state
    run (UpdateLet n) state       = updateLet state n
    run (SlideLet n) state        = slideLet state n
    run (Error error) state       = ErrorState error


    ---------------------------------------- HELPER FUNCTIONS ----------------------------------------
    -- 'address' takes the function name and delivers the address of its DEF-cell in the global environment.
    address :: Store HeapCell -> String -> Either String Int
    address (Heap hcells) f = address' hcells 0 f where
        address' (x : xs) acc f = case x of
            DEF {fname = fname} -> if f == fname then return acc else address' xs (acc + 1) f
            _                   -> address' xs (acc + 1) f
        address' [] _ _         = Left $ "Compile error: Heap does not contain definition of function '" ++ show f ++ "'."
    address _ _                 = Left "Compile error: Function 'address' not called on heap."

    -- 'add2arg' takes the address of an APP-cell and delivers the heap address of its argument.
    add2arg :: Store HeapCell -> Int -> Either String Int
    add2arg h@(Heap hcells) addr = if (addr >= 0) && (addr < depth h) then let APP addr1 addr2 = hcells !! addr in return addr2 else Left $ "Compile error: Heap does not contain cell at address " ++ show addr ++ "'."
    add2arg _ _                  = Left "Compile error: Function 'add2arg' not called on heap."

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
    newAPP, newVAL :: Store HeapCell -> Int -> Int -> (Int, Store HeapCell)
    newAPP heap a b = (depth heap, push heap (APP a b))

    newVAL heap t w 
            | t == 2    = (depth heap, push heap (VALBool w))
            | otherwise = (depth heap, push heap (VALNum w))

    newIND :: Store HeapCell -> Int -> (Int, Store HeapCell)
    newIND heap a = (depth heap, push heap (IND a))

    newPRE :: Store HeapCell -> Keyword -> Int -> (Int, Store HeapCell)
    newPRE heap kw n = (depth heap, push heap $ PRE kw $ arity' kw)

    newUNI :: Store HeapCell -> (Int, Store HeapCell)
    newUNI heap = (depth heap, push heap UNINITIALIZED)


    ---------------------------------------- MF FUNCTIONS ----------------------------------------
    reset :: State -> State
    reset s = s {sp = -1, pc = pc s + 1}

    pushfun :: State -> String -> State
    pushfun s name = case address (heap s) name of
        Right int  -> s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell int)}
        Left error -> ErrorState error

    pushval :: State -> Int -> Int -> State
    pushval s t w = let tuple = newVAL (heap s) t w in s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell (fst tuple)), heap = snd tuple}

    pushparam :: State -> Int -> State
    pushparam s n = case access (stack s) (sp s - n - 1) of
        Right (StackCell addr) -> case add2arg (heap s) addr of
            Right addr -> case save (stack s) (StackCell addr) (sp s + 1) of
                Right stack -> s {pc = pc s + 1, sp = sp s + 1, stack = stack}
                Left error  -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error
        
    makeapp :: State -> State
    makeapp s = case access (stack s) (sp s) of
        Right (StackCell a) -> case access (stack s) (sp s - 1) of
            Right (StackCell b) -> let tuple = newAPP (heap s) a b in 
                case save (stack s) (StackCell (fst tuple)) (sp s - 1) of
                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                    Left error           -> ErrorState error
                    _                    -> ErrorState "Compile error: save not called on stack in function 'makeapp'"
            Left error         -> ErrorState error
        Left error         -> ErrorState error

    slide :: State -> Int -> State
    slide s n = case access (stack s) (sp s - 1) of
        Right scell_1 -> case save (stack s) scell_1 (sp s - n - 1) of
            Right stack_1 -> case access stack_1 (sp s) of
                Right scell_2 -> case save stack_1 scell_2 (sp s - n) of
                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
                    Left error           -> ErrorState error
                    _                    -> ErrorState "Compile error: save not called on stack in function 'slide'"
                Left error    -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error

    reduce :: State -> State
    reduce s = case access (stack s) (sp s) of
        Right (StackCell addr) -> case access (heap s) addr of
            Right elem -> case elem of
                (APP addr1 addr2) -> s {sp = sp s + 1, stack = push (stack s) (StackCell addr1)}
                (DEF f n addr3)   ->  s {pc = addr3, sp = sp s + 1, stack = push (stack s) (StackCell (pc s + 1))}
                _                 -> case access (stack s) (sp s - 1) of
                    Right (StackCell addr4) -> case access (stack s) (sp s) of
                        Right scell -> case save (stack s) scell (sp s - 1) of
                            Right (Stack scells) -> s {pc = addr4, sp = sp s - 1, stack = Stack (take (sp s) scells)}
                            Left error           -> ErrorState error
                            _                    -> ErrorState "Compile error: save not called on stack in function 'reduce'"
                        Left error           -> ErrorState error
                    Left error           -> ErrorState error
            Left error -> ErrorState error
        Left error           -> ErrorState error

    return' :: State -> State
    return' s = case access (stack s) (sp s - 1) of
        Right (StackCell addr) -> case access (stack s) (sp s) of
            Right scell -> case save (stack s) scell (sp s - 1) of
                Right (Stack scells) -> s {pc = addr, sp = sp s - 1, stack = Stack (take (sp s) scells)}
                Left error           -> ErrorState error
                _                    -> ErrorState "Compile error in return'"
            Left error           -> ErrorState error
        Left error           -> ErrorState error

    halt :: State -> State
    halt s = s
    
    value :: Store HeapCell -> Int -> Either String HeapCell
    value heap adr1 = case access heap adr1 of
        Right (IND adr2) -> value heap adr2
        Right hcell      -> Right hcell
        Left error       -> Left error

    unwind :: State -> State
    unwind s = case access (stack s) (sp s) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (APP adr1 adr2) -> s {sp = sp s + 1, stack = push (stack s) (StackCell adr1)}
            Right hcell           -> s {pc = pc s + 1}
            Left error            -> ErrorState error
        Left error             -> ErrorState error

    call :: State -> State
    call s = case access (stack s) (sp s) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (DEF _ _ adr) -> s {pc = adr, sp = sp s + 1, stack = push (stack s) (StackCell (pc s + 1))}
            Right (PRE op 2)    -> s {pc = 4, sp = sp s + 1, stack = push (stack s) (StackCell (pc s + 1))}
            Right (PRE If 3)    -> s {pc = 13, sp = sp s + 1, stack = push (stack s) (StackCell (pc s + 1))}
            Right (PRE op 1)    -> s {pc = 21, sp = sp s + 1, stack = push (stack s) (StackCell (pc s + 1))}
            Right (VALNum _)    -> s {pc = pc s + 1}
            Right (VALBool _)   -> s {pc = pc s + 1}
            Left error          -> ErrorState error
        Left error             -> ErrorState error
    
    return'' :: State -> State
    return'' s = case access (stack s) (sp s - 1) of
        Right (StackCell adr) -> case access (stack s) (sp s) of
            Right (StackCell adr1) -> case save (stack s) (StackCell adr1) (sp s - 1) of
                Right nstack -> s {pc = adr, sp = sp s - 1, stack = nstack}
                Left error   -> ErrorState error
            Left error             -> ErrorState error
        Left error            -> ErrorState error

    pushpre :: State -> Keyword -> State
    pushpre s kw = let tuple = newPRE (heap s) kw (arity' kw) in
        case save (stack s) (StackCell (fst tuple)) (sp s + 1) of
                Right stack -> s{pc = pc s + 1, sp = sp s + 1, stack = stack, heap = snd tuple}
                Left error   -> ErrorState error

    funcUpdate :: State -> Int -> State
    funcUpdate s arg = case access (stack s) (sp s) of
        Right (StackCell addr) -> let hcell = IND addr in case access (stack s) (sp s - arg - 2) of
            Right (StackCell addr1) -> case save (heap s) hcell addr1 of
                Right heap -> s {pc = pc s + 1, heap = heap}
                Left error -> ErrorState error
            Left error              -> ErrorState error
        Left error             -> ErrorState error

    opUpdate :: State -> State
    opUpdate s = case access (stack s) (sp s) of 
        Right (StackCell addr1) -> case access (heap s) addr1 of
            Right hcell -> case access (stack s) (sp s - 2) of
                Right (StackCell addr2) -> case save (heap s) hcell addr2 of
                    Right heap -> case access (stack s) (sp s - 2) of
                        Right aux -> case access (stack s) (sp s - 1) of
                            Right scell -> case save (stack s) scell (sp s - 2) of
                                Right nstack -> case save nstack aux (sp s - 1) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                                    Left error           -> ErrorState error
                                    _                    -> ErrorState "Error in 'opUpdate'"
                                Left error             -> ErrorState error
                            Left error            -> ErrorState error 
                        Left error            -> ErrorState error
                    Left error            -> ErrorState error
                Left error            -> ErrorState error
            Left error            -> ErrorState error
        Left error            -> ErrorState error

    operator :: State -> Int -> State
    operator s op = case op of
        1 -> case access (stack s) (sp s - 2) of
            Right (StackCell addr1) -> case value (heap s) addr1 of
                Right (PRE op 1) -> case access (stack s) (sp s - 1) of
                    Right scell -> case save (stack s) scell (sp s - 2) of
                        Right stack   -> case access stack (sp s) of
                            Right (StackCell addr2) -> case value (heap s) addr2 of
                                Right (VALBool w) -> case op of
                                    Not -> if fromEnum w == 1
                                                then let tuple = newVAL (heap s) 1 1 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    _                    -> ErrorState "'operator: 4 error"
                                                else let tuple = newVAL (heap s) 1 0 in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                                    _                    -> ErrorState "'operator: 4 error"
                                    _   -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                Right (VALNum w)  -> case op of
                                    Minus -> let tuple = newVAL (heap s) 0 (- w) in case save stack (StackCell (fst tuple)) (sp s - 1) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = snd tuple}
                                        _     -> ErrorState "Compile error: 'operator' calls value with wrong arguments." 
                                    _         -> ErrorState "Compile error: 'operator' calls value with wrong arguments."
                                _                 -> ErrorState "'operator: 2 error"      
                            Left error -> ErrorState error
                        Left error -> ErrorState error
                    Left error  -> ErrorState error
                _               -> ErrorState "'operator: 1 error"
            Left error  -> ErrorState error
        2 -> case access (stack s) (sp s - 3) of
            Right (StackCell addr) -> case value (heap s) addr of
                Right (PRE op 2) -> case access (stack s) (sp s - 2) of
                    Right scell -> case save (stack s) scell (sp s - 4) of
                        Right stack -> case access stack (sp s - 1) of
                            Right (StackCell addr2) -> let hcell1 = value (heap s) addr2 in
                                case access stack (sp s) of
                                    Right (StackCell addr3) -> let hcell2 = value (heap s) addr3 in
                                        case hcell1 of
                                            Right (VALBool bool1) -> case hcell2 of
                                                Right (VALBool bool2) -> case op of
                                                    And    -> let tuple = newVAL (heap s) 1 (bool1 .&. bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error 
                                                    Or     -> let tuple = newVAL (heap s) 1 (bool1 .|. bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Equals -> let tuple = newVAL (heap s) 1 (0 .&. xor bool1 bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Less   -> let tuple = newVAL (heap s) 1 (fromEnum $ bool1 < bool2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    _      -> ErrorState "Type error."
                                                _                      -> ErrorState "'operator': 9 error"
                                            Right (VALNum num1) -> case hcell2 of
                                                Right (VALNum num2) -> case op of
                                                    Equals -> let tuple = newVAL (heap s) 1 (fromEnum $ num1 == num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Less   -> let tuple = newVAL (heap s) 1 (fromEnum $ num1 < num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Plus   -> let tuple = newVAL (heap s) 0 (num1 + num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Times  -> let tuple = newVAL (heap s) 0 (num1 * num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    Divide -> let tuple = newVAL (heap s) 0 (num1 `div` num2) in
                                                        case save stack (StackCell $ fst tuple) (sp s - 3) of
                                                            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = snd tuple}
                                                            Left error  -> ErrorState error
                                                    _      -> ErrorState "Type error."
                                                _                    -> ErrorState "'operator': 8 error"
                                            _                   -> ErrorState "Type error."
                                    Left error          -> ErrorState error
                            Left error          -> ErrorState error
                        Left error          -> ErrorState error
                    Left error          -> ErrorState error
                _                -> ErrorState "'operator': 7 error"
            Left error          -> ErrorState error
        3 -> case access (stack s) (sp s) of
            Right (StackCell addr) -> case value (heap s) addr of
                Right (VALBool bool) -> case bool of
                    1  -> case access (stack s) (sp s - 4) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case save (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case access stack (sp s - 1) of
                                    Right scell -> case save stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $ (take $ sp s - 2) scells}
                                        _                    -> ErrorState "'operator: 6 error"
                                    Left error  -> ErrorState error
                                Left error  -> ErrorState error
                            Left error  -> ErrorState error
                        Left error              -> ErrorState error
                    0 -> case access (stack s) (sp s - 5) of
                        Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                            Right addr2 -> case save (stack s) (StackCell addr2) (sp s - 3) of
                                Right stack -> case access stack (sp s - 1) of
                                    Right scell -> case save stack scell (sp s - 4) of
                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $(take $ sp s - 3) scells}
                                        _                    -> ErrorState "'operator: 6 error"
                                    Left error  -> ErrorState error
                                Left error  -> ErrorState error
                            Left error  -> ErrorState error
                        Left error              -> ErrorState error
                    _ -> ErrorState "Type error"
                _                    -> ErrorState "'operator: 5 error"

            Left error              -> ErrorState error
        _ -> ErrorState "type error"
    
    alloc :: State -> State
    alloc s = let tuple = newUNI (heap s) in s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell (fst tuple)), heap = snd tuple}

    updateLet :: State -> Int -> State
    updateLet s@State{stack = Stack scells} n = case access (stack s) (sp s - n - 1) of
        Right (StackCell addr) -> case add2arg (heap s) addr of
            Right addr1 -> case access (stack s) (sp s) of
                Right (StackCell addr2) -> case save (heap s) (IND addr2) addr1 of
                    Right heap -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                    Left error -> ErrorState error
                Left error              -> ErrorState error
            Left error  -> ErrorState error
        Left error             -> ErrorState error

    slideLet :: State -> Int -> State
    slideLet s n = case access (stack s) (sp s) of
        Right scell -> case save (stack s) scell (sp s - n) of
            Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
            Left error           -> ErrorState error
        Left error  -> ErrorState error