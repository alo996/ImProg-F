{-# LANGUAGE NamedFieldPuns #-}
module MiniMF where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe
    import Compiler
    import Debug.Trace
    import Tokenizer
    import Parser
    import Data.Char

    -- interpret recursively takes instruction in code at programm-counter and runs it with the given state
    interpret :: State -> State
    interpret s@State{pc, sp, code = Code ccells, stack, heap} = case trace (show (ccells !! pc) ++ ", pc = " ++ show pc ++ ", sp = " ++ show sp ++ ", stack = " ++ show stack ++ ", heap = " ++ show heap) (access (Code ccells) pc) of
        Right instruction -> case instruction of
            Halt -> s
            _    -> case run instruction s of
                ErrorState error -> ErrorState error
                state            -> interpret state
        Left error        -> ErrorState error
    interpret (ErrorState error) = ErrorState error
    interpret _                  = ErrorState "Compile error: Function 'interpret' called with wrong argument."

    -- Given an instruction, execute the respective MiniMF function.
    run :: Instruction -> State -> State
    run Reset state               = reset state
    run (Pushfun fname) state     = pushfun state fname
    run (Pushval typ field) state = case typ of
        "Bool" -> pushval state 2 field
        _      -> pushval state 1 field
    run (Pushparam addr) state    = pushparam state addr
    run Makeapp state             = makeapp state
    run (Slide n) state           = slide state n
    run Reduce state              = reduce state
    run Return state              = return' state
    run Halt state                = halt state
    run (Error error) state       = ErrorState error

    -- kept code tightly to functions described in the script

    -- help procedures:

    -- address takes the function name and delivers the address of its DEF-cell in the global environment
    address :: Store HeapCell -> String -> Either String Int
    address (Heap hcells) f = address' hcells 0 f where
        address' (x : xs) acc f = case x of
            DEF {fname = fname} -> if f == fname then return acc else address' xs (acc + 1) f -- ERROR: fname is usually of the form "AtomicExpr (Val "fname")"", this solution doesn't work at the moment
            _                   -> address' xs (acc + 1) f
        address' [] _ _         = Left $ "Compile error: Heap does not contain definition of function '" ++ show f ++ "'."
    address _ _                 = Left "Compile error: Function 'address' not called on heap."

    -- add2arg takes the address of an APP-cell and delivers the heapaddress of its argument
    add2arg :: Store HeapCell -> Int -> Either String Int
    add2arg h@(Heap hcells) addr = if (addr >= 0) && (addr < depth h) then let APP addr1 addr2 = hcells !! addr in return addr2 else Left $ "Compile error: Heap does not contain cell at address " ++ show addr ++ "'."
    add2arg _ _                  = Left "Compile error: Function 'add2arg' not called on heap."

    newAPP :: Store HeapCell -> Int -> Int -> (Int, Store HeapCell)
    newAPP heap a b = (depth heap, push heap (APP a b))

    newVAL :: Store HeapCell -> Int -> Int -> (Int, Store HeapCell)
    newVAL heap t w 
            | t == 0    = (depth heap, push heap (VALNum w))
            | otherwise = if w == 0 
                            then (depth heap, push heap (VALBool False))
                            else (depth heap, push heap (VALBool True))

    newIND :: Store HeapCell -> Int -> (Int, Store HeapCell)
    newIND heap a = (depth heap, push heap (IND a))

    newPRE :: Store HeapCell -> Keyword -> Int -> Either String (Int, Store HeapCell)
    newPRE heap kw n = case arity' kw of
        Just n  -> return (depth heap, push heap (PRE kw n))
        Nothing -> Left "Compile error: 'arity' called with illegal keyword."
    
    arity' :: Keyword -> Maybe Int
    arity' kw = case kw of
        And    -> Just 2
        Or     -> Just 2
        Equals -> Just 2
        Less   -> Just 2
        Plus   -> Just 2
        Minus  -> Just 1
        Times  -> Just 2
        Divide -> Just 2
        Not    -> Just 1
        _      -> Nothing

    -- reset initializes programm-state
    reset :: State -> State
    reset s = s {sp = -1, pc = pc s + 1}

    -- pushfun pushes/loads the address of a DEF-celll of a function on the stack
    pushfun :: State -> String -> State
    pushfun s name = case address (heap s) name of
        Right int  -> s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell int)}
        Left error -> ErrorState error

    pushval :: State -> Int -> Int -> State
    pushval s t w = let tuple = newVAL (heap s) t w in s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell (fst tuple)), heap = snd tuple}

    -- pushparam loads the value of an argument of a function on the stack
    pushparam :: State -> Int -> State
    pushparam s n = case access (stack s) (sp s - n - 1) of
        Right (StackCell addr) -> case add2arg (heap s) addr of
            Right addr -> case save (stack s) (StackCell addr) (sp s + 1) of
                Right stack -> s {pc = pc s + 1, sp = sp s + 1, stack = stack}
                Left error  -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error
        
    -- makeapp creates an APP-cell in the heap and loads its address on the stack
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

    -- slide deletes the stack cells two beneath the last two cells and replaces them by the last two cells ("abräumen")
    -- common function structure: pattern match result of function with case of to extract monad value and apply to next function
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

    -- reduce reduces the imaginated "stack graph" and transforms its structure (eg. call, jump back after procedure)
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

    -- return jumps back to saved address (result and address saved!)
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

    -- Test functions
    execute'' :: String -> State
    execute'' input = 
        case tokenize input of
            Right tokens -> case program tokens of
                Right ast  -> case compileProgram (fst ast) of
                    ErrorState error -> ErrorState error
                    state            -> state
                Left error -> ErrorState error
            Left error -> ErrorState error