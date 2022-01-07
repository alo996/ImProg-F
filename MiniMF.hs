module MiniMF where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe
    
    address :: Store HeapCell -> String -> Either String Int
    address (GlobalEnv gcells) f = address' gcells 0 f where
        address' (x : xs) acc f = case x of
            (DEF f _ _) -> return acc
            _           -> address' xs (acc + 1) f
        address' [] _ _         = Left $ "Compile error: Global environment does not contain definition of function '" ++ show f ++ "'."
    address _ _                 = Left "Function 'address' not called on global environment."

    add2arg :: Store HeapCell -> Int -> Either String Int 
    add2arg h@(Heap hcells) addr = if (addr >= 0) && (addr < depth h) then let APP addr1 addr2 = hcells !! addr in return addr2 else Left $ "Compile error: Heap does not contain cell at address " ++ show addr ++ "'."
    add2arg _ _                  = Left "Function 'add2arg' not called on heap."

    new :: Store HeapCell -> Int -> Int -> Int -> Either String (Int, Store HeapCell)
    -- new :: heap -> type (0: APP, 1: VALNum, 2: VALBool) -> field a -> field b -> (heap adress, updated heap)
    new heap typ a b
        | typ == 0  = return (depth heap, push heap (APP a b))
        | typ == 1  = return (depth heap, push heap (VALNum 1 b))
        | typ == 2 = case b of
            0 -> return (depth heap, push heap (VALBool 2 False))
            1 -> return (depth heap, push heap (VALBool 2 True))
            _ -> Left "Invalid input in 'new'"
        | otherwise = Left "Invalid input in 'new'"

    typ :: HeapCell -> Int
    typ (APP _ _)     = 0
    typ (VALNum _ _)  = 1
    typ (VALBool _ _) = 2
    typ DEF {}        = 3

    reset :: State -> State
    reset s = s {sp = -1, pc = pc s + 1}

    pushfun :: State -> String -> State
    pushfun s name = case address (global s) name of
        Right int  -> s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell int)}
        Left error -> ErrorState error
    -- update pc, update sp, save a new stack that now has the address of function 'name' in the global environment at position 'sp s'

    pushval :: State -> Int -> Int -> State
    pushval s typ field = case new (heap s) typ 0 field of
        Right (addr, heap) -> s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell addr), heap = heap}
        Left error         -> ErrorState error

    pushparam :: State -> Int -> State
    pushparam s n = case add2arg (heap s) ((sp s + 1) - n - 2) of
        Right addr -> case save (stack s) (StackCell addr) (sp s) of
            Right stack -> s {pc = pc s + 1, sp = sp s + 1, stack = stack}
            Left error  -> ErrorState error
        Left error -> ErrorState error

    makeapp :: State -> State
    makeapp s = case access (stack s) (sp s) of
        Right (StackCell a) -> case access (stack s) (sp s - 1) of
            Right (StackCell b) -> case new (heap s) 0 a b of
                Right (addr, heap) -> case save (stack s) (StackCell addr) (sp s - 1) of
                    Right stack -> s {pc = pc s + 1, sp = sp s - 1, stack = stack, heap = heap}
                    Left error  -> ErrorState error
                Left error         -> ErrorState error
            Left error         -> ErrorState error
        Left error         -> ErrorState error
    
    slide :: State -> Int -> State
    slide s n = case access (stack s) (sp s - 1) of -- stack[T - 1] existiert
        Right t1   -> case access (stack s) (sp s) of -- stack[T] existiert
            Right t2  -> case save (stack s) t1 (sp s - n - 1) of -- stack[T - 1] kann an stack[T - n - 1] gespeichert werden
                Right stack_1 -> case save stack_1 t2 (sp s - n) of -- stack[T] kann an stack[T - n] gespeichert werden
                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n) scells)}
                    Left error           -> ErrorState error
                    _                    -> ErrorState "slide called on wrong store"
                Left error    -> ErrorState error
            Left error -> ErrorState error
        Left error -> ErrorState error

    reduce :: State -> State
    reduce s = case access (stack s) (sp s) of 
        Right (StackCell addr) -> case access (heap s) addr of
            Right elem -> case elem of
                (APP addr1 addr2) -> s {pc = pc s - 1, sp = sp s + 1, stack = push (stack s) (StackCell addr1)}
                (DEF f n addr1)   -> s {pc = addr1, sp = sp s + 1, stack = push (stack s) (StackCell (pc s))}
                _                 -> case access (stack s) (sp s - 1) of
                    Right (StackCell addr1) -> case access (stack s) (sp s) of
                        Right scell -> case save (stack s) scell (sp s - 1) of
                            Right (Stack scells) -> s {pc = addr1, sp = sp s - 1, stack = Stack (init scells)}
                            Left error           -> ErrorState error
                            _                    -> ErrorState "save called on wrong store"                
                        Left error           -> ErrorState error
                        _                    -> ErrorState "save called on wrong store"
                    Left error           -> ErrorState error
                    _                    -> ErrorState "save called on wrong store"
        Left error           -> ErrorState error

    return' :: State -> State
    return' s = case access (stack s) (sp s - 1) of
        Right s1@(StackCell addr1) -> case access (stack s) (sp s) of
            Right s2@(StackCell addr2) -> case save (stack s) s2 addr1 of
                Right stack -> s {pc = addr1, sp = sp s - 1, stack = stack}
                Left error           -> ErrorState error
            Left error           -> ErrorState error 
        Left error           -> ErrorState error
    -- halt implementieren
    -- let beginningState = State 0 emptyCode emptyStack emptyHeap emptyGlobalEnv


    -- run :: State -> State
    -- page 71 hauptzyklus

    -- executeInstruction



