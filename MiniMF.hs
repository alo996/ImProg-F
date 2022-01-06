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
        Left error -> ErrorState $ show error
    -- update pc, update sp, save a new stack that now has the address of function 'name' in the global environment at position 'sp s'

    pushval :: State -> Int -> Int -> State
    pushval s typ field = case new (heap s) typ 0 field of
        Right (addr, heap) -> s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell addr), heap = heap}
        Left error         -> ErrorState $ show error

    pushparam :: State -> Int -> State
    pushparam s n = case add2arg (heap s) ((sp s + 1) - n - 2) of
        Right addr -> case save (stack s) (StackCell addr) (sp s) of
            Just stack -> s {pc = pc s + 1, sp = sp s + 1, stack = stack}
            _          -> ErrorState "error"
        Left error -> ErrorState $ show error

    makeapp :: State -> State
    makeapp s = case access (stack s) (sp s) of
        Just (StackCell a) -> case access (stack s) (sp s - 1) of
            Just (StackCell b) -> case new (heap s) 0 a b of
                Right (addr, heap) -> case save (stack s) (StackCell addr) (sp s - 1) of
                    Just stack -> s {pc = pc s + 1, sp = sp s - 1, stack = stack, heap = heap}
                    _          -> ErrorState "error"
                Left error         -> ErrorState $ show error
            _                   -> ErrorState "error"
        _                  -> ErrorState "error"
    
    slide :: State -> Int -> State
    slide s n = case access (stack s) (sp s - 1) of -- stack[T - 1] existiert
        Just t1 -> case access (stack s) (sp s) of -- stack[T] existiert
            Just t2 -> case save (stack s) t1 (sp s - n - 1) of -- stack[T - 1] kann an stack[T - n - 1] gespeichert werden
                Just stack_1 -> case save stack_1 t2 (sp s - n) of -- stack[T] kann an stack[T - n] gespeichert werden
                    Just (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n) scells)}
                    _                   -> ErrorState "error"
                _       -> ErrorState "error"
            _       -> ErrorState "error"
        _       -> ErrorState "error"

    reduce :: State -> State
    reduce s = case access (stack s) (sp s) of 
        Just (StackCell addr) -> case access (heap s) addr of
            Just elem -> case elem of
                (APP addr1 addr2) -> s {pc = pc s - 1, sp = sp s + 1, stack = push (stack s) (StackCell addr1)}
                (DEF f n addr1)   -> s {pc = addr1, sp = sp s + 1, stack = push (stack s) (StackCell (pc s))}
                _                 -> case access (stack s) (sp s - 1) of
                    Just (StackCell addr1) -> case access (stack s) (sp s) of
                        Just scell -> case save (stack s) scell (sp s - 1) of
                            Just (Stack scells) -> s {pc = addr1, sp = sp s - 1, stack = Stack (init scells)}
                            _                   -> ErrorState "error"                    
                        _          -> ErrorState "error"
                    _                      -> ErrorState "error"     
            _         -> ErrorState "error"
        _         -> ErrorState "error"

    return' :: State -> State
    return' s = case access (stack s) (sp s - 1) of
        Just s1@(StackCell addr1) -> case access (stack s) (sp s) of
            Just s2@(StackCell addr2) -> case save (stack s) s2 addr1 of
                Just stack -> s {pc = addr1, sp = sp s - 1, stack = stack}
                _          -> ErrorState "error" 
            _                         -> ErrorState "error" 
        _                         -> ErrorState "error"

    -- halt implementieren
    -- let beginningState = State 0 emptyCode emptyStack emptyHeap emptyGlobalEnv


    -- run :: State -> State
    -- page 71 hauptzyklus

    -- executeInstruction



