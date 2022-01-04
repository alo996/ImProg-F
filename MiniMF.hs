module MiniMF where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe
    import Distribution.Types.Lens (_Impl)
    
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
        Right addr -> s {pc = (pc s) + 1, sp = (sp s) + 1, stack = save (stack s) (StackCell addr) (sp s)}
        Left error -> ErrorState $ show error

    makeapp :: State -> State
    makeapp s = case access (stack s) (sp s) of
        Just (StackCell a) -> case access (stack s) (sp s - 1) of
            Just (StackCell b) -> case new (heap s) 0 a b of
                Right (addr, heap) -> s {pc = pc s + 1, sp = sp s - 1, stack = save (stack s) (StackCell addr) (sp s - 1), heap = heap}
                Left error         -> ErrorState $ show error
            _                   -> ErrorState "error"
        _                  -> ErrorState "error"
    
    slide :: State -> Int -> State
    slide s n = case access (stack s) (sp s - 1) of -- stack[T - 1] existiert
        Just t1 -> case access (stack s) (sp s) of -- stack[T] existiert
            Just t2 -> let Stack cells = save (save (stack s) t1 (sp s - n - 1)) t2 (sp s - n) -- speichere den umsortierten stack in Stack cells 
                            in s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n) cells)} -- initialisiere Rückgabezustand, wobei die letzten n elemente gelöscht werden
            _       -> ErrorState "error"
        _       -> ErrorState "error"

    reduce :: State -> State
    reduce s = case access (stack s) (sp s) of 
        Just (StackCell addr) -> case access (heap s) addr of
            Just elem -> case elem of
                (APP addr1 addr2) -> s {pc = pc s - 1, sp = sp s + 1, stack = push (stack s) (StackCell addr1)}
                (DEF f n addr1)   -> s {pc = addr1, sp = sp s + 1, stack = push (stack s) (StackCell (pc s))}
                (VALNum typ val)  -> case access (stack s) (sp s - 1) of
                    Just (StackCell addr1) -> case access (stack s) (sp s) of
                        Just scell -> let Stack cells = save (stack s) scell (sp s - 1)
                                            in s {pc = addr1, sp = sp s - 1, stack = Stack (init cells)}
                        _          -> ErrorState "error"                    
                    _         -> ErrorState "error" 
                (VALBool typ val) -> undefined
            _         -> ErrorState "error"
        _         -> ErrorState "error"

    

    {-
    let beginningState = State 0 emptyCode emptyStack emptyHeap emptyGlobalEnv

    return' :: State -> State -- access :: Stack Stackcell -> Int -> Maybe Stackcell
    return' s = do
        StackCell adr <- access (stack s) (sp s - 1)
        if isNothing adrfromstack then 
            ErrorState "fehler"
        else -- save :: Store a -> a -> Int -> Store a
            s {pc = adr, sp = (sp s) - 1, stack = save (stack s) (StackCell 1) (sp s)}


    slide :: State -> Int -> State
    slide s n = do
    stackCellLast <- access (stack s) (sp s - 1)
    stackCellAct <- access (stack s) (sp s)
    let newStack = save (stack s) () (sp s - n - 1)


    pushvalnum :: State -> Bool -> State
    pushvalnum s val = let heapadr@(adr, newheap) = newVALNum (heap s) val in s {pc = pc s + 1, sp = sp s + 1, stack = save (stack s) (StackCell adr) (sp s), heap = newheap}
    -- update pc, update sp, save adress of the newly generated HeapCell with its number variable in stack and update heap

    pushvalbool :: State -> Bool -> State
    pushvalnum s val = let heapadr@(adr, newheap) = newVALBool (heap s) val in s {pc = pc s + 1, sp = sp s + 1, stack = save (stack s) (StackCell adr) (sp s), heap = newheap}
    -- update pc, update sp, save adress of the newly generated HeapCell with its boolean variable in stack and update heap
    -}
