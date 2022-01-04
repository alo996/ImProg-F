module MiniF where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe

    address :: Store GlobalCell -> String -> Either String Int
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

    {-
    let beginningState = State 0 emptyCode emptyStack emptyHeap emptyGlobalEnv

    pushparam :: State -> Int -> State
    pushparam s adr = do
    sadr <- add2arg (heap s) ((sp s + 1) - adr - 2)
    if isLeft sadr then
        ErrorState "uff falsch"
    else
        s {pc = (pc s) + 1, sp = (sp s) + 1, stack = save (stack s) (StackCell sadr) (sp s)}

    return' :: State -> State -- access :: Stack Stackcell -> Int -> Maybe Stackcell
    return' s = do
        StackCell adr <- access (stack s) (sp s - 1)
        if isNothing adrfromstack then 
            ErrorState "fehler"
        else -- save :: Store a -> a -> Int -> Store a
            s {pc = adr, sp = (sp s) - 1, stack = save (stack s) (StackCell 1) (sp s)}


    makeapp :: State -> State
    makeapp s = let heapadr@(adr, newheap) = newAPP (heap s) (sp s) (sp s - 1) in s {pc = pc s + 1, sp = sp s - 1, stack = save (stack s) (StackCell adr) (sp s)}


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
