module Store where
    import Declarations

    -- create empty store
    emptyCode, emptyStack, emptyHeap, emptyGlobalEnv :: Store a
    emptyCode      = Code []
    emptyStack     = Stack []
    emptyHeap      = Heap []
    emptyGlobalEnv = GlobalEnv []

    -- push element on beginning of store
    push :: Store a -> a -> Store a
    push (Code ccells) elem      = Code (elem : ccells)
    push (Stack scells) elem     = Stack (elem : scells)
    push (Heap hcells) elem      = Heap (elem : hcells)
    push (GlobalEnv gcells) elem = GlobalEnv (elem : gcells)

    -- pop first element from store
    pop :: Store a -> Maybe a
    pop (Code (x : xs))      = return x
    pop (Stack (x : xs))     = return x
    pop (Heap (x : xs))      = return x
    pop (GlobalEnv (x : xs)) = return x
    pop _                    = Nothing

    -- return depth of store
    depth :: Store a -> Int
    depth (Code ccells)      = length ccells
    depth (Stack scells)     = length scells
    depth (Heap hcells)      = length hcells
    depth (GlobalEnv gcells) = length gcells

    -- access n-th element in store
    access :: Store a -> Int -> Maybe a
    access c@(Code ccells) pos      = if (pos < depth c) && (pos >= 0) then Just (ccells !! pos) else Nothing
    access s@(Stack scells) pos     = if (pos < depth s) && (pos >= 0) then Just (scells !! pos) else Nothing
    access h@(Heap hcells) pos      = if (pos < depth h) && (pos >= 0) then Just (hcells !! pos) else Nothing
    access g@(GlobalEnv gcells) pos = if (pos < depth g) && (pos >= 0) then Just (gcells !! pos) else Nothing

    -- reverse store
    reverseStore :: Store a -> Store a
    reverseStore (Code ccells)      = Code (reverse ccells)
    reverseStore (Stack scells)     = Stack (reverse scells)
    reverseStore (Heap hcells)      = Heap (reverse hcells)
    reverseStore (GlobalEnv gcells) = GlobalEnv (reverse gcells)

    -- overwrite n-th element in a store
    save :: Store a -> a -> Int -> Maybe (Store a)
    save c@(Code ccells) ccell pos      = if (pos <= depth c) && (pos >= 0) then Just (Code (take (pos - 1) ccells ++ [ccell] ++ drop pos ccells)) else Nothing
    save s@(Stack scells) scell pos     = if (pos <= depth s) && (pos >= 0) then Just (Stack (take (pos - 1) scells ++ [scell] ++ drop pos scells)) else Nothing
    save h@(Heap hcells) hcell pos      = if (pos <= depth h) && (pos >= 0) then Just (Heap (take (pos - 1) hcells ++ [hcell] ++ drop pos hcells)) else Nothing
    save g@(GlobalEnv hcells) hcell pos = if (pos <= depth g) && (pos >= 0) then Just (GlobalEnv (take (pos - 1) hcells ++ [hcell] ++ drop pos hcells)) else Nothing