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
    access c@(Code ccells) int      = if int < depth c then Just (ccells !! int) else Nothing
    access s@(Stack scells) int     = if int < depth s then Just (scells !! int) else Nothing
    access h@(Heap hcells) int      = if int < depth h then Just (hcells !! int) else Nothing
    access g@(GlobalEnv gcells) int = if int < depth g then Just (gcells !! int) else Nothing

    assignStackCellAdr :: Store StackCell -> Int -> Int -> Maybe Store StackCell -- Fehler beheben..
    assignStackCellAdr s@(Stack scells) int adr    = if int < depth s
        then Just Stack (map (\x -> if elemIndex x == int then StackCell adr else x) scells)
        else Nothing

    -- reverse store
    reverseStore :: Store a -> Maybe (Store a)
    reverseStore (Code ccells)      = Just $ Code (reverse ccells)
    reverseStore (Stack scells)     = Just $ Stack (reverse scells)
    reverseStore (Heap hcells)      = Just $ Heap (reverse hcells)
    reverseStore (GlobalEnv gcells) = Just $ GlobalEnv (reverse gcells)

    -- save element at n-th position in a store
    save :: Store a -> a -> Int -> Store a
    save (Code ccells) ccell pos      = Code (take (pos - 1) ccells ++ [ccell] ++ drop pos ccells)
    save (Stack scells) scell pos     = Stack (take (pos - 1) scells ++ [scell] ++ drop pos scells)
    save (Heap hcells) hcell pos      = Heap (take (pos - 1) hcells ++ [hcell] ++ drop pos hcells)
    save (GlobalEnv gcells) gcell pos = Stack (take (pos - 1) gcells ++ [gcell] ++ drop pos gcells)