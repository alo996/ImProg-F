module Store where
    import Declarations

    -- Create empty store.
    emptyCode, emptyStack, emptyHeap :: Store a
    emptyCode  = Code []
    emptyStack = Stack []
    emptyHeap  = Heap []

    -- Push element on the end of a store. Not very efficient, but more intuitive at the moment.
    push :: Store a -> a -> Store a
    push (Code ccells) elem      = Code (ccells ++ [elem])
    push (Stack scells) elem     = Stack (scells ++ [elem])
    push (Heap hcells) elem      = Heap (hcells ++ [elem])

    -- Return depth of store.
    depth :: Store a -> Int
    depth (Code ccells)  = length ccells
    depth (Stack scells) = length scells
    depth (Heap hcells)  = length hcells

    -- Access element at index 'ind' in a store.
    access :: Show a => Store a -> Int -> Either String a
    access c@(Code ccells) ind  = if (ind < depth c) && (ind >= 0) && not(null ccells) then return $ ccells !! ind else Left $ "Compile error: " ++ show c ++ " has no index " ++ show ind ++ "."
    access s@(Stack scells) ind = if (ind < depth s) && (ind >= 0) && not(null scells) then return $ scells !! ind else Left $ "Compile error: " ++ show s ++ " has no index " ++ show ind ++ "."
    access h@(Heap hcells) ind  = if (ind < depth h) && (ind >= 0) && not(null hcells) then return $ hcells !! ind else Left $ "Compile error: " ++ show h ++ " has no index " ++ show ind ++ "."

    -- Reverse a store.
    reverseStore :: Store a -> Store a
    reverseStore (Code ccells)  = Code (reverse ccells)
    reverseStore (Stack scells) = Stack (reverse scells)
    reverseStore (Heap hcells)  = Heap (reverse hcells)

    -- Either overwrite element at index 'ind' in a store or push element at end of store.
    save :: Show a => Store a -> a -> Int -> Either String (Store a)
    save c@(Code ccells) ccell ind  = if (ind <= depth c) && (ind >= 0) then return $ Code (take ind ccells ++ [ccell] ++ drop (ind + 1) ccells) else Left $ "Compile error in 'save': " ++ show c ++ " has no index " ++ show ind ++ "."
    save s@(Stack scells) scell ind = if (ind <= depth s) && (ind >= 0) then return $ Stack (take ind scells ++ [scell] ++ drop (ind + 1) scells) else Left $ "Compile error in 'save': " ++ show s ++ " has no index " ++ show ind ++ "."
    save h@(Heap hcells) hcell ind  = if (ind <= depth h) && (ind >= 0) then return $ Heap (take ind hcells ++ [hcell] ++ drop (ind + 1) hcells) else Left $ "Compile error in 'save': " ++ show h ++ " has no index " ++ show ind ++ "."