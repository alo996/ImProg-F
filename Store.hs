module Store where
    import Declarations

    -- create empty store
    emptyCode, emptyStack, emptyHeap :: Store a
    emptyCode      = Code []
    emptyStack     = Stack []
    emptyHeap      = Heap []

    -- push element at the end of a store (not very efficient, but more maybe intuitive for current development)
    push :: Store a -> a -> Store a
    push (Code ccells) elem      = Code (ccells ++ [elem])
    push (Stack scells) elem     = Stack (scells ++ [elem])
    push (Heap hcells) elem      = Heap (hcells ++ [elem])

    -- return depth of store
    depth :: Store a -> Int
    depth (Code ccells)      = length ccells
    depth (Stack scells)     = length scells
    depth (Heap hcells)      = length hcells

    -- access n-th element in store
    access :: Show a => Store a -> Int -> Either String a
    access c@(Code ccells) pos      = if (pos <= depth c) && (pos >= 0) && not(null ccells) then return $ ccells !! pos else Left $ "Compile error: " ++ show c ++ " has no index " ++ show pos ++ "."
    access s@(Stack scells) pos     = if (pos <= depth s) && (pos >= 0) && not(null scells) then return $ scells !! pos else Left $ "Compile error: " ++ show s ++ " has no index " ++ show pos ++ "."
    access h@(Heap hcells) pos      = if (pos <= depth h) && (pos >= 0) && not(null hcells) then return $ hcells !! pos else Left $ "Compile error: " ++ show h ++ " has no index " ++ show pos ++ "."

    -- reverse store
    reverseStore :: Store a -> Store a
    reverseStore (Code ccells)      = Code (reverse ccells)
    reverseStore (Stack scells)     = Stack (reverse scells)
    reverseStore (Heap hcells)      = Heap (reverse hcells)

    -- either overwrite n-th element in a store or push element at end of store
    save :: Show a => Store a -> a -> Int -> Either String (Store a)
    save c@(Code ccells) ccell pos      = if (pos <= depth c + 1) && (pos >= 0) then return $ Code (take (pos - 1) ccells ++ [ccell] ++ drop pos ccells) else Left $ "Compile error in 'save': " ++ show c ++ " has no index " ++ show pos ++ "."
    save s@(Stack scells) scell pos     = if (pos <= depth s + 1) && (pos >= 0) then return $ Stack (take (pos - 1) scells ++ [scell] ++ drop pos scells) else Left $ "Compile error in 'save': " ++ show s ++ " has no index " ++ show pos ++ "."
    save h@(Heap hcells) hcell pos      = if (pos <= depth h + 1) && (pos >= 0) then return $ Heap (take (pos - 1) hcells ++ [hcell] ++ drop pos hcells) else Left $ "Compile error in 'save': " ++ show h ++ " has no index " ++ show pos ++ "."

    {-
    pop-function not really needed.
    pop :: Store a -> Maybe a
    pop (Code (x : xs))      = return x
    pop (Stack (x : xs))     = return x
    pop (Heap (x : xs))      = return x
    pop (GlobalEnv (x : xs)) = return x
    pop _                    = Nothing
    -}