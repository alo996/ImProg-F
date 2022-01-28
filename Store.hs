module Store where
    import Declarations

    -- Push element at the end of a stack. Not very efficient, but more intuitive at the moment.
    pushStack :: Stack -> StackCell -> Stack
    pushStack (Stack scells) elem  = Stack (scells ++ [elem])

    pushHeap :: Heap -> HeapCell -> Heap
    pushHeap (Heap hcells) elem  = Heap (hcells ++ [elem])

    -- Access element at index 'ind' in code, stack or heap.
    accessCode :: Code -> Int -> Either String Instruction
    accessCode c@(Code ccells) ind = if (ind < length ccells) && (ind >= 0) && not(null ccells) then return $ ccells !! ind else Left $ "Compile error: " ++ show c ++ " has no index " ++ show ind ++ "."

    accessStack :: Stack -> Int -> Either String StackCell
    accessStack s@(Stack scells) ind = if (ind < length scells) && (ind >= 0) && not(null scells) then return $ scells !! ind else Left $ "Compile error: " ++ show s ++ " has no index " ++ show ind ++ "."

    accessHeap :: Heap -> Int -> Either String HeapCell
    accessHeap h@(Heap hcells) ind = if (ind < length hcells) && (ind >= 0) && not(null hcells) then return $ hcells !! ind else Left $ "Compile error: " ++ show h ++ " has no index " ++ show ind ++ "."

    -- Either overwrite element at index 'ind' in a stack/heap or push element at end of store.
    saveStack :: Stack -> StackCell -> Int -> Either String Stack
    saveStack s@(Stack scells) scell ind  = if (ind <= length scells) && (ind >= 0) then return $ Stack (take ind scells ++ [scell] ++ drop (ind + 1) scells) else Left $ "Compile error in 'save': " ++ show s ++ " has no index " ++ show ind ++ "."

    saveHeap :: Heap -> HeapCell -> Int -> Either String Heap
    saveHeap h@(Heap hcells) hcell ind   = if (ind <= length hcells) && (ind >= 0) then return $ Heap (take ind hcells ++ [hcell] ++ drop (ind + 1) hcells) else Left $ "Compile error in 'save': " ++ show h ++ " has no index " ++ show ind ++ "."