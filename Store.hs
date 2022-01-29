module Store where
    
import Declarations
    ( Heap(..), HeapCell, StackCell, Stack(..), Instruction, Code(..) )


-- | Push element at the end of a stack or heap.
pushStack :: Stack -> StackCell -> Stack
pushStack (Stack scells) elem = Stack $ scells ++ [elem]

pushHeap :: Heap -> HeapCell -> Heap
pushHeap (Heap hcells) elem = Heap $ hcells ++ [elem]

-- | Access element at index 'n' in a code, stack or heap.
accessCode :: Code -> Int -> Either String Instruction
accessCode c@(Code ccells) n
    | (n < length ccells) && (n >= 0) && not(null ccells) = return $ ccells !! n 
    | otherwise                                           = Left $ "Compile error: " ++ show c ++ " has no index " ++ show n ++ "."

accessStack :: Stack -> Int -> Either String StackCell
accessStack s@(Stack scells) n
    | (n < length scells) && (n >= 0) && not(null scells) = return $ scells !! n 
    | otherwise                                           = Left $ "Compile error: " ++ show s ++ " has no index " ++ show n ++ "."

accessHeap :: Heap -> Int -> Either String HeapCell
accessHeap h@(Heap hcells) n 
    | (n < length hcells) && (n >= 0) && not(null hcells) = return $ hcells !! n 
    | otherwise                                           = Left $ "Compile error: " ++ show h ++ " has no index " ++ show n ++ "."

-- | Either overwrite element at index 'n' in a stack or heap, or push element at its end.
saveStack :: Stack -> StackCell -> Int -> Either String Stack
saveStack s@(Stack scells) scell n
    | (n <= length scells) && (n >= 0) = return $ Stack (take n scells ++ [scell] ++ drop (n + 1) scells) 
    | otherwise                        = Left $ "Compile error in 'save': " ++ show s ++ " has no index " ++ show n ++ "."

saveHeap :: Heap -> HeapCell -> Int -> Either String Heap
saveHeap h@(Heap hcells) hcell n 
    | (n <= length hcells) && (n >= 0) = return $ Heap (take n hcells ++ [hcell] ++ drop (n + 1) hcells) 
    | otherwise                        = Left $ "Compile error in 'save': " ++ show h ++ " has no index " ++ show n ++ "."