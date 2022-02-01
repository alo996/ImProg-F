{- |
Module      : Store
Description : This module contains all functionality to perform necessary operations on stores code, stack, global and heap.
-}
module Store where

import Declarations
    (Instruction(Return, Reset, Pushfun, Halt, Pushparam, Unwind,
                  Call, Operator, OpUpdate),
    StackCell,
    HeapCell,
    Heap(..),
    Stack(..),
    Code(..))


-- | Access element at index 'n' in code, stack or heap.
accessCode :: Code -> Int -> Either String Instruction
accessCode c@(Code ccells) n
    | n < length ccells && n >= 0 && not (null ccells) = return $ ccells !! n 
    | otherwise                                        = Left $ "Code has no index " ++ show n ++ "."

accessStack :: Stack -> Int -> Either String StackCell
accessStack s@(Stack scells) n
    | n < length scells && n >= 0 && not (null scells) = return $ scells !! n 
    | otherwise                                        = Left $ "Stack has no index " ++ show n ++ "."

accessHeap :: Heap -> Int -> Either String HeapCell
accessHeap h@(Heap hcells) n 
    | n < length hcells && n >= 0 && not(null hcells) = return $ hcells !! n 
    | otherwise                                       = Left $ "Heap has no index " ++ show n ++ "."

-- | Initialize code with the set of MF instructions needed before translating function definitions.
codeInit :: Code
codeInit =
    Code [Reset, Pushfun "main", Call, Halt, Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator 2, OpUpdate, Return, Pushparam 1, Unwind, Call, Operator 3, OpUpdate, Unwind, Call, Return, Pushparam 1, Unwind, Call, Operator 1, OpUpdate, Return]

-- | Push element at the end of a stack or heap.
pushStack :: Stack -> StackCell -> Stack
pushStack (Stack scells) elem = Stack $ scells ++ [elem]

pushHeap :: Heap -> HeapCell -> Heap
pushHeap (Heap hcells) elem = Heap $ hcells ++ [elem]

-- | Either overwrite element at index 'n' in a stack or heap, or push element to its end.
saveStack :: Stack -> StackCell -> Int -> Either String Stack
saveStack s@(Stack scells) scell n
    | n <= length scells && n >= 0 = return $ Stack (take n scells ++ [scell] ++ drop (n + 1) scells) 
    | otherwise                    = Left $ "Stack has no index " ++ show n ++ "."

saveHeap :: Heap -> HeapCell -> Int -> Either String Heap
saveHeap h@(Heap hcells) hcell n 
    | n <= length hcells && n >= 0 = return $ Heap (take n hcells ++ [hcell] ++ drop (n + 1) hcells) 
    | otherwise                    = Left $ "Heap has no index " ++ show n ++ "."
