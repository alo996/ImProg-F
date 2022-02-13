{- |
Module      : MF
Description : This module contains all functionality to interpret a translated F program.
-}
{-# LANGUAGE NamedFieldPuns #-}
module MF where

import Data.Bits (Bits((.&.), (.|.), xor))

import Declarations
    (Global(..),
    Heap(..),
    HeapCell(IND, DEF, APP, VALBool, VALNum, PRE, UNINITIALIZED),
    Instruction(..),
    Operator(..),
    Stack(Stack),
    StackCell(StackCell),
    State(..))
import Store
    (accessCode,
    accessHeap,
    accessStack,
    pushHeap,
    pushStack,
    saveHeap,
    saveStack)


---------------------------------------- MAIN EXECUTION CYCLE ----------------------------------------
-- | 'interpret' recursively executes a set of MF instructions. Either returns a valid state when instruction 'HALT' is reached, or an error occurs.
interpret :: State -> State
interpret (ErrorState error) = ErrorState error
interpret s                  = case accessCode (code s) (pc s) of
    Right instr -> case instr of
        Halt -> halt s
        _    -> case run instr s of
            ErrorState error -> ErrorState error
            s'               -> interpret s'
    Left error  -> ErrorState error

-- | Given an instruction, 'run' executes its respective MF function.
run :: Instruction -> State -> State
run Alloc s            = alloc s
run Call s             = call s
run (Error error) s    = ErrorState error
run (FuncUpdate n) s   = funcUpdate s n
run Halt s             = halt s
run Makeapp s          = makeapp s
run (Operator n) s     = operator s n
run OpUpdate s         = opUpdate s
run (Pushfun fname) s  = pushfun s fname
run (Pushparam addr) s = pushparam s addr
run (Pushpre kw) s     = pushpre s kw
run (Pushval typ val) s
    | typ == "Int" = pushval s 1 val
    | otherwise    = pushval s 2 val
run Reset s            = reset s
run Return s           = return' s
run (Slide n) s        = slide s n
run (SlideLet n) s     = slideLet s n
run Unwind s           = unwind s
run (UpdateLet n) s    = updateLet s n


---------------------------------------- MF FUNCTIONS ----------------------------------------
-- | 'call' equals to the DEF-case in MFmini reduce: calls a function, saves the return address
call :: State -> State
call s = case accessStack (stack s) (sp s) of
    Right (StackCell addr) -> case value (heap s) addr of
        Right (DEF _ _ addr1) -> s {pc = addr1, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
        Right (PRE op 2)      -> s {pc = 4, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
        Right (PRE IfOp 3)    -> s {pc = 13, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
        Right (PRE op 1)      -> s {pc = 21, sp = sp s + 1, stack = pushStack (stack s) (StackCell (pc s + 1))}
        Right (VALNum _)      -> s {pc = pc s + 1}
        Right (VALBool _)     -> s {pc = pc s + 1}
        Left error            -> ErrorState $ "Runtime error in 'call': " ++ error
        _                     -> ErrorState "Runtime error in 'call'."
    Left error             -> ErrorState $ "Runtime error in 'call': " ++ error

-- | 'funcUpdate'
funcUpdate :: State -> Int -> State
funcUpdate s arg = case accessStack (stack s) (sp s) of
    Right (StackCell addr) -> let hcell = IND addr in case accessStack (stack s) (sp s - arg - 2) of
        Right (StackCell addr1) -> case saveHeap (heap s) hcell addr1 of
            Right heap -> s {pc = pc s + 1, heap = heap}
            Left error -> ErrorState $ "Runtime error in 'funcUpdate': " ++ error
        Left error              -> ErrorState $ "Runtime error in 'funcUpdate': " ++ error
    Left error             -> ErrorState $ "Runtime error in 'funcUpdate': " ++ error

-- | 'halt' function returns the given state unchanged. Implemented for the sake of completeness though not really necessary.
halt :: State -> State
halt s = s

-- | 'makeapp' creates new APP-cell and saves its address on the stack
makeapp :: State -> State
makeapp s = case accessStack (stack s) (sp s) of
    Right (StackCell a) -> case accessStack (stack s) (sp s - 1) of
        Right (StackCell b) -> let (n, heap') = newAPP (heap s) a b in
            case saveStack (stack s) (StackCell n) (sp s - 1) of
                Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap'}
                Left error           -> ErrorState $ "Runtime error in 'makeapp': " ++ error
        Left error         -> ErrorState $ "Runtime error in 'makeapp': " ++ error
    Left error         -> ErrorState $ "Runtime error in 'makeapp': " ++ error

operator :: State -> Int -> State
operator s op = case op of
    1 -> case accessStack (stack s) (sp s - 2) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (PRE op 1) -> case accessStack (stack s) (sp s - 1) of
                Right scell -> case saveStack (stack s) scell (sp s - 2) of
                    Right stack   -> case accessStack stack (sp s) of
                        Right (StackCell addr1) -> case value (heap s) addr1 of
                            Right (VALBool bool) -> case op of
                                NotOp -> if fromEnum bool == 1
                                            then let (n, heap') = newVAL (heap s) 2 0 in case saveStack stack (StackCell n) (sp s - 1) of
                                                Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap'}
                                                Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                            else let (n, heap') = newVAL (heap s) 2 1 in case saveStack stack (StackCell n) (sp s - 1) of
                                                Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap'}
                                                Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                _   -> ErrorState $ "Runtime error: Boolean " ++ boolify (show bool) ++ " can not be evaluated with operator " ++ show op ++ "."
                            Right (VALNum w)  -> case op of
                                UnaryMinOp -> let (n, heap') = newVAL (heap s) 1 (- w) in case saveStack stack (StackCell n) (sp s - 1) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap'}
                                    Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                _         -> ErrorState $ "Runtime error in 'operator': Operator '" ++ show op ++ "' must not be applied to integer " ++  show w ++ "."
                            Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                            _                    -> ErrorState "Runtime error in 'operator'."
                        Left error -> ErrorState $ "Runtime error in 'operator': " ++ error
                    Left error -> ErrorState $ "Runtime error in 'operator': " ++ error
                Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
            Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
            _                    -> ErrorState "Runtime error in 'operator'."
        Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
    2 -> case accessStack (stack s) (sp s - 3) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (PRE op 2) -> case accessStack (stack s) (sp s - 2) of
                Right scell -> case saveStack (stack s) scell (sp s - 4) of
                    Right stack -> case accessStack stack (sp s - 1) of
                        Right (StackCell addr1) -> let hcell1 = value (heap s) addr1 in
                            case accessStack stack (sp s) of
                                Right (StackCell addr2) -> let hcell2 = value (heap s) addr2 in
                                    case hcell1 of
                                        Right (VALBool bool1) -> case hcell2 of
                                            Right (VALBool bool2) -> case op of
                                                AndOp    -> let (n, heap') = newVAL (heap s) 2 (bool1 .&. bool2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                OrOp     -> let (n, heap') = newVAL (heap s) 2 (bool1 .|. bool2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                EqualsOp -> let (n, heap') = newVAL (heap s) 2 (fromEnum $ bool1 == bool2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                LessOp   -> let (n, heap') = newVAL (heap s) 2 (fromEnum $ bool1 < bool2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                _      -> ErrorState $ "Runtime error: Booleans " ++ boolify (show bool1) ++ " and " ++ boolify (show bool2) ++ " can not be evaluated with operator " ++ show op ++ "."
                                            Right (VALNum num)    -> ErrorState $ "Runtime error: Boolean " ++ boolify (show bool1) ++ " and integer " ++ show num ++ " can not be evaluated with operator " ++ show op ++ "."
                                            Left error            -> ErrorState $ "Runtime error in 'operator': " ++ error
                                            _                     -> ErrorState "Runtime error in 'operator'."
                                        Right (VALNum num1) -> case hcell2 of
                                            Right (VALNum num2) -> case op of
                                                EqualsOp -> let (n, heap') = newVAL (heap s) 2 (fromEnum $ num1 == num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                LessOp   -> let (n, heap') = newVAL (heap s) 2 (fromEnum $ num1 < num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                PlusOp   -> let (n, heap') = newVAL (heap s) 1 (num1 + num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                TimesOp  -> let (n, heap') = newVAL (heap s) 1 (num1 * num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                BinaryMinOp  -> let (n, heap') = newVAL (heap s) 1 (num1 - num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                DivideOp -> if num2 == 0
                                                                then ErrorState "Runtime error: Division by zero."
                                                                else let (n, heap') = newVAL (heap s) 1 (num1 `div` num2) in
                                                    case saveStack stack (StackCell n) (sp s - 3) of
                                                        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack (take (sp s - 2) scells), heap = heap'}
                                                        Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                                _      -> ErrorState $ "Runtime error: Integers " ++ show num1 ++ " and " ++ show num2 ++ " can not be evaluated with operator " ++ show op ++ "."
                                            Right (VALBool bool) -> ErrorState $ "Runtime error: Integer " ++ show num1 ++ " and boolean " ++ boolify (show bool) ++ " can not be evaluated with operator " ++ show op ++ "."
                                            Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                            _                    -> ErrorState "Runtime error in 'operator'. 6"
                                        _                   -> ErrorState $ "Runtime error in 'operator': Neither integer nor boolean accessed at heapcell " ++ show hcell1 ++ "."
                                Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
                        Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
                    Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
                Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
            Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
            _                   -> ErrorState $ "Runtime error in 'operator': No binary operator accessed at heapcell " ++ show (value (heap s) addr)
        Left error          -> ErrorState $ "Runtime error in 'operator': " ++ error
    3 -> case accessStack (stack s) (sp s) of
        Right (StackCell addr) -> case value (heap s) addr of
            Right (VALBool bool) -> case bool of
                1  -> case accessStack (stack s) (sp s - 4) of
                    Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                        Right addr2 -> case saveStack (stack s) (StackCell addr2) (sp s - 3) of
                            Right stack -> case accessStack stack (sp s - 1) of
                                Right scell -> case saveStack stack scell (sp s - 4) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $ (take $ sp s - 2) scells}
                                    Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                            Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                        Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                    Left error              -> ErrorState $ "Runtime error in 'operator': " ++ error
                0 -> case accessStack (stack s) (sp s - 5) of
                    Right (StackCell addr1) -> case add2arg (heap s) addr1 of
                        Right addr2 -> case saveStack (stack s) (StackCell addr2) (sp s - 3) of
                            Right stack -> case accessStack stack (sp s - 1) of
                                Right scell -> case saveStack stack scell (sp s - 4) of
                                    Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 3, stack = Stack $ (take $ sp s - 2) scells}
                                    Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
                                Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                            Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                        Left error  -> ErrorState $ "Runtime error in 'operator': " ++ error
                    Left error              -> ErrorState $ "Runtime error in 'operator': " ++ error
                _ -> ErrorState "Runtime error in 'operator'."
            Left error           -> ErrorState $ "Runtime error in 'operator': " ++ error
            _                    -> ErrorState "Runtime error: 'If' expression expects boolean value."
        Left error              -> ErrorState $ "Runtime error in 'operator': " ++ error
    _ -> ErrorState "Runtime error in 'operator'."

opUpdate :: State -> State
opUpdate s = case accessStack (stack s) (sp s) of
    Right (StackCell addr) -> case accessHeap (heap s) addr of
        Right hcell -> case accessStack (stack s) (sp s - 2) of
            Right (StackCell addr1) -> case saveHeap (heap s) hcell addr1 of
                Right heap -> case accessStack (stack s) (sp s - 2) of
                    Right scell -> case accessStack (stack s) (sp s - 1) of
                        Right scell1 -> case saveStack (stack s) scell1 (sp s - 2) of
                            Right stack -> case saveStack stack scell (sp s - 1) of
                                Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                                Left error           -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
                            Left error             -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
                        Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
                    Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
                Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
            Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
        Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error
    Left error            -> ErrorState $ "Runtime error in 'opUpdate': " ++ error

-- | 'pushfun' pushes the heap address of a given function name on the stack.
-- | pushfun state nameOfFunction
pushfun :: State -> String -> State
pushfun s name = case address (global s) name of
    Right int  -> s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell int)}
    Left error -> ErrorState error

-- | 'pushparam' calls 'add2arg' and pushes the returned address of the second argument on the stack.
-- | pushparam state adr
pushparam :: State -> Int -> State
pushparam s n = case accessStack (stack s) (sp s - n - 1) of
    Right (StackCell addr) -> case add2arg (heap s) addr of
        Right addr1 -> s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell addr1)}
        Left error -> ErrorState $ "Runtime error in 'pushparam': " ++ error
    Left error -> ErrorState $ "Runtime error in 'pushparam': " ++ error

-- | 'pushpre' loads pre defined function by creating a new PRE-cell and pushing its address on the stack.
pushpre :: State -> Operator -> State
pushpre s op = let (n, heap') = newPRE (heap s) op (arity' op) in
    case saveStack (stack s) (StackCell n) (sp s + 1) of
            Right stack -> s{pc = pc s + 1, sp = sp s + 1, stack = stack, heap = heap'}
            Left error  -> ErrorState $ "Runtime error in 'pushpre': " ++ error

-- | 'pushpre' creates new VAL-cell and pushes its address on the stack.
-- | pushpre state type value
pushval :: State -> Int -> Int -> State
pushval s t w = s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell n), heap = heap'}
  where
    (n, heap') = newVAL (heap s) t w

-- | 'reset' initializes state in the beginning of MF run through.
reset :: State -> State
reset s = s {sp = -1, pc = pc s + 1}

-- | 'return'' returns to previously saved code address by changing the programm counter.
return' :: State -> State
return' s = case accessStack (stack s) (sp s - 1) of
    Right (StackCell addr) -> case accessStack (stack s) (sp s) of
        Right scell -> case saveStack (stack s) scell (sp s - 1) of
            Right (Stack scells) -> s {pc = addr, sp = sp s - 1, stack = Stack (take (sp s) scells)}
            Left error           -> ErrorState $ "Runtime error in 'return'': " ++ error
        Left error             -> ErrorState $ "Runtime error in 'return'': " ++ error
    Left error            -> ErrorState $ "Runtime error in 'return'': " ++ error

-- | 'slide' clears the stack by n cells below the top two cells
slide :: State -> Int -> State
slide s n = case accessStack (stack s) (sp s - 1) of
    Right scell -> case saveStack (stack s) scell (sp s - n - 1) of
        Right stack -> case accessStack stack (sp s) of
            Right scell1 -> case saveStack stack scell1 (sp s - n) of
                Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
                Left error           -> ErrorState $ "Runtime error in 'slide': " ++ error
            Left error    -> ErrorState $ "Runtime error in 'slide': " ++ error
        Left error -> ErrorState $ "Runtime error in 'slide': " ++ error
    Left error -> ErrorState $ "Runtime error in 'slide': " ++ error

-- | 'unwind' equals to the APP-case in MFmini reduce: exposes the backbone
unwind :: State -> State
unwind s = case accessStack (stack s) (sp s) of
    Right (StackCell addr) -> case value (heap s) addr of
        Right (APP addr1 _) -> s {sp = sp s + 1, stack = pushStack (stack s) (StackCell addr1)}
        Right hcell         -> s {pc = pc s + 1}
        Left error          -> ErrorState $ "Runtime error in 'unwind': " ++ error
    Left error             -> ErrorState $ "Runtime error in 'undwind': " ++ error

--------------------------------- MF FUNCTIONS EXTENSION ---------------------------------
-- | 'slideLet' clears the stack by n cells below the uppermost cell
slideLet :: State -> Int -> State
slideLet s n = case accessStack (stack s) (sp s) of
    Right scell -> case saveStack (stack s) scell (sp s - n) of
        Right (Stack scells) -> s {pc = pc s + 1, sp = sp s - n, stack = Stack (take (sp s - n + 1) scells)}
        Left error           -> ErrorState $ "Runtime error in 'slideLet': " ++ error
    Left error  -> ErrorState $ "Runtime error in 'slideLet': " ++ error

-- | 'updateLet'
updateLet :: State -> Int -> State
updateLet s@State{stack = Stack scells} n = case accessStack (stack s) (sp s - n - 1) of
    Right (StackCell addr) -> case add2arg (heap s) addr of
        Right addr1 -> case accessStack (stack s) (sp s) of
            Right (StackCell addr2) -> case saveHeap (heap s) (IND addr2) addr1 of
                Right heap -> s {pc = pc s + 1, sp = sp s - 1, stack = Stack (take (sp s) scells), heap = heap}
                Left error -> ErrorState $ "Runtime error in 'updateLet': " ++ error
            Left error              -> ErrorState $ "Runtime error in 'updateLet': " ++ error
        Left error  -> ErrorState $ "Runtime error in 'updateLet': " ++ error
    Left error             -> ErrorState $ "Runtime error in 'updateLet': " ++ error
updateLet (ErrorState error) _            = ErrorState $ "Runtime error in 'updateLet': " ++ error

-- | 'alloc' creates new UNI-cell and pushes its address on the stack
alloc :: State -> State
alloc s = s {pc = pc s + 1, sp = sp s + 1, stack = pushStack (stack s) (StackCell n), heap = heap'}
  where
    (n, heap') = newUNI (heap s)

---------------------------------------- HELPER FUNCTIONS FOR MF ----------------------------------------
-- | 'address' returns the heap address of a given function name if successful, otherwise it returns an error.
address :: Global -> String -> Either String Int
address (Global gcells) f = address' gcells f
  where
    address' :: [(String, Int)] -> String -> Either String Int
    address' ((fname, n) : xs) f = if fname == f then return n else address' xs f
    address' [] f                = Left $ "Runtime error: Function '" ++ f ++ "' not found."

-- | If 'add2arg' accesses an APP-cell, it returns its second argument. If it accesses an IND cell, it recursively calls itself with its argument.
add2arg :: Heap -> Int -> Either String Int
add2arg h@(Heap hcells) addr
    | (addr >= 0) && (addr < length hcells) = let hcell = hcells !! addr in case hcell of
        APP addr1 addr2 -> return addr2
        IND addr3       -> add2arg h addr3
        _               -> Left $ "Expected an APP-cell, found " ++ show hcell ++ " instead."
    | otherwise                             = Left $ "No heapcell at index " ++ show addr ++ "."

-- | 'arity'' returns the number of formal arguments of an operator.
arity' :: Operator -> Int
arity' op = case op of
    AndOp       -> 2
    BinaryMinOp -> 2
    DivideOp    -> 2
    EqualsOp    -> 2
    IfOp        -> 3
    LessOp      -> 2
    NotOp       -> 1
    OrOp        -> 2
    PlusOp      -> 2
    TimesOp     -> 2
    UnaryMinOp  -> 1

-- | 'boolify' returns more readable output for boolean values.
boolify :: String -> String
boolify x = if x == "0" then "'false'" else "'true'"

-- | 'interpretVerbose' returns the emulation trace as a string.
interpretVerbose :: State -> String
interpretVerbose (ErrorState error) = error
interpretVerbose s                  = case accessCode (code s) (pc s) of
        Right instr -> case instr of
            Halt -> resultToString s
            _    -> case run instr s of
                ErrorState error -> error
                s'               -> show s ++ "\n" ++ interpretVerbose s'
        Left error  -> error

-- | 'new...' functions create different types of heap cells.
newAPP, newVAL :: Heap -> Int -> Int -> (Int, Heap)
newAPP h@(Heap hcells) a b = (length hcells, pushHeap h (APP a b))

-- | 'newVAL' creates a heapcell of type 't' (1 for integers, 2 for booleans) with value 'w'.
newVAL h@(Heap hcells) t w
        | t == 1    = (length hcells, pushHeap h (VALNum w))
        | otherwise = (length hcells, pushHeap h (VALBool w))

-- | 'newIND' pushes a heapcell of type IND (pointer cell)
newIND :: Heap -> Int -> (Int, Heap)
newIND h@(Heap hcells) n = (length hcells, pushHeap h (IND n))

-- | 'newPRE' pushes a heapcell of type PRE (pre defined)
newPRE :: Heap -> Operator -> Int -> (Int, Heap)
newPRE h@(Heap hcells) op n = (length hcells, pushHeap h (PRE op $ arity' op))

-- | 'newPRE' pushes a heapcell of type UNI (uninitialized)
newUNI :: Heap -> (Int, Heap)
newUNI h@(Heap hcells) = (length hcells, pushHeap h UNINITIALIZED)

-- | 'resultToString' takes the last state of interpret and returns the result string
resultToString :: State -> String
resultToString (ErrorState error) = "---> " ++ error
resultToString s                  = case accessStack (stack s) (sp s) of
    Right (StackCell addr) -> case accessHeap (heap s) addr of
        Right (VALNum n)  -> "---> Result: " ++ show n
        Right (VALBool b) -> if b == 0 then "---> Result: false" else "---> Result: true"
        Right (IND m)     -> case value (heap s) m of
            Right (VALNum n)  -> "---> Result: " ++ show n
            Right (VALBool b) -> "---> Result: " ++ show b
            Left error        -> "---> " ++ error
            _                 -> "---> Runtime error."
        _                 -> "---> Runtime error."
    Left error             -> "---> " ++ error

-- | 'value' returns the heap cell pointed at by IND-cell
value :: Heap -> Int -> Either String HeapCell
value heap addr = case accessHeap heap addr of
    Right (IND addr1) -> value heap addr1
    Right hcell       -> Right hcell
    Left error        -> Left error
