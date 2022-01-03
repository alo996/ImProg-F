-- input: a list of definitions
-- output: initial machine state
-- newtype State = State (Int, [Instruction], Stack, Heap, Global) not needed if using record syntax?
-- hints: record syntax, pushes into the stack from the left end
import Data.List
import Declarations
import Store
import Declarations (State(State))


address :: Store GlobalCell -> String -> Either String Int
address (GlobalEnv gcells) f = address' gcells 0 f where
    address' (x : xs) acc f = case x of
        DEF f _ _ -> return acc
        _         -> address' xs (acc + 1) f
    address' [] _ _         = Left $ "Compile error: Global environment does not contain definition of function '" ++ show f ++ "'."
address _ _                 = Left "Function 'address' not called on global environment."

add2arg :: Store HeapCell -> Int -> Either String Int
add2arg h@(Heap hcells) addr = if addr < depth h then let APP addr1 addr2 = hcells !! addr in return addr2 else Left $ "Compile error: Heap does not contain cell at address " ++ show addr ++ "'."
add2arg _ _                = Left "Function 'add2arg' not called on heap."

new :: Store HeapCell -> Int -> Int -> Int -> (Int, Store HeapCell)
new heap typ a b 
    | typ == 0  = (depth heap, push heap (APP a b))
    | typ == 1  = (depth heap, push heap (VALNum 1 b))
    | otherwise = case b of
        0 -> (depth heap, push heap (VALBool 2 False))
        _ -> (depth heap, push heap (VALBool 2 True))

typ :: HeapCell -> Int
typ (APP _ _)     = 0
typ (VALNum _ _)  = 1
typ (VALBool _ _) = 2

-- error state einführen, der in Loop abgefangen wird

-- let beginningState = State 0 emptyCode emptyStack emptyHeap emptyGlobalEnv

reset :: State -> State
reset s = s {sp = -1, pc = pc s + 1}

pushFun :: State -> String -> State
pushFun s@State{pc = pc, stack = stack, global = global} f = do
    funAdr <- address global f
    newStack <- assignStackCellAdr stack pc funAdr -- TODO: error-state kreieren, wenn nothing zurückkommt
    if isNothing newStack then
        s -- errorstate
    else
        s{pc = pc+1, stack = newStack}
-- pushfun s name = s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell (fromRight (-1) (address (global s) name)))}



pushValNum :: State -> Int -> State
pushValNum s@State{pc = pc, stack = stack, heap = heap} val = do
    let heapadr@(adr, newheap) = newVALNum heap val
    newStack <- assignStackCellAdr stack pc heapadr
    if isNothing newStack then
        s -- errorstate
    else
        s{pc = pc+1, heap = newheap, stack = newStack}


pushValNum :: State -> Bool -> State
pushValNum s@State{pc = pc, stack = stack, heap = heap} val = do
    let heapadr@(adr, newheap) = newVALBool heap val
    newStack <- assignStackCellAdr stack pc heapadr
    if isNothing newStack then
        s -- errorstate
    else
        s{pc = pc+1, heap = newheap, stack = newStack}

-- pushval :: State -> Int -> Int -> Int -> State
pushval s typ a b = s {pc = pc s + 1, sp = sp s + 1, stack = push (stack s) (StackCell (fst (new (heap s) typ a b))), heap = snd (new (heap s) typ a b)}


pushParam :: State -> Int -> State
-- ...


pushfun :: State -> String -> State
pushfun s name = s {pc = pc s + 1, sp = sp s + 1, stack = save (stack s) (StackCell (address (global s) name)) (sp s)}
-- update pc, update sp, save a new stack that now has the address of function 'name' in the global environment at position 'sp s'

{-
    data State
        = State {
        pc :: Int,
        sp :: Int,
        code :: Store Instruction,
        stack :: Store StackCell,
        heap :: Store HeapCell,
        global :: Store GlobalCell
        } deriving Show
-}

--compileProgram :: [Definition] -> State
-- a program consists of multiple definitions
-- we have to translate them one by one

--compileDefinition
-- page 77 in the script: ÜbDef
-- translates one definition into a list of instructions
-- calls translateExpression to translate the body

--compileExpression
-- page 78 in the script: ÜbKons
-- translates one expression into a list of instructions

-- pay attention to the local environment Pos (page 77)
