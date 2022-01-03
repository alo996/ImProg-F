-- input: a list of definitions
-- output: initial machine state
-- newtype State = State (Int, [Instruction], Stack, Heap, Global) not needed if using record syntax?
-- hints: record syntax, pushes into the stack from the left end
import Data.List
import Declarations
import Store
import Declarations (State(State))


address :: Store GlobalCell -> String -> Maybe Int
address (GlobalEnv gcells) f = address' gcells 0 f where
    address' (x : xs) acc f = case x of
        DEF f _ _ -> acc
        _         -> address' xs (acc + 1) f
    address' [] _ _         = -1 -- not nice
address _ _                 = -1 -- not nice

add2arg :: Store HeapCell -> Int -> Maybe Int
add2arg (Heap hcells) addr = let APP addr1 addr2 = hcells !! addr in return addr2 -- unsafe
add2arg _ _                = Nothing

newAPP :: Store HeapCell -> Int -> Int -> Maybe (Int, Store HeapCell)
newAPP h@(Heap hcells) n m = return (depth h, push h (APP n m))
newAPP _ _ _               = Nothing

newVALNum :: Store HeapCell -> Int -> (Int, Store HeapCell)
newVALNum heap num = (depth heap, push heap (VALNum 0 num))

newVALBool :: Store HeapCell -> Bool -> (Int, Store HeapCell)
newVALBool heap bool = (depth heap, push heap (VALBool 1 bool))

typ :: HeapCell -> Int
typ (VALNum _ _)  = 0
typ (VALBool _ _) = 1
typ _             = -1 -- not nice

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
