-- input: a list of definitions
-- output: initial machine state
-- newtype State = State (Int, [Instruction], Stack, Heap, Global) not needed if using record syntax?
-- hints: record syntax, pushes into the stack from the left end
import Data.List
import Declarations
import Store

address :: Store GlobalCell -> String -> Maybe Int
address (GlobalEnv gcells) f = address' gcells 0 f where
    address' (x : xs) acc f = case x of
        DEF f _ _ -> return acc
        _         -> address' xs (acc + 1) f
    address' [] _ _         = Nothing
address _ _                 = Nothing

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
typ _             = -1


reset :: State -> State
reset s@State {pc = pc} = s {pc = pc + 1}


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