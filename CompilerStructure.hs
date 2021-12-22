-- input: a list of definitions
-- output: initial machine state

data Instruction = Reset | Pushfun String | Pushval String Int | Pushparam Int | Makeapp | Slide Int | Reduce | Halt deriving (Show)
--newtype State = State (Int, [Instruction], Stack, Heap, Global) not needed if using record syntax?
-- consists of a program counter, generated instructions, a stack, a heap and a global environment
-- hints: record syntax, pushes into the stack from the left end
data State = State
    {
        pc :: Int,
		sp :: Int,
        code :: [Instruction],
        stack :: [StackCell],
        heap :: HeapCell
    } deriving Show


data HeapCell = DEF String Int Int | APP Int Int | VAL String Int deriving (Show)
type StackCell = Int

reset :: State -> State
reset s = {pc = pc + 1, sp = -1}


compileProgram :: [Definition] -> State
-- a program consists of multiple definitions
-- we have to translate them one by one

compileDefinition
-- page 77 in the script: ÜbDef
-- translates one definition into a list of instructions
-- calls translateExpression to translate the body

compileExpression
-- page 78 in the script: ÜbKons
-- translates one expression into a list of instructions

-- pay attention to the local environment Pos (page 77)
