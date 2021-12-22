data Instruction = Reset | Pushfun ____ | Pushval __ __ | Pushparam __ | Makeapp | Slide __ | Reduce | Return | Halt deriving (Eq, Show)

data State = State 
{
	stack :: Stack StackF,
	heap :: ___ HeapF,
	stackPointer :: Int,
	pCounter :: Int,
	code:: [Instruction]
} deriving (Show)

