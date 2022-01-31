{- |
Module      : Compiler
Description : This module contains all functionality to translate a parsed F program into a set MF instructions, while setting up the global environment and heap for the interpretation process.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Compiler where

import Declarations
    (AtomicExpr(Var, LitBool, LitNum, Expr),
    BoolF(BoolF),
    Code(Code),
    Def(..),
    Expr(..),
    Global(..),
    Heap(..),
    HeapCell(DEF, fname, arity),
    Instruction(FuncUpdate, Slide, Unwind, Call, Return, Pushval,
                Pushparam, Error, Pushfun, Pushpre, Makeapp, UpdateLet, SlideLet,
                Alloc),
    LocalDef(..),
    Operator(IfOp, PlusOp, TimesOp, DivideOp, EqualsOp, LessOp, AndOp,
              OrOp, BinaryMinOp, UnaryMinOp, NotOp),
    Stack(Stack),
    State(..))
import Store (codeInit)
import Tokenizer
import Parser


---------------------------------------- F PROGRAM TRANSLATION ----------------------------------------
-- | Compile a program. 'compileProgram' takes a program (a list of definitions) and returns the initial machine state if successful.
compileProgram :: [Def] -> State
compileProgram defs = compileProgram' defs s
  where
    s = State {pc = 0, sp = -1, code = codeInit, stack = Stack [], global = Global [], heap = Heap []}
    -- | 'compileProgram'' recursively compiles each definition of the program.
    compileProgram' :: [Def] -> State -> State
    compileProgram' _ (ErrorState error) = ErrorState error
    compileProgram' (def : defs) s       = compileProgram' defs (compileDefinition def s)
    compileProgram' [] s                 = lookupMain $ heap s
      where
        -- | After compiling all definitions, 'lookupMain' checks whether the function 'main' has been specified as required.
        lookupMain :: Heap -> State
        lookupMain (Heap (DEF {fname, arity} : hs)) = if fname == "main" && arity == 0 then s else lookupMain (Heap hs)
        lookupMain _                                = ErrorState "Runtime error: Function main not defined."

-- | Compile a definition. 'compileDefinition' takes the definition to compile and the current machine state and updates its code, global environment and heap.
compileDefinition :: Def -> State -> State
compileDefinition 
    (Def (AtomicExpr (Var fname)) es e) 
    s@State{code = Code ccells, global = Global gcells, heap = Heap hcells} 
      = if checkDuplicate fname (global s) 
        then s
              {
                code = Code (ccells ++ compileExpression e localenv ++ [FuncUpdate len, Slide (len + 1), Unwind, Call, Return]),
                global = Global (gcells ++ [(fname, length hcells)]),
                heap = Heap (hcells ++ hcell)
              }     
        else ErrorState $ "Runtime error: Multiple declarations of " ++ show fname ++ "."
          where 
            localenv = createPos es
            len = length localenv
            hcell = [DEF fname (length localenv) (length ccells)]
compileDefinition _ _ = ErrorState "Error in 'compileDefinition'"

-- | Compile an expression. 'compileExpression' takes the expression to compile and a local environment and returns a list of MF instructions.
compileExpression :: Expr -> [(Expr, Int)] -> [Instruction]
compileExpression e pos = case e of
    AtomicExpr (LitBool (BoolF False)) -> [Pushval "Bool" 0]
    AtomicExpr (LitBool (BoolF True))  -> [Pushval "Bool" 1]
    AtomicExpr (LitNum n)              -> [Pushval "Int" n]
    AtomicExpr (Var name)              -> case pos of
        [] -> [Pushfun name]
        _  -> case posInd e pos of
            Right n    -> [Pushparam n]
            Left error -> [Error error]
    AtomicExpr (Expr e)                -> compileExpression e pos
    Func (AtomicExpr (Var fname)) e2   -> compileExpression e2 pos ++ [Pushfun fname, Makeapp]
    Func e1 e2                         -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Makeapp]
    LetIn ldefs e                      -> compileLocalDefinitions ldefs e pos
    Add e1 e2                          -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre PlusOp, Makeapp, Makeapp]
    Mult e1 e2                         -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre TimesOp, Makeapp, Makeapp]
    Div e1 e2                          -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre DivideOp, Makeapp, Makeapp]
    Equal e1 e2                        -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre EqualsOp, Makeapp, Makeapp]
    LessThan e1 e2                     -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre LessOp, Makeapp, Makeapp]
    LogicalAnd e1 e2                   -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre AndOp, Makeapp, Makeapp]
    LogicalOr e1 e2                    -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre OrOp, Makeapp, Makeapp]
    BinaryMin e1 e2                    -> compileExpression e2 pos ++ compileExpression e1 (posInc pos 1) ++ [Pushpre BinaryMinOp, Makeapp, Makeapp]
    UnaryMin e                         -> compileExpression e pos ++ [Pushpre UnaryMinOp, Makeapp]
    LogicalNot e                       -> compileExpression e pos ++ [Pushpre NotOp, Makeapp]
    IfThenElse e1 e2 e3                ->
        compileExpression e3 pos ++ compileExpression e2 (posInc pos 1) ++ compileExpression e1 (posInc pos 2) ++ [Pushpre IfOp, Makeapp, Makeapp, Makeapp]

{- | Compile a local definition. 'compileLocalDefinitions' takes a list of local definitions, the corresponding expression and a local environment. 
It returns a list of MF instructions.
-}
compileLocalDefinitions :: [LocalDef] -> Expr -> [(Expr, Int)] -> [Instruction]
compileLocalDefinitions ldefs e pos = compileLocalDefinitions' ldefs e (replicate' [Alloc, Alloc, Makeapp] len) pos' len len
  where
    len = length ldefs
    pos' = createPos' ldefs pos len
    {- | 'compileLocalDefinitions'' recursively iterates through a list of local definitions and compiles them. 
    It takes the list of local definitions to compile, the expression these local definitions correspond to, the list of already created MF instructions as an accumulator, the local environment of the function, the number of local definitions to compile as argument for the 'SlideLet' instruction after successful compilation and the number of remaining local definitions after each step as argument for the corresponding 'UpdateLet' instruction.
    -}
    compileLocalDefinitions' :: [LocalDef] -> Expr -> [Instruction] -> [(Expr, Int)] -> Int -> Int -> [Instruction]
    compileLocalDefinitions' ((LocalDef e1 e2) : ldefs) e ccells pos' n acc =
        compileLocalDefinitions' ldefs e (ccells ++ compileExpression e2 pos' ++ [UpdateLet $ acc-1]) pos' n (acc-1)
    compileLocalDefinitions' [] e ccells pos' n _                           = ccells ++ compileExpression e pos' ++ [SlideLet n]


---------------------------------------- HELPER FUNCTIONS FOR COMPILER ----------------------------------------
-- | 'checkDuplicate' returns True if a new function name 'f1' does not already exist in a global environment.
checkDuplicate :: String -> Global -> Bool 
checkDuplicate f1 (Global []) = True 
checkDuplicate f1 (Global ((f2, _) : gcells))
    | f1 == f2  = False 
    | otherwise = checkDuplicate f1 (Global gcells)

-- | Create a local environment for a given list of formal parameters.
createPos :: [Expr] -> [(Expr, Int)]
createPos es = zip es [1..]

-- | Extend a local environment by adding let bindings to its front.
createPos' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
createPos' ldefs pos n = createPos'' ldefs pos' (length ldefs - 2)
  where
    pos' = posInc pos n
    {- | 'createPos''' takes a list of local definitions, a local environment and an integer.
    It recursively iterates through the list of local definitions, adding (variable, index)-bindings to the front of the local environment.
    -}
    createPos'' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
    createPos'' ((LocalDef e1 _) : ldefs) pos n = createPos'' ldefs ((e1, n) : pos) (n - 1)
    createPos'' [] pos _                        = pos

instructionsToString :: [Instruction] -> String
instructionsToString ins = foldl (++) "+———-----------+\n| Instructions |\n+——------------+\n" (map (\ i -> show i ++ "\n") ins)

-- | Increment all indices in a local environment by an offset.
posInc :: [(Expr, Int)] -> Int -> [(Expr, Int)]
posInc pos n = map (\ (a, b) -> (a, b + n)) pos

-- | Get the index of the first occurence of a formal parameter in a given local environment.
posInd :: Expr -> [(Expr, Int)] -> Either String Int
posInd e pos = case lookup e pos of
    Just n -> return n
    _      -> Left $ "Runtime error: Local environment " ++ show pos ++ " does not contain formal parameter " ++ show e ++ "."

-- | Replicate a list n-1 times and concatenate.
replicate' :: [a] -> Int -> [a]
replicate' xs n
    | n > 1     = xs ++ replicate' xs (n - 1)
    | otherwise = xs

testIns :: String -> IO ()
testIns input = case tokenize input of
    Right toks -> case program toks of
      Right ast  -> case compileProgram (fst ast) of
        ErrorState error           -> putStrLn error
        State {code = Code ccells} -> putStrLn $ instructionsToString ccells
      Left error -> putStrLn error
    Left error -> putStrLn error