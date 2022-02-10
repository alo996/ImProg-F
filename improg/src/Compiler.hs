{- |
Module      : Compiler
Description : This module contains all functionality to translate a parsed F program into a set of MF instructions, while setting up the global environment and heap for the interpretation process.
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


---------------------------------------- F PROGRAM TRANSLATION ----------------------------------------
-- | Compile a program. 'compileProgram' takes a program (a list of definitions) and returns the initial machine state if successful.
compileProgram :: [Def] -> State
compileProgram defs = compileProgram' defs s
  where
    s = createGlobal defs State {pc = 0, sp = -1, code = codeInit, stack = Stack [], global = Global [], heap = Heap []}
    -- | 'compileProgram'' recursively compiles each definition of the program.
    compileProgram' :: [Def] -> State -> State
    compileProgram' _ (ErrorState error) = ErrorState error
    compileProgram' (d : ds) s           = compileProgram' ds (compileDefinition d s)
    compileProgram' [] s                 = lookupMain $ heap s
      where
        -- | After compiling all definitions, 'lookupMain' checks whether the function 'main' has been specified as required.
        lookupMain :: Heap -> State
        lookupMain (Heap (DEF {fname, arity} : hs)) 
            | fname == "main" && arity == 0 = s 
            | fname == "main" && arity > 0  = ErrorState "Runtime error: Function 'main' is not correctly defined." 
            | otherwise                     = lookupMain (Heap hs)
        lookupMain _  = ErrorState "Runtime error: Function 'main' is not defined."

-- | Compile a definition. 'compileDefinition' takes the definition to compile and the current machine state and updates its code, global environment and heap.
compileDefinition :: Def -> State -> State
compileDefinition (Def (AtomicExpr (Var fname)) es e) s@State{} = case checkDuplicate fname s of
  ErrorState error                                 -> ErrorState error 
  s'@State{code = Code ccells, heap = Heap hcells} ->
    s'
    {
        code = Code (ccells ++ compileExpression e localenv (global s') ++ [FuncUpdate len, Slide (len + 1), Unwind, Call, Return]),
        heap = Heap (hcells ++ hcell)
    }     
      where 
        localenv = createPos es
        len = length localenv
        hcell = [DEF fname (length localenv) (length ccells)]
compileDefinition _ _                                           = ErrorState "Error in 'compileDefinition'."

{- | Compile an expression. 'compileExpression' takes the expression to compile, a local environment and the already set up global environment of the program and returns a list of MF instructions.
-}
compileExpression :: Expr -> [(Expr, Int)] -> Global -> [Instruction]
compileExpression e pos g = case e of
    AtomicExpr (LitBool (BoolF False))  -> [Pushval "Bool" 0]
    AtomicExpr (LitBool (BoolF True))   -> [Pushval "Bool" 1]
    AtomicExpr (LitNum n)               -> [Pushval "Int" n]
    AtomicExpr (Var name)               -> case pos of
        [] -> [Pushfun name]
        _  -> posInd e pos g
    AtomicExpr (Expr e)                 -> compileExpression e pos g
    Func e1@(AtomicExpr (Var fname)) e2 -> case posInd e1 pos g of
      [Pushparam n]  -> compileExpression e2 pos g ++ [Pushparam $ n+1, Makeapp]
      [Pushfun name] -> compileExpression e2 pos g ++ [Pushfun name, Makeapp]
      _              -> compileExpression e2 pos g ++ [Pushfun fname, Makeapp]
    Func e1 e2                          -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Makeapp]
    LetIn ldefs e                       -> compileLocalDefinitions ldefs e pos g
    Add e1 e2                           -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre PlusOp, Makeapp, Makeapp]
    Mult e1 e2                          -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre TimesOp, Makeapp, Makeapp]
    Div e1 e2                           -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre DivideOp, Makeapp, Makeapp]
    Equal e1 e2                         -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre EqualsOp, Makeapp, Makeapp]
    LessThan e1 e2                      -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre LessOp, Makeapp, Makeapp]
    LogicalAnd e1 e2                    -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre AndOp, Makeapp, Makeapp]
    LogicalOr e1 e2                     -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre OrOp, Makeapp, Makeapp]
    BinaryMin e1 e2                     -> compileExpression e2 pos g ++ compileExpression e1 (posInc pos 1) g ++ [Pushpre BinaryMinOp, Makeapp, Makeapp]
    UnaryMin e                          -> compileExpression e pos g ++ [Pushpre UnaryMinOp, Makeapp]
    LogicalNot e                        -> compileExpression e pos g ++ [Pushpre NotOp, Makeapp]
    IfThenElse e1 e2 e3                 ->
        compileExpression e3 pos g ++ compileExpression e2 (posInc pos 1) g ++ compileExpression e1 (posInc pos 2) g ++ [Pushpre IfOp, Makeapp, Makeapp, Makeapp]

{- | Compile local definitions. 'compileLocalDefinitions' takes a list of local definitions, the expression for which the local definitions have been specified, a local environment and the already set up global environment of the program.
It returns a list of MF instructions.
-}
compileLocalDefinitions :: [LocalDef] -> Expr -> [(Expr, Int)] -> Global ->[Instruction]
compileLocalDefinitions ldefs e pos s = compileLocalDefinitions' ldefs e (replicate' [Alloc, Alloc, Makeapp] len) pos' s len len
  where
    len = length ldefs
    pos' = createPos' ldefs pos len
    {- | 'compileLocalDefinitions'' recursively iterates through a list of local definitions and compiles each one of them. 
    It takes the list of local definitions to be compiled, the expression these local definitions correspond to, the list of already created MF instructions as an accumulator, the local environment of the function, the number of local definitions to be compiled as argument for the 'SlideLet' instruction after successful compilation and the number of remaining local definitions after each step as argument for the corresponding 'UpdateLet' instruction.
    -}
    compileLocalDefinitions' :: [LocalDef] -> Expr -> [Instruction] -> [(Expr, Int)] -> Global -> Int -> Int -> [Instruction]
    compileLocalDefinitions' ((LocalDef e1 e2) : ldefs) e ccells pos' g n acc =
        compileLocalDefinitions' ldefs e (ccells ++ compileExpression e2 pos' g ++ [UpdateLet $ acc-1]) pos' g n (acc-1)
    compileLocalDefinitions' [] e ccells pos' g n _                           = ccells ++ compileExpression e pos' g ++ [SlideLet n]


---------------------------------------- HELPER FUNCTIONS FOR COMPILER ----------------------------------------
-- | 'checkDuplicate' returns True if the definition of a function 'f' does not already exist in a heap.
checkDuplicate :: String -> State -> State
checkDuplicate f s = checkDuplicate' f s (heap s)
  where
    -- 'checkDuplicate'' recursively iterates through the heap and checks whether a function 'f' has already been specified.
    checkDuplicate' :: String -> State -> Heap -> State
    checkDuplicate' f s (Heap (DEF{fname} : hcells))
        | f == fname = ErrorState $ "Runtime error in 'checkDuplicate'': Multiple declarations of function '" ++ f ++ "'."
        | otherwise  = checkDuplicate' f s (Heap hcells)
    checkDuplicate' _ s (Heap []) = s
    checkDuplicate' _ _ _         = ErrorState "Runtime error 'checkDuplicate'': Heap set up incorrectly."

-- | Create the global environment.
createGlobal :: [Def] -> State -> State
createGlobal defs s = createGlobal' defs s 0 
  where
    -- 'createGlobal'' loops through all definitions of a program and adds (function name, heap position)-pairs to the global environment. 
    createGlobal' :: [Def] -> State -> Int -> State
    createGlobal' 
        ((Def (AtomicExpr (Var fname)) _ _) : defs) 
        s@State{global = Global gcells} acc 
          = createGlobal' defs s {global = Global (gcells ++ [(fname, acc)])} (acc + 1)
    createGlobal' [] s _ = s
    createGlobal' _ _ _  = ErrorState "Runtime error in 'createGlobal'': Global environment set up incorrectly."
      
-- | Create a local environment for a given list of formal parameters.
createPos :: [Expr] -> [(Expr, Int)]
createPos es = zip es [1..]

-- | Extend a local environment by adding let bindings to its front.
createPos' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
createPos' ldefs pos n = createPos'' ldefs pos' (length ldefs - 2)
  where
    pos' = posInc pos n
    {- | 'createPos''' takes a list of local definitions, a local environment and an index.
    It recursively iterates through the list of local definitions, adding (variable, index)-bindings to the front of the local environment.
    -}
    createPos'' :: [LocalDef] -> [(Expr, Int)] -> Int -> [(Expr, Int)]
    createPos'' ((LocalDef e1 _) : ldefs) pos n = createPos'' ldefs ((e1, n) : pos) (n - 1)
    createPos'' [] pos _                        = pos

-- | Increment all indices in a local environment by an offset.
posInc :: [(Expr, Int)] -> Int -> [(Expr, Int)]
posInc pos n = map (\ (a, b) -> (a, b + n)) pos

-- | Get the index of the first occurence of a formal parameter in a given local environment, or look for a global function with the same name.
posInd :: Expr -> [(Expr, Int)] -> Global -> [Instruction]
posInd e@(AtomicExpr (Var name)) pos g@(Global gcells) = case lookup e pos of
    Just n -> [Pushparam n]
    _      -> case lookup name gcells of
      Just n' -> [Pushfun name]
      _       -> [Error $ "Runtime error: Function '" ++ name ++ "' not found."]
posInd _ _ _                                           = [Error "Runtime error in 'posInd': Local environment indexed incorrectly."]

-- | Replicate a list n-1 times and concatenate.
replicate' :: [a] -> Int -> [a]
replicate' xs n
    | n > 1     = xs ++ replicate' xs (n - 1)
    | otherwise = xs
    