module Compiler where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe
    import MiniMF

    compileProgram :: [Def] -> State
    compileProgram defs = let s = State {pc = 0, sp = -1, code = Code [], stack = Stack [], heap = Heap [], global = Heap []} in compileProgram' defs s where
        compileProgram' (d : ds) state = compileProgram' ds (compileDefinition d state)
        compileProgram' [] state       = state

    compileDefinition :: Def -> State -> State
    compileDefinition (Def e1 es e2) s@State{code = Code ccells, global = GlobalEnv hcells} = let localenv = posList es in
        s {code = Code (ccells ++ compileExpression e2 0 localenv ++ [Slide (length localenv + 1), Reduce, Return]), global = push (global s) (DEF (show e1) (length localenv) (depth (code s)))}
    compileDefinition _ _                                                                   = ErrorState "error"

    compileExpression :: Expr -> Int -> [(Expr, Int)] -> [Instruction]
    compileExpression e num pos = case e of
            AtomicExpr (LitBool (BoolF False)) -> [Pushval "Bool" 0]
            AtomicExpr (LitBool (BoolF True))  -> [Pushval "Bool" 1]
            AtomicExpr (LitNum num)            -> [Pushval "Int" num]
            AtomicExpr (Var name)              -> case posInd e num pos of
                Just ind -> [Pushparam ind]
                _        -> [Error]
            AtomicExpr (Expr expr)             -> compileExpression expr num pos
            Func e1 e2                         -> compileExpression e2 num pos ++ [Pushfun $ show e, Makeapp]
            _                                  -> [Error]

    posList :: [Expr] -> [(Expr, Int)]
    posList es = posList' es 1 where
        posList' (x : xs) acc = (x, acc) : posList' xs (acc + 1)
        posList' [] acc       = []

    -- Get the index of the first occurence of a formal parameter in the local environment
    posInd :: Expr -> Int -> [(Expr, Int)] -> Maybe Int
    posInd e num pos = case lookup e pos of
        Just ind -> Just (ind + num)
        _        -> Nothing
