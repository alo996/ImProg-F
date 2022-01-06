module Compiler where
    import Data.List
    import Declarations
    import Store
    import Data.Either
    import Data.Maybe
    import MiniMF

    {-
        data Instruction 
        = Reset 
        | Pushfun String 
        | Pushval String Int 
        | Pushparam Int 
        | Makeapp 
        | Slide Int 
        | Reduce 
        | Halt 
        deriving Show

        data Def 
        = Def Expr [Expr] Expr 
        deriving Show
        save :: Store a -> a -> Int -> Maybe (Store a)
    -}


    compileProgram :: [Def] -> State
    compileProgram = undefined
    --compileProgram defs = compileProgram' defs state where
    --    compileProgram' (d : ds) acc = compileProgram' ds (acc ++ compileDefinition d)
    --    compileProgram' [] acc       = let acc' = [Reset, Pushfun "main", Reduce, Halt] ++ acc in State {pc = 0, sp = -1, code = Code acc', stack = Stack [], heap = Heap [], global = GlobalEnv []}

    compileDefinition :: Def -> State -> State
    compileDefinition (Def e1 es e2) s@State{code = Code ccells, global = GlobalEnv hcells} = let localenv = posList es in
        s {code = Code (ccells ++ compileExpression e2 0 localenv ++ [Slide (length localenv + 1), Reduce, Return]), global = push (global s) (DEF (show e1) (length localenv) (depth (code s)))}
    compileDefinition _ _                                                                   = ErrorState "error"
    {-
            | DEF 
        {
            id :: String, 
            arity :: Int, 
            caddr :: Int
        }

        data Expr
        = Add Expr Expr
        | Func Expr Expr -- Func means function application
        | Mult Expr Expr
        | Div Expr Expr 
        | UnaryMin Expr
        | Equal Expr Expr
        | LessThan Expr Expr
        | LogicalAnd Expr Expr
        | LogicalOr Expr Expr
        | LogicalNot Expr
        | LetIn [LocalDef] Expr
        | IfThenElse Expr Expr Expr
        | AtomicExpr AtomicExpr
        deriving Show

        data AtomicExpr 
        = Var String
        | LitBool BoolF 
        | LitNum Int 
        | Expr Expr 
        deriving Show

    type Var = String
    -}

    {-
    compileExpression takes an expression (the defining expression of a function, e.g. in 'f x = 3 * x', then '3 * x' is the defining expression), 
    an offset for the pos-function (to fulfill pos + i(x) for recursive calculations) and the local environment of the function definition.
    -}
    compileExpression :: Expr -> Int -> [(Expr, Int)] -> [Instruction]
    compileExpression e num pos = case e of
            AtomicExpr (LitBool (BoolF False)) -> [Pushval "Bool" 0]
            AtomicExpr (LitBool (BoolF True))  -> [Pushval "Bool" 1]
            AtomicExpr (LitNum num)            -> [Pushval "Int" num]
            AtomicExpr (Var name)              -> case posInd e num pos of
                Just ind -> [Pushparam $ ind]
                _        -> [Error]
            AtomicExpr (Expr expr)             -> compileExpression expr num pos
            Func e es                          -> compileExpression e2 (num + 1) pos ++ [Pushfun $ show e1]
            Add e1 e2                          -> [Pushfun "+"] ++ 
    -- page 78 in the script: ÜbKons
    -- translates one expression into a list of instructions
    -- pay attention to the local environment Pos (page 77)

    {-
    Create the local environment as a list of tuples, 
    each containing a formal parameter and its position in the list of formal parameters
    -}
    posList :: [Expr] -> [(Expr, Int)]
    posList es = posList' es 1 where
        posList' (x : xs) acc = (x, acc) : posList' xs (acc + 1)
        posList' [] acc       = []

    -- Get the index of the first occurence of a formal parameter in the local environment
    posInd :: Expr -> Int -> [(Expr, Int)] -> Maybe Int
    posInd e num pos = case lookup e pos of
        Just ind -> Just (ind + num)
        _        -> Nothing
