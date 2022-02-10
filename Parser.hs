{- |
Module      : Parser
Description : This module contains all functionality to pursue syntactical analysis in F. It implements a recursive descent LL(1) parser, which specifies one function per rule in the F grammar. 
As defined in the Declarations file, a parser has type 'type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])'.
-}
module Parser where

import Declarations
    (AtomicExpr(Var, LitBool, LitNum),
    Expr(..),
    LocalDef(..),
    Def(..),
    Parser,
    BoolF(BoolF),
    Keyword(LBracket, Semicolon, Assign, Let, In, If, Then, Else, Or,
            And, Not, Equals, Less, Minus, Divide, RBracket, Plus, Times),
      Token(..))
import Tokenizer (tokenize)


{- | 'program' parses a program (a list of definitions). It takes a list of tuples, each tuple containing a token and its line number in the source code. If successful, it returns a tuple, containing a list of definitions and an empty list as the entire input was parsed. Otherwise it returns an error.
-}
program :: Parser [Def]
program [] = return ([], [])
program ts = do
  -- Parse one definition.
  (d, ts1) <- def ts
  -- If the next token is a keyword, recursively apply 'program' and concatenate the results.
  match (KeywordToken Semicolon) ts1 >>= \ (_, ts2) -> program ts2 >>= \ (ds, ts3) -> return (d : ds, ts3)

-- | 'def' parses one function definition.
def :: Parser Def
def ts = do
  (e, ts1) <- variable ts
  case ts1 of
    -- At least one formal parameter has been specified.
    d@((NameToken _, _) : ts2)     -> restDef d >>= \ (es, ts3) -> match (KeywordToken Assign) ts3 >>= \ (_, ts4) -> expr ts4 >>= \ (e1, ts5) -> return (Def e es e1, ts5) 
    -- The function has no formal parameters and the next token is '='. Therefore continues parsing the defining expression.
    (KeywordToken Assign, _) : ts2 -> expr ts2 >>= \ (e1, ts3) -> return (Def e [] e1, ts3) 
    -- The next token is neither a variable nor '='. This is syntactically incorrect, therefore returns an error. 
    (token, line) : _              -> Left $ "Syntax error in line " ++ show line ++ ": Keyword '=' or expression expected but found '" ++ toksToString token ++ "'."
    -- The tokenstream has suddenly ended, which is syntactically incorrect.
    []                             -> Left "Syntax error at end of program: Keyword '=' or expression expected."

-- | Parses a list of local definitions.
localDefs :: Parser [LocalDef]
localDefs ts = do
  (d, ts1) <- localDef ts
  case ts1 of
    (KeywordToken Semicolon, _) : ts2 -> localDefs ts2 >>= \ (ds, ts3) -> return (d : ds, ts3)
    _                                 -> return ([d], ts1)

-- | Parses exactly one local definition.
localDef :: Parser LocalDef
localDef ts = do
  (e, ts1) <- variable ts
  match (KeywordToken Assign) ts1 >>= \ (_, ts2) -> expr ts2 >>= \ (e1, ts3) -> return (LocalDef e e1, ts3)

-- | All expressions are of type '[(Token, Int)] -> Either String (Expr, [(Token, Int)])'.
expr, expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8, atomicExpr, variable :: Parser Expr
expr ((KeywordToken Let, _) : ts) = do
  (e, ts1) <- localDefs ts
  match (KeywordToken In) ts1 >>= \ (_, ts2) -> expr ts2 >>= \ (e1, ts3) -> return (LetIn e e1, ts3)
expr ((KeywordToken If, _) : ts)  = do
  (e, ts1) <- expr ts 
  (_ , ts2) <- match (KeywordToken Then) ts1
  (e1, ts3) <- expr ts2
  (_, ts4) <- match (KeywordToken Else) ts3
  (e2, ts5) <- expr ts4
  return (IfThenElse e e1 e2, ts5)
expr ts                           = expr1 ts

expr1 ts = do
  (e, ts1) <- expr2 ts
  case ts1 of
    (KeywordToken Or, _) : ts2 -> expr1 ts2 >>= \ (e1, ts3) -> return (LogicalOr e e1, ts3)
    _                          -> return (e, ts1)

expr2 ts = do
  (e, ts1) <- expr3 ts
  case ts1 of
    (KeywordToken And, _) : ts2 -> expr2 ts2 >>= \ (e1, ts3) -> return (LogicalAnd e e1, ts3)
    _                           -> return (e, ts1)

expr3 ((KeywordToken Not, _) : ts) = expr4 ts >>= \ (e, ts1) -> return (LogicalNot e, ts1)
expr3 ts                           = expr4 ts

expr4 ts = do
  (e, ts1) <- expr5 ts
  case ts1 of
    (KeywordToken Equals, _) : ts2 -> expr5 ts2 >>= \ (e1, ts3) -> return (Equal e e1, ts3)
    (KeywordToken Less, _) : ts2   -> expr5 ts2 >>= \ (e1, ts3) -> return (LessThan e e1, ts3)
    _                              -> return (e, ts1)

expr5 ts = do
  (e, ts1) <- expr6 ts
  case ts1 of
    (KeywordToken Minus, _) : ts2 -> expr6 ts2 >>= \ (e1, ts3) -> return (BinaryMin e e1, ts3)
    _                             -> restExpr5 ts1 >>= \ (es, ts3) -> return (foldl Add e es, ts3)  -- Addition is left associative.

expr6 ((KeywordToken Minus, _) : ts) = expr7 ts >>= \ (e, ts1) -> return (UnaryMin e, ts1)
expr6 ts                             = expr7 ts

expr7 ts  = do
  (e, ts1) <- expr8 ts
  case ts1 of
    (KeywordToken Divide, _) : ts2 -> expr8 ts2 >>= \ (e1, ts3) -> return (Div e e1, ts3)
    _                              -> restExpr7 ts1 >>= \ (es, ts3) -> return (foldl Mult e es, ts3)  -- Mulitplication is left associative.

expr8 ts = atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (foldl Func e es, ts2)  -- Function application is left associative.
  
atomicExpr n@((NameToken _, _) : _)          = variable n
atomicExpr ((BooleanToken bool, _) : ts)     = return (AtomicExpr (LitBool bool), ts)
atomicExpr ((NumberToken num, _) : ts)       = return (AtomicExpr (LitNum num), ts)
atomicExpr ((KeywordToken LBracket, _) : ts) = do
  (e, ts1) <- expr ts
  match (KeywordToken RBracket) ts1 >>= \ (_, ts2) -> return (e, ts2)
atomicExpr ((token, line) : _)               = Left $ "Syntax error in line " ++ show line ++ ": Expression expected but found '" ++ toksToString token ++ "'."
atomicExpr []                                = Left "Syntax error at end of program: Expression expected."

variable ((NameToken name, _) : ts) = return (AtomicExpr (Var name), ts)
variable ((token, line) : _)        = Left $ "Syntax error in line " ++ show line ++ ": Identifier expected but found '" ++ toksToString token ++ "'."
variable []                         = Left "Syntax error at end of program: Identifier expected."

-- | 'Rest' functions recursively iterate through the remaining tokenstream until their pattern is no longer matched.
restDef, restExpr5, restExpr7, restExpr8 :: Parser [Expr]
restDef ts = case ts of
  (NameToken _, _) : _ -> variable ts >>= \ (e, ts1) -> restDef ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  _                    -> return ([], ts)

restExpr5 ((KeywordToken Plus, _) : ts)  = expr6 ts >>= \ (e, ts1) -> restExpr5 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
restExpr5 ts                             = return ([], ts)

restExpr7 ((KeywordToken Times, _) : ts) = expr8 ts >>= \ (e, ts1) -> restExpr7 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
restExpr7 ts                             = return ([], ts)

restExpr8 ts = case ts of
  (NameToken _, _) : _           -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  (BooleanToken _, _) : _        -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  (NumberToken _, _) : _         -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  (KeywordToken LBracket, _) : _ -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  _                              -> return ([], ts)


---------------------------------------- HELPER FUNCTIONS FOR PARSER ----------------------------------------
-- | 'defsToString' is called when the user sets the '-ast' flag before execution. It prints the parsed function definitions.
defsToString :: [Def] -> String
defsToString defs = foldl (++) "+---------------+\n| Parser Output |\n+---------------+\n" (map (\ d -> show d ++ "\n") defs)

{- | 'match' checks whether a certain keyword is next in the remaining tokenstream. If so, this token is removed and the caller can operate on the remaining tokens. Otherwise it returns an error, indicating a syntactical error.
-}
match :: Token -> Parser ()  
match (KeywordToken key1) ((KeywordToken key2, line) : ts)
    -- If the tokenstream begins with the expected keyword, 'match' returns the remaining tokens for further calculations.
  | key1 == key2 = return ((), ts)
    -- If the keywords do not match, an error is returned.
  | otherwise    = Left $ "Syntax error in line " ++ show line ++ ": Keyword '" ++ show key1 ++ "' expected but found '" ++ show key2 ++ "'."
match t1 ((t2 , line) : _) = Left $ "Syntax error in line " ++ show line ++ ": Keyword '" ++ toksToString t1 ++ "' expected but found '" ++ toksToString t2 ++ "'."
match t1 []                = Left $ "Syntax error at end of program: Keyword '" ++ toksToString t1 ++ "' expected."

{- | 'toksToString' gives a compact string representation of tokens for error handling. The automatically derived 'show' function for tokens aims at correctly representing tokens in verbose output mode.
-}
toksToString :: Token -> String
toksToString (BooleanToken (BoolF bool)) = show bool
toksToString (KeywordToken kw)           = show kw
toksToString (NameToken name)            = show name
toksToString (NumberToken num)           = show num