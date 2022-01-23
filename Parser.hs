-- This module contains all functionality to pursue syntactical analysis in F.
module Parser where
  import Tokenizer
  import Declarations

  {-
  The idea of the parser is the following: for each rule in the non-left recursive grammar, create a function that implements that rule.
  Each function returns our parametrized parser type (see Declarations file). Implementing a rule means the following: 
  If there is some structure in the rule, check whether that structure occurs in the tokenized input stream. 
  If the structure is violated, we return an error. If the structure is respected, evaluate the token stream as far as possible 
  with the given function. 
  The parser works with the principle of recursive descent: Each function calls some more special function 
  (for example expr calls expr1, expr1 calls expr2 etc.), until we reach the 'base-cases', like atomicExpr.
  The final return value is an abstract syntax tree, which we will work with in the upcoming steps.

  The most general function in our parser is program. It works as follows:
  The first token has to be a definition, which has to start with a variable according to our grammar. 
  That means it starts with a NameToken. So if we detect a NameToken, we apply the function def on the tokenstream. 
  After the definition function did its job (we do not need to care about its implementation here and can just assume it returns a definition), 
  we must find a semicolon, otherwise this wouldn't be a valid definition. 
  So we call the helper function match, which is defined and explained at the end of the module to deal with this fact.
  -}
  program :: Parser [Def]
  program [] = return ([], [])
  program ts = do
    (d, ts1) <- def ts
    match (KeywordToken Semicolon) ts1 >>= \ (_, ts2) -> program ts2 >>= \ (ds, ts3) -> return (d : ds, ts3)

  {-
  Let's see how def works. The corresponding rule in the grammar is 'Variable {Variable} "=" Expression'.
  So a valid definition consists of at least one variable, the '=' symbol and one expression. 
  We again assume that we have two functions, variable and expr, that correctly parse variables and expressions, 
  without knowing their concrete implementations at the moment.
  -}
  def :: Parser Def -- the return value is Either String (Def, [(Token, Int)])
  def ts = do -- as we have to do some calculations in the Either monad, we directly jump into a do block
    (e, ts1) <- variable ts -- as our parser only looks ahead one symbol, we apply the function variable on the tokenstream ts. It returns the tuple (e, ts1), with e being the parsed expression (a variable) and ts1 the remaining tokenstream.
    case ts1 of -- We have to check what the first symbol of ts1 is to continue with the correct calculation
      d@((NameToken _, _) : ts2)     -> restDef d >>= \ (es, ts3) -> match (KeywordToken Assign) ts3 >>= \ (_, ts4) -> expr ts4 >>= \ (e1, ts5) -> return (Def e es e1, ts5) 
      (KeywordToken Assign, _) : ts2 -> expr ts2 >>= \ (e1, ts3) -> return (Def e [] e1, ts3) -- We found the Keyword '='. That means we call expr on the remaining tokenstream, as our rule states that we need to find exactly one more expression.  
      (token , line) : _             -> Left $ "Syntax error in line " ++ show line ++ ": Keyword '=' or expression expected but found '" ++ show token ++ "'." -- We neither found a NameToken nor the Keyword '='. That means there is something wrong with the input and we tell the user what we would have expexcted instead.
      []                             -> Left "Syntax error at end of program: Keyword '=' or expression expected." -- The tokenstream has suddenly ended. This is also incorrect.
    
  localDefs :: Parser [LocalDef]
  localDefs ts = do
    (d, ts1) <- localDef ts
    case ts1 of
      (KeywordToken Semicolon, _) : ts2 -> localDefs ts2 >>= \ (ds, ts3) -> return (d : ds, ts3)
      _                                 -> return ([d], ts1)

  localDef :: Parser LocalDef
  localDef ts = do
    (e, ts1) <- variable ts
    match (KeywordToken Assign) ts1 >>= \ (_, ts2) -> expr ts2 >>= \ (e1, ts3) -> return (LocalDef e e1, ts3)

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

  expr5 ts = expr6 ts >>= \ (e, ts1) -> restExpr5 ts1 >>= \ (es, ts2) -> return (foldl Add e es, ts2)

  expr6 ((KeywordToken Minus, _) : ts) = expr7 ts >>= \ (e, ts1) -> return (UnaryMin e, ts1)
  expr6 ts                             = expr7 ts

  expr7 ts  = do
    (e, ts1) <- expr8 ts
    case ts1 of
      (KeywordToken Divide, _) : ts2 -> expr8 ts2 >>= \ (e1, ts3) -> return (Div e e1, ts3)
      _                              -> restExpr7 ts1 >>= \ (es, ts3) -> return (foldl Mult e es, ts3)

  expr8 ts = atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (foldl Func e es, ts2)
    
  atomicExpr n@((NameToken _, _) : _)          = variable n
  atomicExpr ((BooleanToken bool, _) : ts)     = return (AtomicExpr (LitBool bool), ts)
  atomicExpr ((NumberToken num, _) : ts)       = return (AtomicExpr (LitNum num), ts)
  atomicExpr ((KeywordToken LBracket, _) : ts) = do
    (e, ts1) <- expr ts
    match (KeywordToken RBracket) ts1 >>= \ (_, ts2) -> return (e, ts2)
  atomicExpr ((token, line) : _)               = Left $ "Syntax error in line " ++ show line ++ ": Expression expected but found '" ++ show token ++ "'."
  atomicExpr []                                = Left "Syntax error at end of program: Expression expected."

  variable ((NameToken name, _) : ts) = return (AtomicExpr (Var name), ts)
  variable ((token , line) : _)       = Left $ "Syntax error in line " ++ show line ++ ": Identifier expected but found '" ++ show token ++ "'."
  variable []                         = Left "Syntax error at end of program: Identifier expected."

  restDef, restExpr5, restExpr7, restExpr8 :: Parser [Expr]
  restDef ts = case ts of
    (NameToken _, _) : _ -> variable ts >>= \ (e, ts1) -> restDef ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    _                    -> return ([], ts)

  restExpr5 ((KeywordToken Plus, _) : ts)     = expr6 ts >>= \ (e, ts1) -> restExpr5 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  restExpr5 ((KeywordToken LBracket, _) : ts) = match (KeywordToken Minus) ts >>= \ (_, ts1) -> expr6 ts1 >>= \ (e1, ts2) -> match (KeywordToken RBracket) ts2 >>= \ (_, ts3) -> return ([UnaryMin e1], ts3)
  restExpr5 ts                                = return ([], ts)

  restExpr7 ((KeywordToken Times, _) : ts) = expr8 ts >>= \ (e, ts1) -> restExpr7 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  restExpr7 ts                             = return ([], ts)

  restExpr8 ts = case ts of
    (NameToken _, _) : _           -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (BooleanToken _, _) : _        -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (NumberToken _, _) : _         -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (KeywordToken LBracket, _) : _ -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    _                              -> return ([], ts)

  match :: Token -> Parser ()  
  -- match :: Token -> [(Token, Int)] -> Either String ((), [(Token, Int)]) is identical
  match (KeywordToken key1) ((KeywordToken key2, line) : ts)
    | key1 == key2 = return ((), ts) -- If the tokenstream begins with the expected keyword, we return the Right value to do further calculations.
    | otherwise    = Left $ "Syntax error in line " ++ show line ++ ": Keyword '" ++ show (KeywordToken key1) ++ "' expected but found '" ++ show (KeywordToken key2) ++ "'."
  match t1 ((t2 , line) : _) = Left $ "Syntax error in line " ++ show line ++ ": Keyword " ++ show t1 ++ " expected but found '" ++ show t2 ++ "'."
  match t1 []                = Left $ "Syntax error at end of program: Keyword '" ++ show t1 ++ "' expected."