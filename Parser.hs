-- This module contains all functionality to pursue syntactical analysis in F.
module Parser where
  import Tokenizer
  import Declarations
  import Debug.Trace

  program :: Parser Prog
  program d@((NameToken _, _) : ts) = do
    (d, ts1) <- def d
    case ts1 of
      (KeywordToken Semicolon, _) : ts2 -> program ts2 >>= \ (Prog ds, ts3) -> return (Prog (d : ds), ts3)
      (token , line) : _                -> Left $ "Syntax error in line " ++ show line ++ ": Keyword ';' expected but found '" ++ show token ++ "'."
      []                                -> Left "Syntax error at end of program: Keyword ';' expected."
  program ((token, line) : _)       = Left $ "Syntax error in line " ++ show line ++ ": Identifier expected but found '" ++ show token ++ "'."
  program []                        = Right (Prog [], [])

  def :: Parser Def
  def ts = do
    (e, ts1) <- variable ts
    case ts1 of
      d@((NameToken _, _) : ts2)     -> def d >>= \ (Def es e1, ts3) -> return (Def (e : es) e1, ts3)
      (KeywordToken Assign, _) : ts2 -> expr ts2 >>= \ (e1, ts3) -> return (Def [e] e1, ts3)
      (token , line) : _             -> Left $ "Syntax error in line " ++ show line ++ ": Keyword '=' or expression expected but found '" ++ show token ++ "'."
      []                             -> Left "Syntax error at end of program: Keyword '=' or expression expected."
    
  localDefs :: Parser LocalDefs
  localDefs ts = do
    (d, ts1) <- localDef ts
    case ts1 of
      (KeywordToken Semicolon, _) : ts2 -> localDefs ts2 >>= \ (ds, ts3) -> return (d : ds, ts3)
      _                                 -> return ([d], ts1)
  
  localDef :: Parser LocalDef
  localDef ts = do
    (e, ts1) <- variable ts
    case ts1 of
      (KeywordToken Assign, _) : ts2 -> expr ts2 >>= \ (e1, ts3) -> return (LocalDef e e1, ts3)
      (token, line) : _              -> Left $ "Syntax error in line " ++ show line ++ ": Keyword '=' expected but found '" ++ show token ++ "'."
      []                             -> Left "Syntax error at end of program: Keyword '=' expected."

  expr, expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8, atomicExpr, variable :: Parser Expr
  expr ((KeywordToken Let, _) : ts) = do
    (e, ts1) <- localDefs ts
    case ts1 of
      (KeywordToken In, _) : ts2 -> expr ts2 >>= \ (e1, ts3) -> return (LetIn e e1, ts3)
      (token, line) : _          -> Left $ "Syntax error in line " ++ show line ++ ": Keyword 'in' expected but found '" ++ show token ++ "'."
      []                         -> Left "Syntax error at end of program: Keyword 'in' expected."
  expr ((KeywordToken If, _) : ts)  = do
    (e, ts1) <- expr ts
    case ts1 of
      (KeywordToken Then, _) : ts2 -> do
        (e1, ts3) <- expr ts2
        case ts3 of
          (KeywordToken Else, _) : ts3 -> expr ts3 >>= \ (e2, ts4) -> return (IfThenElse e e1 e2, ts4)
          (token, line) : _            -> Left $ "Syntax error in line " ++ show line ++ ": Keyword 'else' expected but found '" ++ show token ++ "'."
          []                           -> Left "Syntax error at end of program: Keyword 'else' expected."
      (token, line) : _            -> Left $ "Syntax error in line " ++ show line ++ ": Keyword 'then' expected but found '" ++ show token ++ "'."
      []                           -> Left "Syntax error at end of program: Keyword 'then' expected."
  expr ts                           = expr1 ts

  expr1 ts = do
    (e, ts1) <- expr2 ts
    case ts1 of
      (KeywordToken Or, _) : ts2 -> expr1 ts2 >>= \ (e1, ts3) -> return (LogicalOr e e1, ts3)
      _ -> return (e, ts1)

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
    
  atomicExpr n@((NameToken _, _) : _)             = variable n
  atomicExpr ((BooleanToken bool, _) : ts)        = Right (AtomicExpr (LitBool bool), ts)
  atomicExpr ((NumberToken num, _) : ts)          = Right (AtomicExpr (LitNum num), ts)
  atomicExpr ((KeywordToken LBracket, _) : ts) = do
    (e, ts1) <- expr ts
    case ts1 of
      (KeywordToken RBracket, _) : ts2 -> return (e, ts2)
      (token, line) : _                -> Left $ "Syntax error in line " ++ show line ++ ": Right bracket expected but found '" ++ show token ++ "'."
      _                                -> Left "Syntax error at end of program: Right bracket expected."
  atomicExpr ((token, line) : _)                     = Left $ "Syntax error in line " ++ show line ++ ": Expression expected but found '" ++ show token ++ "'."
  atomicExpr []                                      = Left "Syntax error at end of program: Expression expected."

  variable ((NameToken name, _) : ts) = Right (AtomicExpr (Var name), ts)
  variable ((token , line) : _)       = Left $ "Syntax error in line " ++ show line ++ ": Identifier expected but found '" ++ show token ++ "'."
  variable []                         = Left "Syntax error at end of program: Identifier expected."

  restExpr5, restExpr7, restExpr8 :: Parser [Expr]
  restExpr5 ((KeywordToken Plus, _) : ts)  = expr6 ts >>= \ (e, ts1) -> restExpr5 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  restExpr5 ((KeywordToken Minus, _) : ts) = expr6 ts >>= \ (e, ts1) -> return ([UnaryMin e], ts1)
  restExpr5 ts                             = return ([], ts)

  restExpr7 ((KeywordToken Times, _) : ts) = expr8 ts >>= \ (e, ts1) -> restExpr7 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
  restExpr7 ts                             = return ([], ts)

  restExpr8 ts = case ts of
    (NameToken _, _) : _           -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (BooleanToken _, _) : _        -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (NumberToken _, _) : _         -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    (KeywordToken LBracket, _) : _ -> atomicExpr ts >>= \ (e, ts1) -> restExpr8 ts1 >>= \ (es, ts2) -> return (e : es, ts2)
    _                              -> return ([], ts)
