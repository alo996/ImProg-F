module Parser where

  import Tokenizer_is
  import Declarations
  import Data.List
  import Data.Char
  
{-
Grammar
Programm ::= Definition ";" { Definition ";"} .
Definition ::= Variable {Variable} "=" Ausdruck .
Lokaldefinitionen ::= Lokaldefinition { ";" Lokaldefinition } .
Lokaldefinition ::= Variable "=" Ausdruck .
Ausdruck ::= "let" Lokaldefinitionen "in" Ausdruck
 | "if" Ausdruck "then" Ausdruck "else" Ausdruck
 | Ausdruck BinärOp Ausdruck
 | UnärOp Ausdruck
 | Ausdruck Ausdruck
 | "(" Ausdruck ")"
 | AtomarerAusdruck .
BinärOp ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
UnärOp ::= "not" | "−" .
AtomarerAusdruck ::= Variable | Zahl | Wahrheitswert .

Program ::= Definiton ";" {Definition ";"}
Definition ::= Variable {Variable} "=" Expression
LocalDefinitions ::= LocalDefiniton {";" LocalDefinition}
LocalDefinition ::= Variable "=" Expression
Expression ::= "let" LocalDefinitions "in" Expression
| "if" Expression "then" Expression "else" Expression
| Expression1
Expression1 ::= Expression2 ["|" Expression1 ]
Expression2 ::= Expression3 ["&" Expression2 ]
Expression3 ::= ["not"] Expression4
Expression4 ::= Expression5 [ComparisonOperator Expression5]
Expression5 ::= Expression6 RestExpression5
RestExpression5 ::= {"+" Expression6 } | "-" Expression6
Expression6 ::= ["-"] Expression7
Expression7 ::= Expression8 RestExpression7
RestExpression7 ::= {"*" Expression8 } | "/" Expression8
Expression8 ::= AtomicExpression {AtomicExpression}
AtomicExpression ::= Variable | Literal | "(" Expression ")"
ComparisonOperator ::= "==" | "<"
Variable ::= Name

data Expression = Expression String | Name String | BoolLit Bool | NumLit Int | Mult atomExpression atomExpression 
-}

  type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

  data Expression 
          = Add Expression Expression 
          | Mult Expression Expression
          | Div Expression Expression
          | BinaryMin Expression Expression
          | UnaryMin Expression
          | Equal Expression Expression
          | LessThan Expression Expression
          | LogicalAnd Expression Expression
          | LogicalOr Expression Expression
          | LogicalNot Expression
          | LetIn LocalDefinitions Expression
          | IfThenElse Expression Expression Expression
          | AtomicExpr AtomicExpression

  data LocalDefinition = Assignment Variable Expression
  type LocalDefinitions = [LocalDefinition]

  type Variable = String
  data AtomicExpression = Var Variable | LitBool BoolF | LitNum Int | Expr Expression


  variable :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  variable ((NameToken name, lc) : xs) = Right ([AtomicExpr (Var name)], xs)
  variable ((x , lc) : xs)             = Left $ "Parse error on input " ++ show x ++ " in line " ++ show lc
  variable []                          = Left "Parse error"
  
  atomicExpr :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  atomicExpr a@((NameToken x, lc) : xs)                  = variable a
  atomicExpr ((BooleanToken b@(BoolF x), lc) : xs)       = Right ([AtomicExpr (LitBool (BoolF x))], xs)
  atomicExpr ((NumberToken x, lc) : xs)                  = Right ([AtomicExpr (LitNum x)], xs)
  atomicExpr ((KeywordToken LBracket, lc) : xs)          = do 
    (e, ys) <- expr xs
    case ys of
      ((KeywordToken RBracket, lc) : zs) -> return (e, ys)
      _                                  -> Left "Parse error"
  atomicExpr ((_, lc): xs)                               = Left $ "Parse error in line " ++ show lc
  atomicExpr ts                                          = return ([], ts)

  expr8 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr8 xs = do
    (e, ys) <- atomicExpr xs
    (es, zs) <- expr8 ys
    return (e ++ es, zs)

  restExpr7 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  restExpr7 ((KeywordToken Times, _) : xs)  = do
    (e, ys) <- expr8 xs
    (es, zs) <- restExpr7 ys
    return (e ++ es, zs)
  restExpr7 ((KeywordToken Divide, _) : xs) = do
    (e, ys) <- expr8 xs
    return (e, ys)
  restExpr7 ts                              = return ([], ts)

  expr7 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr7 xs = do
    (e, ys) <- expr8 xs
    (es, zs) <- restExpr7 ys
    return (e ++ es, zs)

  expr6 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr6 ((KeywordToken Minus, _) : xs) = do
    (e, ys) <- expr7 xs
    return (e, ys)
  expr6 ts                             = expr7 ts

  restExpr5 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  restExpr5 ((KeywordToken Plus, _) : xs)  = do
    (e, ys) <- expr6 xs
    (es, zs) <- restExpr5 ys
    return (e ++ es, zs)
  restExpr5 ((KeywordToken Minus, _) : xs) = expr6 xs
  restExpr5 ts                             = return ([], ts)

  expr5 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr5 xs = do
    (e, ys) <- expr6 xs
    (es, zs) <- restExpr5 ys
    return (e ++ es, zs)

  expr4 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr4 xs = do
    (e, ys) <- expr5 xs
    case ys of 
      ((KeywordToken Equals, lc) : zs) -> do
        (es, as) <- expr5 zs
        return (e ++ es, as)
      ((KeywordToken Less, lc) : zs) -> do
        (es, as) <- expr5 zs
        return (e ++ es, as)
      _ -> return (e, ys)

  expr3 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr3 ((KeywordToken Not, _) : xs) = expr4 xs
  expr3 ((_, _) : xs)                = expr4 xs

  expr2 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr2 xs = do
    (e, ys) <- expr3 xs
    case ys of 
      ((KeywordToken And, lc) : zs) -> do
        (es, as) <- expr2 zs
        return (e ++ es, as)
      _ -> return (e, ys)

  expr1 :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr1 xs = do
    (e, ys) <- expr2 xs
    case ys of 
      ((KeywordToken Or, lc) : zs) -> do
        (es, as) <- expr1 zs
        return (e ++ es, as)
      _ -> return (e, ys)

  expr :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  expr ((KeywordToken Let, lc) : xs) = do
    (e, ys) <- localDefs xs
    case ys of
      ((KeywordToken In, lc) : zs) -> expr zs
      _                            -> Left "Parse error"
  expr ((KeywordToken If, lc) : xs) = do
    (e, ys) <- expr xs
    case ys of
      ((KeywordToken Then, lc) : zs) -> do
        (f, as) <- expr zs
        case as of 
          ((KeywordToken Else, lc) : as) -> expr as
          _                              -> Left "Parse error"
      _                              -> Left "Parse error"
  expr xs                           = expr1 xs

  localDef :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  localDef ((NameToken name, lc) : xs) = do
    (e, ys) <- variable [(NameToken name, lc)]
    case ys of
      ((KeywordToken Assign, lc) : zs) -> expr zs
      _                                -> Left "Parse error"
  localDef ts                          = return ([], ts)


  localDefs :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  localDefs xs = do
    (e, ys) <- localDef xs
    case ys of 
      ((KeywordToken Semicolon, lc) : zs) -> localDefs zs
      _                                   -> return (e, ys)

  def :: [(Token, Int)] -> Either String ([Expression], [(Token, Int)])
  def xs = do
    (e, ys) <- variable xs
    case ys of
      ((NameToken name1, lc) : zs) -> def zs
      ((KeywordToken Assign, lc) : zs) -> expr zs
      _                                -> Left "Parse error"

  program :: [(Token, Int)] -> Either String (Expression, [(Token, Int)])
  program xs = do
    (e, ys) <- def xs
    case ys of
      ((KeywordToken Semicolon, lc) : zs) -> program zs
      _                                   -> Left "Parse error"