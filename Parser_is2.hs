module Parser_is2 where

  import Tokenizer_is
  import Declarations
  import Data.List
  import Data.Char
  import Debug.Trace

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

Program ::= Definiton ";" {Definition ";"} - every definition forms a tree kind of; [tree]
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
AtomicExpression ::= Variable | Literal | "(" Expression ")" - for example function application like main = f 1
ComparisonOperator ::= "==" | "<"
Variable ::= Name

data Expression = Expression String | Name String | BoolLit Bool | NumLit Int | Mult atomExpression atomExpression
-}

--Parser errors should be dropped when we expect something and it is not there

  type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

  --data Definition = Asd

  data Expression
          = Add Expression Expression
          | Func Expression Expression -- Variable Expression Expression -- sure with "Variable?" perhaps rather AtomicExpression + Expression? (changed for test purposes)
          | Mult Expression Expression
          | Div Expression -- ?perhaps only one?
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
       --   | Assignment Variable Expression
          deriving Show

  data LocalDefinition = Assignment Expression Expression deriving Show --no idea how to implement if "Variable" instead of "Expression"
  type LocalDefinitions = [LocalDefinition]

  data DefTree a = L | Knt a (DefTree a) (DefTree a) deriving Show
  type Deftrees = [DefTree Expression] --müsste wahrscheinlich irgendwie mit LocalDefinitions zusammenarbeiten auch

  type Variable = String
  data AtomicExpression = Var Variable | LitBool BoolF | LitNum Int | Expr Expression deriving Show

  --program (list of defs) & def as tree - definitions are missing


  variable :: Parser Expression
  variable ((NameToken name, lc) : xs) = Right (AtomicExpr (Var name), xs)
  variable ((x , lc) : xs)             = Left $ "Parse error on input in variable" ++ show x ++ " in line " ++ show lc
  variable []                          = Left "Parse error variable"

  atomicExpr :: Parser Expression
  atomicExpr a@((NameToken x, lc) : xs)                  = variable a
  atomicExpr ((BooleanToken b@(BoolF x), lc) : xs)       = Right (AtomicExpr (LitBool (BoolF x)), xs)
  atomicExpr ((NumberToken x, lc) : xs)                  = trace "atomicExpr Right on NumberToken" (Right (AtomicExpr (LitNum x), xs))
  atomicExpr ((KeywordToken LBracket, lc) : xs)          = do
    (e, ys) <- expr xs
    case ys of
      ((KeywordToken RBracket, lc) : zs) -> return (e, ys)
      _                                  -> Left "Parse error in atomicexpr - no right bracket detected"
  --atomicExpr []                                          = return ([], [])
  --atomicExpr ts                                          = return ([], ts)

  expr8 :: Parser Expression
  expr8 xs = do
    (e, ys) <- trace ("expr8, calling atomicExpr with input " ++ show xs) (atomicExpr xs)
    (es, ys1) <- restexpr8 ys
    return (foldl Func e es, ys1)

  restexpr8 :: Parser [Expression]
  restexpr8 a@((NameToken x, lc) : xs)           = do
    (e, ys) <- trace ("expr8, calling atomicExpr (NameToken) with input " ++ show xs) (atomicExpr a)
    (es, ys1) <- restexpr8 ys
    return (e:es, ys1)
  restexpr8 a@((BooleanToken x, lc) : xs)        = do
    (e, ys) <- trace ("expr8, calling atomicExpr (BooleanToken) with input " ++ show xs) (atomicExpr a)
    (es, ys1) <- restexpr8 ys
    return (e:es, ys1)
  restexpr8 a@((NumberToken x, lc) : xs)         = do
    (e, ys) <- trace ("expr8, calling atomicExpr (NumberToken) with input " ++ show xs) (atomicExpr a)
    (es, ys1) <- restexpr8 ys
    return (e:es, ys1)
  restexpr8 a@((KeywordToken LBracket, lc) : xs) = do
    (e, ys) <- trace ("expr8, calling atomicExpr (LBracket) with input " ++ show xs) (atomicExpr a)
    (es, ys1) <- restexpr8 ys
    return (e:es, ys1)
  restexpr8 xs                                   = return ([], xs)

  expr7 :: Parser Expression
  expr7 xs  = do
    (e, ys) <- trace ("expr7, calling expr8 with input " ++ show xs) (expr8 xs)
    (es, zs) <- trace ("expr7, calling restExpr7 with input " ++ show ys) (restExpr7 ys)
    --case es of -- potentially we do not need a case for Divide for it is an unary operation and will just be folded over?
    --    ((Div _ _) : xs) -> return (foldl es, zs)
    return (foldl Mult e es, zs)

  restExpr7 :: Parser [Expression]
  restExpr7 ((KeywordToken Times, _) : xs)  = do
    (e, ys) <- trace ("restExpr7mult, calling expr8 with input " ++ show xs) (expr8 xs)
    (es, zs) <- trace ("restExpr7mult, calling restExpr7 with input " ++ show ys) (restExpr7 ys)
    return (e:es, zs)
  restExpr7 ((KeywordToken Divide, _) : xs) = do
    (e, ys) <- trace ("restExpr7div, calling expr8 with input " ++ show xs) (expr8 xs)
    return ([Div e], ys)
  restExpr7 ts                              = return ([], ts)

  expr6 :: Parser Expression
  expr6 ((KeywordToken Minus, _) : xs) = do
    (e, ys) <- trace ("expr6, calling expr7 with input " ++ show xs) (expr7 xs)
    return (UnaryMin e, ys)
  expr6 ts                             = trace ("expr6, calling expr7 with input " ++ show ts) (expr7 ts)

  expr5 :: Parser Expression
  expr5 xs = do
    (e, ys) <- trace ("expr5, calling expr6 with input " ++ show xs) (expr6 xs)
    (es, zs) <- trace ("expr5, calling restExpr5 with input " ++ show ys) (restExpr5 ys)
    return (foldl Add e es, zs)

  restExpr5 :: Parser [Expression]
  restExpr5 ((KeywordToken Plus, _) : xs)  = do
    (e, ys) <- trace ("restExpr5, calling expr6 with input " ++ show xs) (expr6 xs)
    (es, zs) <- trace ("restExpr5, calling restExpr5 with input " ++ show ys) (restExpr5 ys)
    return (e:es, zs)
  restExpr5 ((KeywordToken Minus, _) : xs) = do
    (e, ys) <- trace ("restExpr5, calling expr6 with input " ++ show xs) (expr6 xs)
    return ([UnaryMin e], ys)
  restExpr5 ts                             = return ([], ts)

  expr4 :: Parser Expression
  expr4 xs = do
    (e, ys) <- trace ("expr4, calling expr5 with input " ++ show xs) (expr5 xs)
    case ys of
      ((KeywordToken Equals, lc) : zs) -> do
        (es, as) <- trace ("expr4, calling expr5 with input " ++ show zs) (expr5 zs)
        return (Equal e es, as)
      ((KeywordToken Less, lc) : zs) -> do
        (es, as) <- trace ("expr4, calling expr5 with input " ++ show zs) (expr5 zs)
        return (LessThan e es, as)
      _ -> return (e, ys) -- needed?

  expr3 :: Parser Expression
  expr3 ((KeywordToken Not, _) : xs) = do
      (e, es) <- trace ("expr3, calling expr4 with input " ++ show xs) (expr4 xs)
      return (LogicalNot e, es)
  expr3 a@((_, _) : xs)              = trace ("expr3, calling expr4 with input " ++ show a) (expr4 a)

  expr2 :: Parser Expression
  expr2 xs = do
    (e, ys) <- trace ("expr2, calling expr3 with input " ++ show xs) (expr3 xs)
    case ys of
      ((KeywordToken And, lc) : zs) -> do
        (es, as) <- trace ("expr2, calling expr2 with input " ++ show zs) (expr2 zs)
        return (LogicalAnd e es, as)
      _ -> return (e, ys)

  expr1 :: Parser Expression
  expr1 xs = do
    (e, ys) <- trace ("expr1, calling expr2 with input " ++ show xs) (expr2 xs)
    case ys of
      ((KeywordToken Or, lc) : zs) -> do
        (es, as) <- trace ("expr1, calling expr1 with input " ++ show zs) (expr1 zs)
        return (LogicalOr e es, as)
      _ -> return (e, ys)

  expr :: Parser Expression --missing Datentypzuweisung
  expr ((KeywordToken Let, lc) : xs) = do
    (e, ys) <- trace ("expr, calling localDefs with input " ++ show xs) (localDefs xs) --localDefs liefert Liste von Definitions
    case ys of
      ((KeywordToken In, lc) : zs) -> do
        (es, as) <- trace ("expr, calling expr with input " ++ show zs) (expr zs)
        return (LetIn e es, as)
      _                            -> Left "Parse error of expr"

  expr ((KeywordToken If, lc) : xs) = do
    (e, ys) <- trace ("expr, calling expr with input " ++ show xs) (expr xs)
    case ys of
      ((KeywordToken Then, lc) : zs) -> do
        (f, as) <- trace ("expr, calling expr with input " ++ show zs) (expr zs)
        case as of
          ((KeywordToken Else, lc) : as) -> expr as
          _                              -> Left "Parse error of expr"
      _                              -> Left "Parse error of expr"
  expr xs                           = expr1 xs

  --liefert LocalDefinition
  localDef :: Parser LocalDefinition
  localDef a@((NameToken name, lc) : xs) = do
    (e, ys) <- trace ("localDef, calling variable with input " ++ show [(NameToken name, lc)]) (variable a)
    case ys of
      ((KeywordToken Assign, lc) : zs) -> do
        (e1, y1) <- trace ("localDef, calling expr with input " ++ show zs) (expr zs)
        return (Assignment e e1, y1)
      _                                -> Left "Parse error of localDef"
  --localDef ts                          = expr ts --need to think what to do here

--liefert Liste von LocalDefinition
  localDefs :: Parser LocalDefinitions
  localDefs xs = do
    (e, ys) <- trace ("localDefs, calling localDef with input " ++ show xs) (localDef xs)
    case ys of
      ((KeywordToken Semicolon, lc) : zs) -> trace ("localDefs, calling localDefs with input " ++ show zs) (localDefs zs)
      _                                   -> return ([e], ys)

  -- Definition ::= Variable {Variable} "=" Expression;
  --need to add definition-type; should return a tree
  def :: Parser [Expression]
  def a@((NameToken name, lc) : xs) = do
    (e, ys) <- variable a
    (e1, ys1) <- def xs
    return (e:e1, ys1)
  def ((KeywordToken Assign, lc) : ys2) = do
    (e2, ys2) <- expr ys2
    return ([e2], ys2)

--need to add program type; return should be list of trees
  program :: Parser [Expression]
  program xs = do
    (e, ys) <- trace ("program, calling def with input " ++ show xs) (def xs)
    case ys of
      ((KeywordToken Semicolon, lc) : zs) -> trace ("program, program with input " ++ show zs) (program zs)--dieser Fall kann nicht auftreten aktuell, da davor schon ein Parser error kommen würde
      []                                  -> Right (e, ys)
      _                                   -> Left "Parse error of program"