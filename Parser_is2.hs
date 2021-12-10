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
-}

  {- TODO:

   1. Parser errors should be dropped when we expect something and it is not there
   2. Div - funktioniert, keine Ahnung warum
   3. Tests
   4. Klammern - funktioniert
   5. Präzedenzen (Assoziativ etc.)
   -}

  type Parser a = [(Token, Int)] -> Either String (a, [(Token, Int)])

  data Expr
          = Add Expr Expr
          | Func Expr Expr -- Variable Expression Expression -- sure with "Variable?" perhaps rather AtomicExpression + Expression? (changed for test purposes)
          | Mult Expr Expr
          | Div Expr Expr -- ?perhaps only one? // redone back to 2 x expr
          | UnaryMin Expr
          | Equal Expr Expr
          | LessThan Expr Expr
          | LogicalAnd Expr Expr
          | LogicalOr Expr Expr
          | LogicalNot Expr
          | LetIn LocalDefs Expr
          | IfThenElse Expr Expr Expr
          | AtomicExpr AtomicExpr
          deriving Show

  data LocalDef = LocalDef Expr Expr deriving Show
  type LocalDefs = [LocalDef]

  type Var = String
  data AtomicExpr = Var Var | LitBool BoolF | LitNum Int | Expr Expr deriving Show

  data Def = Def [Expr] Expr deriving Show
  newtype Prog = Prog [Def] deriving Show


  variable :: Parser Expr
  variable ((NameToken name, lc) : xs) = Right (AtomicExpr (Var name), xs)
  variable ((x , lc) : xs)             = Left $ "Parse error of varibale on input " ++ show x ++ " in line " ++ show lc ++ ": invalid syntax."
  variable []                          = Left "Parse error variable"  -- dieser Fall noch unklar

  atomicExpr :: Parser Expr
  atomicExpr a@((NameToken x, lc) : xs)                  = variable a
  atomicExpr ((BooleanToken b@(BoolF x), lc) : xs)       = Right (AtomicExpr (LitBool b), xs)
  atomicExpr ((NumberToken x, lc) : xs)                  = Right (AtomicExpr (LitNum x), xs)
  atomicExpr ((KeywordToken LBracket, lc) : xs)          = do
    (e, ys) <- expr xs
    case ys of
      ((KeywordToken RBracket, lc) : zs) -> return (e, zs)
      ((tk, lc) : zs)                    -> Left $ "Parse error of atomicExpr on input " ++ show tk ++ "in line " ++ show lc ++ ": right bracket expected."
      _                                  -> Left "atomicexpr, Parse error at end of program: right bracket expected."
  atomicExpr ((tk, lc) : xs)                             = Left $ "Parse error of atomicExpr on input " ++ show tk ++ " in line " ++ show lc ++ ": invalid syntax."
  atomicExpr []                                          = Left "Parse error (atomicExpr) at end of program: atomic expression expected."

  expr8 :: Parser Expr
  expr8 xs = do
    (e, ys) <- trace ("expr8, calling atomicExpr with input " ++ show xs) (atomicExpr xs)
    (es, ys1) <- restexpr8 ys
    return (foldl Func e es, ys1)

  restexpr8 :: Parser [Expr]
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

-- Expression7 ::= Expression8 RestExpression7
-- RestExpression7 ::= {"*" Expression8 } | "/" Expression8
  expr7 :: Parser Expr
  expr7 xs  = do
    (e, ys) <- trace ("expr7, calling expr8 with input " ++ show xs) (expr8 xs)
    case ys of
      ((KeywordToken Divide, lc) : zs) -> do
        (es, list) <- expr8 zs
        return (Div e es, list) 
      _  -> do
        (es, list) <- restExpr7 ys
        return (foldl Mult e es, list)
     -- ((tk, lc) : zs)                  -> Left $ "Parse error of expr7 on input " ++ show tk ++ " in line " ++ show lc ++ ": invalid syntax."
     -- _                                -> Left "Parse error (expr7) on end of program: invalid syntax."
    
  restExpr7 :: Parser [Expr]
  restExpr7 ((KeywordToken Times, _) : xs)  = do
    (e, ys) <- trace ("restExpr7mult, calling expr8 with input " ++ show xs) (expr8 xs)
    (es, zs) <- trace ("restExpr7mult, calling restExpr7 with input " ++ show ys) (restExpr7 ys)
    return (e : es, zs)
  restExpr7 ts = return ([], ts)

-- Expression6 ::= ["-"] Expression7
-- Expression7 ::= Expression8 RestExpression7
  expr6 :: Parser Expr
  expr6 ((KeywordToken Minus, _) : xs) = do
    (e, ys) <- trace ("expr6, calling expr7 with input " ++ show xs) (expr7 xs)
    return (UnaryMin e, ys)
  expr6 ts                             = trace ("expr6, calling expr7 with input " ++ show ts) (expr7 ts)

  expr5 :: Parser Expr
  expr5 xs = do
    (e, ys) <- trace ("expr5, calling expr6 with input " ++ show xs) (expr6 xs)
    (es, zs) <- trace ("expr5, calling restExpr5 with input " ++ show ys) (restExpr5 ys)
    return (foldl Add e es, zs)

  restExpr5 :: Parser [Expr]
  restExpr5 ((KeywordToken Plus, _) : xs)  = do
    (e, ys) <- trace ("restExpr5, calling expr6 with input " ++ show xs) (expr6 xs)
    (es, zs) <- trace ("restExpr5, calling restExpr5 with input " ++ show ys) (restExpr5 ys)
    return (e:es, zs)
  restExpr5 ((KeywordToken Minus, _) : xs) = do
    (e, ys) <- trace ("restExpr5, calling expr6 with input " ++ show xs) (expr6 xs)
    return ([UnaryMin e], ys)
  restExpr5 ts                             = return ([], ts)

  expr4 :: Parser Expr
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

  expr3 :: Parser Expr
  expr3 ((KeywordToken Not, _) : xs) = do
      (e, es) <- trace ("expr3, calling expr4 with input " ++ show xs) (expr4 xs)
      return (LogicalNot e, es)
  expr3 a@((_, _) : xs)              = trace ("expr3, calling expr4 with input " ++ show a) (expr4 a)

  expr2 :: Parser Expr
  expr2 xs = do
    (e, ys) <- trace ("expr2, calling expr3 with input " ++ show xs) (expr3 xs)
    case ys of
      ((KeywordToken And, lc) : zs) -> do
        (es, as) <- trace ("expr2, calling expr2 with input " ++ show zs) (expr2 zs)
        return (LogicalAnd e es, as)
      _ -> return (e, ys)

  expr1 :: Parser Expr
  expr1 xs = do
    (e, ys) <- trace ("expr1, calling expr2 with input " ++ show xs) (expr2 xs)
    case ys of
      ((KeywordToken Or, lc) : zs) -> do
        (es, as) <- trace ("expr1, calling expr1 with input " ++ show zs) (expr1 zs)
        return (LogicalOr e es, as)
      _ -> return (e, ys)

  expr :: Parser Expr --missing Datentypzuweisung
  expr ((KeywordToken Let, lc) : xs) = do
    (e, ys) <- trace ("expr, calling localDefs with input " ++ show xs) (localDefs xs) --localDefs liefert Liste von Definitions
    case ys of
      ((KeywordToken In, lc) : zs) -> do
        (es, as) <- trace ("expr (In), calling expr with input " ++ show zs) (expr zs)
        return (LetIn e es, as)
      _                            -> Left "Parse error of expr"

  expr ((KeywordToken If, lc) : xs) = do
    (e, ys) <- trace ("expr (If), calling expr with input " ++ show xs) (expr xs)
    case ys of
      ((KeywordToken Then, lc) : zs) -> do
        (f, as) <- trace ("expr (Then), calling expr with input " ++ show zs) (expr zs)
        case as of
          ((KeywordToken Else, lc) : as) -> do
            (g, bs) <- trace ("expr (Else), calling expr with input " ++ show as) expr as
            return (IfThenElse e f g, bs)
          _                              -> Left "Parse error of expr"
      _                              -> Left "Parse error of expr"
  expr xs                           = expr1 xs

  --liefert LocalDefinition
  localDef :: Parser LocalDef
  localDef xs = do
    (e, ys) <- trace ("localDef, calling variable with input " ++ show xs) (variable xs)
    case ys of
      ((KeywordToken Assign, lc) : zs) -> do
        (e1, y1) <- trace ("localDef, calling expr with input " ++ show zs) (expr zs)
        return (LocalDef e e1, y1)
      _                                -> Left "localDef, Parse error of localDef"

--liefert Liste von LocalDefinition
  localDefs :: Parser LocalDefs
  localDefs xs = do
    (e, ys) <- trace ("localDefs, calling localDef with input " ++ show xs) (localDef xs)
    case ys of
      ((KeywordToken Semicolon, lc) : zs) -> trace ("localDefs, calling localDefs with input " ++ show zs) (localDefs zs)
      _                                   -> return ([e], ys)

  -- data Def = Def [Expr] Expr deriving Show
  def :: Parser Def
  def xs = do
    (e1, ys) <- variable xs
    case ys of
      b@((NameToken name1, lc1) : ys1) -> do
        (Def es2 e2, ys2) <- def b
        return (Def (e1 : es2) e2, ys2)
      ((KeywordToken Assign, lc) : ys2) -> do
        (e2, ys3) <- expr ys2
        return (Def [e1] e2, ys3)
      ((tk , lc) : ys2)                 -> Left $ "def, Parse error on input " ++ show tk ++ " in line " ++ show lc ++ ": invalid syntax."
      _                                 -> Left "def, Parse error at end of program: invalid syntax."


  -- newtype Prog = Prog [Def] deriving Show
  program :: Parser Prog
  program a@((NameToken name1, lc1) : ys1) = do
    (def1, ys) <- trace ("program, calling def with input " ++ show a) (def a)
    case ys of
      ((KeywordToken Semicolon, lc) : zs) -> do
        (Prog defs2, ys1) <- trace ("program, program with input " ++ show zs) (program zs)
        return (Prog (def1 : defs2), ys1)
      ((tk , lc) : zs)                    -> Left $ "program, Parse error on input " ++ show tk ++ " in line " ++ show lc ++ ": invalid syntax."
      _                                   -> Left "program 1, Parse error at end of program: invalid syntax."
  program ((tk, lc) : ys1)             = Left $ "program 2, Parse error on input " ++ show tk ++ " in line " ++ show lc ++ ": invalid syntax."
  program [] = Right(Prog [], [])    
    
