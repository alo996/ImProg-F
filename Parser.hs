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
Expression4 ::= Expression5 [ComparisonOperator Expression5 ]
Expression5 ::= Expression6 RestExpression5
RestExpression5 ::= {"+" Expression6 } | "-" Expression6
Expression6 ::= ["-"] Expression7
Expression7 ::= Expression8 RestExpression7
RestExpression7 ::= {"*" Expression8 } | "/" Expression8
Expression8 ::= AtomicExpression {AtomicExpression}
AtomicExpression ::= Variable | Literal | "(" Expression ")"
ComparisonOperator ::= "==" | "<"
Variable ::= Name
-}

  type Parser a = [Token] -> (Either String a, [Token])

  --data Expression = Expression String | Name String | BoolLit Bool | NumLit Int | Mult atomExpression atomExpression 

  data Expression = MulDiv AtomicExpr AtomicExpr | UnOp Expression | AtomicExpr
  
  data AtomicExpr = Name String | LitBool Bool | LitNum Int deriving Show

  -- [(NameToken "f",1),(NameToken "x",1),(KeywordToken =,1),(NumberToken 3,1)]

  variable :: [(Token, Int)] -> (Either String AtomicExpr, [(Token, Int)])
  variable ((NameToken x, lc) : xs)       = (Right (Name x), xs)
  variable ((_, lc) : xs)                 = (Left ("Parse error in line " ++ show lc), xs)
  
  atomicExpression :: [(Token, Int)] -> (Either String AtomicExpr, [(Token, Int)])
  atomicExpression a@((NameToken x, lc) : xs)        = variable a
  atomicExpression ((BooleanToken x : xs), lc)       = (Right (BoolLit x), xs)
  atomicExpression ((NumberToken x : xs), lc)        = (Right (NumLit x), xs)
  -- atomicExpression (KeywordToken LBracket : xs) = do 
  --            (e, ys) <- expression xs
  --            case ys of
  --                (KeywordToken RBracket : zs) -> (Right e, zs)
  --                (_ : zs)                     -> (Left "what is happening here hahah", zs)  
  atomicExpression  ((_, lc): xs)                    = (Left ("Parse error in line " ++ show lc), xs)

{-

  expr8 :: [Token] -> (Either String [Expression], [Token])
  expr8 xs = do
    (e, ys) <- atomicExpression xs
    (es, zs) <- expr8 ys
    return (e : es, zs)
    
  restExpr7 :: [Token] -> (Either String [Expression], [Token])
  restExpr7 (KeywordToken Times : xs) = do
    (e, ys) <- expr8 xs
    (es, zs) <- restExpr7 ys
    return (e : es, zs)
  restExpr7 (KeywordToken Divide : xs) = do
    (e, ys) <- expr8 xs
    return (e, ys)

  expr7 :: [Token] -> (Either String Expression, [Token])
  expr7 xs = do
    (e, ys) <- expr8 xs
    (es, zs) <- restExpr7 ys
    return (foldl MulDiv e es, zs)
    --maybe to solve the MulDiv with IF THEN ElSE

  expr6 :: [Token] -> (Either String Expression, [Token])
  expr6 (KeywordToken Minus : xs) = do
    (UnOp, ys) <- expr7 xs
    return (UnOp, ys)
  expr6 (_ : xs)                  = expr7 xs

  
-}
  



  --expression :: Parser Expression
  --expression = undefined