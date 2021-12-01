module Parser where

  import Tokenizer
  import Declarations
  import Data.List
  import Data.Char

-- Grammar
-- Programm ::= Definition ";" { Definition ";"} .
--Definition ::= Variable {Variable} "=" Ausdruck .
--Lokaldefinitionen ::= Lokaldefinition { ";" Lokaldefinition } .
--Lokaldefinition ::= Variable "=" Ausdruck .
--Ausdruck ::= "let" Lokaldefinitionen "in" Ausdruck
-- | "if" Ausdruck "then" Ausdruck "else" Ausdruck
-- | Ausdruck BinärOp Ausdruck
-- | UnärOp Ausdruck
-- | Ausdruck Ausdruck
-- | "(" Ausdruck ")"
-- | AtomarerAusdruck .
--BinärOp ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
--UnärOp ::= "not" | "−" .
--AtomarerAusdruck ::= Variable | Zahl | Wahrheitswert .

{-
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

 data Expr = MulDiv AtomExpr Expr | UnOp KeywordToken Expression

-- data AtomicExpr = Var String | LitBool Bool | LitNum Int | Expr Expr 

-- [(NameToken "f",1),(NameToken "x",1),(KeywordToken =,1),(NumberToken 3,1)]

  variable, atomicExpression :: Parser Expression
  variable (NameToken x : xs) = (Right (Name x), xs)
  variable (_ : xs)           = (Left "we do not understand yet", xs)

  atomicExpression (NameToken x : xs)           = variable (NameToken x : xs)
  atomicExpression (BooleanToken x : xs)        = (Right (BoolLit x), xs)
  atomicExpression (NumberToken x : xs)         = (Right (NumLit x), xs)
  -- atomicExpression (KeywordToken LBracket : xs) = do 
  --            (e, ys) <- expression xs
  --            case ys of
  --                (KeywordToken RBracket : zs) -> (Right e, zs)
  --                (_ : zs)                     -> (Left "what is happening here hahah", zs)  
  atomicExpression  (_ : xs)                    = (Left "we do not understand yet", xs)

  expr8 :: Parser [Expression]
  expr8 xs = do
    (e, ys) <- atomicExpression xs
    (es, zs) <- expr8 ys
    return (e : es, zs)
    
  restExpr7 :: Parser [Expression]
  restExpr7 (KeywordToken Times : xs) = do
    (e, ys) <- atomicExpression xs
    (es, zs) <- restExpr ys
    return (e : es, zs)
  restExpr7 (KeywordToken Divide : xs) = undefined

  expr7 :: Parser [Expression]
  expr7 xs = do
    (e, ys) <- expr8 xs
    (es, zs) <- restExpr7 ys
    return (foldl MulDiv e es, zs)

  expr6 :: Parser Expression
  expr6 (KeywordToken Not : xs) = expr7 

  expression :: Parser Expression
  expression = undefined