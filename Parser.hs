
module Parser where

  import Tokenizer
  import Declarations
  import Data.List
  import Data.Char


--TODO-- 

-- In attomicExpr the expression between "(" ")" is not defined, both as a data and the function that calls it
-- we are not sure, that expression 8 needs to be strictly defined (per function/data) 
-- as in the EBNF in the ToyParser it is not defined that way 
-- by implementation: see that the parser does not take a numbertoken followed by name/keyword token. beispiel: let 32a = x + 2 


-- Grammer
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


  --newtype Parser a = P ([Token] -> (Either String a, [Token]))
  type Parser a = [Token] -> (Either String a, [Token])

  data Expression = Expression String | Name String | BoolLit Bool | NumLit Int

-- parse :: [(Token, Int)] -> SyntaxTree

-- [(NameToken "f",1),(NameToken "x",1),(KeywordToken =,1),(NumberToken 3,1)]

  variable, atomicExpression :: Parser Expression
  variable (NameToken x : xs) = (Right (Name x), xs)
  variable (_ : xs)           = (Left "we do not understand yet", xs)



  atomicExpression (NameToken x : xs)           = variable (NameToken x : xs)
  atomicExpression (BooleanToken x : xs)        = (Right (BoolLit x), xs)
  atomicExpression (NumberToken x : xs)         = (Right (NumLit x), xs)
  --atomicExpression (KeywordToken LBracket : xs) = do 
  --            (e, ys) <- expression xs
  --            case ys of
  --                (KeywordToken RBracket : zs) -> (Right e, zs)
  --                (_ : zs)                     -> (Left "what is happening here hahah", zs)  
  atomicExpression  (_ : xs)                = (Left "we do not understand yet", xs)


  expression :: Parser Expression
  expression = undefined

  restExpr7 :: Parser [Expression]
  restExpr7 = undefined
