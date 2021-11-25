
module Parser
    (parseInterface
    ) where

import Declarations_02
import Tokenizer_02
import Data.List
import Data.Char

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
--Variable ::= Name

type Parser a = [Token] -> Either (a, [SyntaxTree])

parse :: [(Token, Int)] -> SyntaxTree

variable :: Parser Expression
variable (Token x : _ ) = do
  case x of
    NameToken : _ -> return (Name x, _)
    _ -> Nothing
    variable _ = Nothing
