Grammatik Vorschlag:

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
Expression4 ::= Epression5 | "(" Expression5 ComparisonOperator Expression5 ")" | ComparisonOperator Expression5 ")"
Expression5 ::= Expression6 RestExpression5
RestExpression5 ::= {"+" Expression6 } | "(" "-" Expression6 | "-" Expression6 | "-" Expression6 ")"
Expression6 ::= Expression 7| "(" "-" Expression7 | "-" Expression7 | "-" Expression7 ")"
Expression7 ::= Expression8 RestExpression7
RestExpression7 ::= {"*" Expression8 } | "/" Expression8 | "/" Expression8 ")"
Expression8 ::= AtomicExpression {AtomicExpression}
AtomicExpression ::= Variable | Literal | "(" Expression ")"
