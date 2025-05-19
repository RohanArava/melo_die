# Defining the grammar

The goal is to define a simple, straight-forward grammar. Which is easy to implement and mostly free of syntax sugars.

Features to include:
- Initially only needs to support integers
- Supports functions
- Supports if-else, for and while loops.
- Supports unary and binary expressions.
- Supports variable assignment and declaration as statements

Example: (Todo: Write a better example)
```
function name(arg1, arg2, arg3){
    let sum = 0
    for let i = arg1; i <= arg2; i=i+1 {
        sum = sum + arg3
    }
    if sum > 0 {
        return sum
    } else {
        let i = 0
        sum = 0
        while i < arg2 {
            sum = sum + arg3
        }
        return sum
    }
}
```

This is trying to make whitespace and newline insignificant

`function name(arg1, arg2, arg3){ let sum = 0 for let i = arg1; i <= arg2; i=i+1 { sum = sum + arg3 } if sum > 0 { return sum } else { let i = 0 sum = 0 while i < arg2 { sum = sum + arg3 } return sum }}`

Trying to define the grammar in BNF:

```
<PROGRAM> ::= <STATEMENT>*

<STATEMENT> ::= <VARIABLE_DECLARATION>
              | <ASSIGNMENT>
              | <IF_STATEMENT>
              | <WHILE_STATEMENT>
              | <FOR_STATEMENT>
              | <FUNCTION_DECLARATION>
              | <RETURN_STATEMENT>
              | <EXPRESSION>

<VARIABLE_DECLARATION> ::= "let" <IDENTIFIER> "=" <EXPRESSION>

<ASSIGNMENT> ::= <IDENTIFIER> "=" <EXPRESSION>

<IF_STATEMENT> ::= "if" <EXPRESSION> "{" <STATEMENT>* "}" <ELSE_CLAUSE>?

<ELSE_CLAUSE> ::= "else" "{" <STATEMENT>* "}"

<WHILE_STATEMENT> ::= "while" <EXPRESSION> "{" <STATEMENT>* "}"

<FOR_STATEMENT> ::= "for" <VARIABLE_DECLARATION> ";" <EXPRESSION> ";" <ASSIGNMENT> "{" <STATEMENT>* "}"

<FUNCTION_DECLARATION> ::= "function" <IDENTIFIER> "(" <PARAM_LIST>? ")" "{" <STATEMENT>* "}"

<PARAM_LIST> ::= <IDENTIFIER> ("," <IDENTIFIER>)*

<RETURN_STATEMENT> ::= "return" <EXPRESSION>

<EXPRESSION> ::= <CONSTANT>
              | <IDENTIFIER>
              | <FUNCTION_CALL>
              | <UNARY_OPERATOR> <EXPRESSION>
              | <EXPRESSION> <BINARY_OPERATOR> <EXPRESSION>
              | "(" <EXPRESSION> ")"
              | <TERNARY_EXPR>

<TERNARY_EXPR> ::= <EXPRESSION> "?" <EXPRESSION> ":" <EXPRESSION>

<FUNCTION_CALL> ::= <IDENTIFIER> "(" <ARG_LIST>? ")"

<ARG_LIST> ::= <EXPRESSION> ("," <EXPRESSION>)*

<UNARY_OPERATOR> ::= "-" | "!"

<BINARY_OPERATOR> ::= "+" | "-" | "*" | "/" | "%" 
                    | "==" | "!=" | "<" | "<=" | ">" | ">=" 
                    | "&&" | "||"

<IDENTIFIER> ::= <LETTER> (<LETTER> | <DIGIT> | "_")*

<CONSTANT> ::= <DIGIT>+

<LETTER> ::= "a" | ... | "z" | "A" | ... | "Z" | "_"

<DIGIT> ::= "0" | "1" | ... | "9"

```