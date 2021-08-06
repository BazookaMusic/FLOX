module AST

open Tokens

type Literal = NUMBER of double | STRING of string | TRUEVAL | FALSEVAL | NIL | IDENTIFIER of string
type BinaryOperator = EQ | NEQ | LESS | LESSEQ | GREATER | GREATEREQ | PLUS  | MINUS  | MULT | DIV | INVALID
type UnaryOperator = MINUS | BANG | INVALID

type Identifier = VarIdentifier of string

let TokenToBinaryOperator (token: TokenType)  = match token with
    | BANG_EQUAL -> NEQ
    | EQUAL_EQUAL -> EQ
    | TokenType.LESS -> BinaryOperator.LESS
    | TokenType.LESS_EQUAL -> LESSEQ
    | TokenType.GREATER -> BinaryOperator.GREATER
    | TokenType.GREATER_EQUAL -> BinaryOperator.GREATEREQ
    | TokenType.PLUS -> BinaryOperator.PLUS
    | TokenType.MINUS -> BinaryOperator.MINUS
    | TokenType.STAR -> BinaryOperator.MULT
    | TokenType.SLASH -> BinaryOperator.DIV
    | _ ->
        // this should never happen
        assert false
        BinaryOperator.INVALID

let TokenToUnaryOperator (token: TokenType)  = match token with
    | TokenType.BANG -> UnaryOperator.BANG
    | TokenType.MINUS -> UnaryOperator.MINUS
    | _ ->
        // this should never happen
        assert false
        UnaryOperator.INVALID

type Expression =
    | BinaryExpression of Expression * BinaryOperator * Expression
    | Grouping of Expression
    | Unary of UnaryOperator * Expression
    | Literal of Literal
    // no source should be valid
    | Empty
    | Invalid

let LiteralToString (literal: Literal): string = match literal with
    | NUMBER x -> x.ToString()
    | STRING x -> x
    | TRUEVAL -> "true"
    | FALSEVAL -> "false"
    | NIL -> "nil"

let BinaryOperatorToString (op:BinaryOperator) = match op with
    | EQ -> "=="
    | NEQ -> "!="
    | LESS -> "<"
    | LESSEQ -> "<="
    | GREATER -> ">"
    | GREATEREQ -> ">="
    | PLUS -> "+"
    | BinaryOperator.MINUS -> "-"
    | MULT -> "*"
    | DIV -> "/"

let UnaryOperatorToString (op: UnaryOperator) = match op with
    | MINUS -> "-"
    | BANG -> "!"

// Statements

type Statement = 
    | ExpressionStatement of Expression
    | PrintStatement of Expression

type Declaration = 
    | StatementDeclaration of Statement
    | VariableDeclaration of Identifier * Option<Expression>