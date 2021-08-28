module AST

open Tokens

[<Struct>]
type Literal = NUMBER of d:double | STRING of s: string | TRUEVAL | FALSEVAL | NIL | IDENTIFIER of id: string

[<Struct>]
type BinaryOperator = EQ | NEQ | LESS | LESSEQ | GREATER | GREATEREQ | PLUS  | MINUS  | MULT | DIV | INVALID | AND | OR

[<Struct>]
type UnaryOperator = MINUS | BANG | INVALID

[<Struct>]
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
    | TokenType.AND -> BinaryOperator.AND
    | TokenType.OR -> BinaryOperator.OR
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
    | Call of Expression * ArgumentList
    | Literal of Literal
    | Assign of Identifier * Expression
    // no source should be valid
    | Empty
    | Invalid

and
    ArgumentList = Expression list

let LiteralToString (literal: Literal): string = match literal with
    | NUMBER x -> x.ToString()
    | STRING x -> x
    | TRUEVAL -> "true"
    | FALSEVAL -> "false"
    | NIL -> "nil"
    | IDENTIFIER id -> id

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
    | IfStatement of Expression * Statement * Option<Statement>
    | WhileStatement of Expression * Statement
    | ForStatement of Option<Declaration> * Option<Statement> * Option<Statement> * Statement
    | Block of Declaration list
and
    Declaration = 
        | StatementDeclaration of Statement
        | VariableDeclaration of Identifier * Option<Expression>