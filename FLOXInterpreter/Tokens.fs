module Tokens

type TokenType =
    |LEFT_PAREN
    |RIGHT_PAREN
    |LEFT_BRACE
    |RIGHT_BRACE
    |COMMA
    // Operators
    |DOT
    |MINUS
    |PLUS
    |SEMICOLON
    |SLASH
    |STAR
    |BANG
    |BANG_EQUAL
    |EQUAL
    |EQUAL_EQUAL
    |GREATER
    |GREATER_EQUAL
    |LESS
    |LESS_EQUAL
    // Data
    |STRING
    |NUMBER
    |TRUE
    |FALSE
    //  Reserved
    //  keywords
    |AND
    |CLASS
    |ELSE
    |FUN
    |FOR
    |IF
    |NIL
    |OR
    |PRINT
    |RETURN
    |SUPER
    |THIS
    |VAR
    |WHILE
    |IDENTIFIER
    // Helpers
    |EOF
    |UNKNOWN
    |IGNORE

type ScannerToken = {Type: TokenType; Lexeme: string; Literal: obj; Line: int}

let MatchSpecialIdentifiers lexeme line =
    match (lexeme) with
    | "false" -> FALSE
    | "true" -> TRUE
    | "and" -> AND
    | "class" -> CLASS
    | "else" -> ELSE
    | "fun" -> FUN
    | "for" -> FOR
    | "if" -> IF
    | "nil" -> NIL
    | "or" -> OR
    | "print" -> PRINT
    | "return" -> RETURN
    | "super" -> SUPER
    | "this" -> THIS
    | "var" -> VAR
    | "while" -> WHILE
    | _ -> UNKNOWN