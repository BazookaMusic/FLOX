﻿module Parser

open Scanner
open Errors
open System.Collections.Generic
open AST
open Tokens
    
let ExpectToken (tokenType: TokenType) (tokens: ScannerToken list) (errorMessage: string) : bool * ScannerToken list * Errors =
    match tokens with
    | (first::rest) when first.Type = tokenType -> (true, rest, NoError)
    | (first::rest) -> 
        let error = Err (first.Line, errorMessage)
        (false, rest, error)
    | [] ->
        let error = Err (-1, "Unexpected end of input.")
        (false, [], error)
    
let rec TreeToString (root:Expression): string =
    match root with
        | Literal lit -> LiteralToString (lit)
        | Unary (op, expr) ->  sprintf "( (%s) %s )" (UnaryOperatorToString op) (TreeToString expr)
        | BinaryExpression (left, op, right) -> sprintf "( %s (%s) %s)" (TreeToString left)  (BinaryOperatorToString op) (TreeToString right)
        | Grouping expr -> sprintf "( %s )"  (TreeToString expr)
        | Empty -> ""
        | Invalid -> "Invalid"


type ParseResult<'T> =
    | Ok of 'T * ScannerToken list
    | Error of Errors list * ScannerToken list

let ObjToNumeric (num:obj): double =
    match num with
    | :? System.Int32 -> double (num :?> int)
    | :? System.Double -> num :?> double
    | _ -> nan


let rec ParseMultipleOfSameType (tokens:ScannerToken list) (parseExpressionFn: (ScannerToken list -> ParseResult<Expression>)) (leftExpr: Expression) (tokenMatchFn: (ScannerToken -> bool)) (expressionTypeNameForError: string): ParseResult<Expression> =
    match (tokens: ScannerToken list) with
        // more than one token, check for operator and parse expression
        | (h::t) when (tokenMatchFn h) -> 
            let operator = TokenToBinaryOperator h.Type
            match parseExpressionFn t with
                | Ok (right, rightRest) ->
                    let parseRest = ParseMultipleOfSameType rightRest parseExpressionFn right tokenMatchFn expressionTypeNameForError
                    match parseRest with
                    | Ok (expr, rest) -> Ok(BinaryExpression (leftExpr, operator, expr), rest)
                    | error -> error
                | error -> error
        // first token is not an operator, stop
        | (h::t) when not t.IsEmpty -> Ok (leftExpr, tokens)
        // first token is an operator but no more tokens. Shouldn't happen
        | [h] when (tokenMatchFn h) -> 
          let error = Err (h.Line, sprintf "Expected %s expression after '%s'" expressionTypeNameForError h.Lexeme)
          Error ([error], [])
        // Just one token left, not an operator. Valid state when reaching EOF.
        | [h] ->
          Ok (leftExpr, tokens)
        // Should be impossible due to EOF
        | [] -> 
          let error = Err (-1, "This should never happen.")
          Error ([error], [])

let rec ParseUnary (tokens: ScannerToken list): ParseResult<Expression> = 
    match tokens with
    | (h::t) when (h.Type = TokenType.BANG || h.Type = TokenType.MINUS) ->
        let operator = h
        match (ParseUnary t) with
        | Ok (right, rest) -> Ok (Unary (TokenToUnaryOperator operator.Type, right), rest)
        | error -> error 
    | tokens -> ParsePrimary tokens
    
and ParsePrimary (tokens: ScannerToken list): ParseResult<Expression> =
    match tokens with
    | (h::t) ->
        match h.Type with
        | TokenType.NUMBER -> Ok (Literal (Literal.NUMBER (ObjToNumeric h.Literal)), t)
        | TokenType.STRING -> Ok (Literal (Literal.STRING (h.Literal :?> string)), t)
        | TokenType.TRUE -> Ok (Literal (Literal.TRUEVAL), t)
        | TokenType.FALSE -> Ok (Literal (Literal.FALSEVAL), t)
        | TokenType.NIL -> Ok (Literal (Literal.NIL), t)
        | TokenType.IDENTIFIER -> Ok (Literal (Literal.IDENTIFIER h.Lexeme), t)
        | TokenType.LEFT_PAREN ->
            match ParseExpression t with
                | Ok (expr, newTail) ->
                    match newTail with
                    | (nh::nt) when nh.Type = RIGHT_PAREN -> Ok (Grouping expr, nt)
                    | (nh::nt) -> 
                        let error = Err (nh.Line, sprintf "Expected ')' after expression but got '%s' instead." nh.Lexeme)
                        Error ([error], nt)
                    | _ -> 
                        let error = Err (0, "Unexpected end of input")
                        Error ([error], [])
                | error -> error
        | _ -> 
            let error = Err (h.Line, sprintf "Unexpected token while parsing literal '%s'" h.Lexeme)
            Error ([error], [])
    | [] -> 
        let error = Err (0, "Unexpected end of input")
        Error ([error], [])

and ParseFactor (tokens: ScannerToken list): ParseResult<Expression> = 
    let parseUnary = ParseUnary tokens
    match parseUnary with
    | Ok (expr, rest) -> 
        let factorTokens (token: ScannerToken) = match token.Type with
        | TokenType.STAR | TokenType.SLASH -> true
        | _ -> false

        ParseMultipleOfSameType rest ParseUnary expr  factorTokens "Factor"
    | _ -> parseUnary

and ParseTerm (tokens: ScannerToken list): ParseResult<Expression> =
    let parseFactor = ParseFactor tokens
    match parseFactor with
    | Ok (expr, rest) -> 
        let termTokens (token: ScannerToken) = match token.Type with
        | TokenType.MINUS | TokenType.PLUS -> true
        | _ -> false

        ParseMultipleOfSameType rest ParseFactor expr  termTokens "term"
    | _ -> parseFactor
    
and ParseComparison (tokens: ScannerToken list): ParseResult<Expression> = 
    let parseTerm = ParseTerm tokens
    match parseTerm with
    | Ok (expr, rest) -> 
        let comparisonTokens (token: ScannerToken) = match token.Type with
            | TokenType.LESS | TokenType.LESS_EQUAL | TokenType.GREATER | TokenType.GREATER_EQUAL -> true
            | _ -> false

        ParseMultipleOfSameType rest ParseTerm expr comparisonTokens "comparison"
    | _ -> parseTerm

and ParseEquality (tokens: ScannerToken list): ParseResult<Expression> = 
    let parseComp = ParseComparison tokens
    match parseComp with
    | Ok (expr, rest) -> ParseMultipleOfSameType rest ParseComparison expr  (fun token -> token.Type = BANG_EQUAL || token.Type = EQUAL_EQUAL) "equality"
    | Error (errors, rest) -> parseComp

and ParseExpression (tokens: ScannerToken list): ParseResult<Expression> =
    ParseEquality tokens


let ParseStatement (tokens: ScannerToken list) (statementConstructor: Expression->Statement): ParseResult<Statement> = 
    match ParseExpression tokens with
    | Ok (expression, rest) ->
        match rest with
        | (h::t) when h.Type = SEMICOLON ->
            Ok (statementConstructor expression, t)
        | (h::t) ->
            let error = Err (h.Line, "Expected ';' after expression.")
            Error ([error], t)
        | [] -> 
            let error = Err (-1, "This should never happen.")
            Error ([error], [])
    | Error (e, rest) as error ->
        // implicit conversion to Error<Statement> from Error<Expression>
        Error (e, rest)

let ParseExpressionStatement (tokens: ScannerToken list): ParseResult<Statement> =
    ParseStatement tokens (fun expr -> ExpressionStatement expr)

let ParsePrintStatement (tokens: ScannerToken list): ParseResult<Statement> =
    ParseStatement tokens (fun expr -> PrintStatement expr)

let ParseStatementByType (tokens: ScannerToken list): ParseResult<Statement> =
    match tokens with
    | (h::t) when h.Type = PRINT -> ParsePrintStatement t
    | (h::t) -> ParseExpressionStatement tokens
    | [h] when h.Type = EOF -> 
        let error = Err (h.Line, "Expected statement but got EOF")
        Error ([error], [h])
    | [] ->
        let error = Err (-1, "This should never happen. Expected tokens.")
        Error ([error], [])

let ParseVariableDeclaration (tokens: ScannerToken list): ParseResult<Declaration> =
    match tokens with
    | (h1::h2::t) when h1.Type = IDENTIFIER && h2.Type = EQUAL ->
        let expressionResult = ParseExpression t

        match expressionResult with
        | Ok (expression, rest) -> 
            let variableDeclaration = VariableDeclaration (VarIdentifier h1.Lexeme, Some expression)
            let (semiFound, rest2, error) = ExpectToken SEMICOLON rest "Expected ';' after expression"

            if semiFound then
                Ok (variableDeclaration, rest2)
            else
                Error ([error], rest2)
        | Error (e, rest) -> Error (e, rest)
    | (h1::t) when h1.Type = IDENTIFIER ->
        let variableDeclaration = VariableDeclaration (VarIdentifier h1.Lexeme, None)
        let (semiFound, rest2, error) = ExpectToken SEMICOLON t "Expected ';' after variable declaration"
        match semiFound with
        | true -> Ok (variableDeclaration, rest2)
        | false -> Error ([error], rest2)
    | (h1::t) ->
        let error = Err (h1.Line, "Expected identifier after 'var'.")
        Error ([error], h1::t)
    | [h1] -> 
        let error = Err (h1.Line, "Unexpected EOF after 'var'.")
        Error ([error], [])
    | _ -> 
        let error = Err (-1, "Unexpected end of input.")
        Error ([error], [])
    
let ParseDeclaration (tokens: ScannerToken list): ParseResult<Declaration> =
    match tokens with
    | (h::t) when h.Type = VAR -> ParseVariableDeclaration t
    | (h::t) -> 
        let statement = ParseStatementByType tokens
        match statement with
            | Ok (statement, rest) -> Ok (StatementDeclaration statement, rest)
            | Error (e, rest) -> Error (e, rest) 
    | [h] when h.Type = EOF -> 
        let error = Err (h.Line, "Expected declaration but got EOF")
        Error ([error], [h])
    | [] ->
        let error = Err (-1, "This should never happen. Expected tokens.")
        Error ([error], [])

let rec ParseMultipleDeclarations (tokens: ScannerToken list) (parsedDeclarations: Declaration list): ParseResult<Declaration list> =
    match tokens with
    | (h::t) when h.Type = EOF ->
        Ok (List.rev parsedDeclarations, (h::t))
    | _ ->
        let declarationResult = ParseDeclaration tokens
        match declarationResult with
            | Ok (declaration, rest) -> ParseMultipleDeclarations rest (declaration::parsedDeclarations)
            // skip statements with errors
            | Error (e, rest) -> ParseMultipleDeclarations rest parsedDeclarations
    

let ParseProgram (tokens: List<ScannerToken>): ParseResult<Declaration list> =
    let tokensAsList = Seq.toList tokens
    if (tokens.Count = 1) then
        let listHead: ScannerToken = List.head tokensAsList
        if listHead.Type = EOF then
            Ok ([StatementDeclaration (ExpressionStatement Empty)], [])
        else
            // should never happen
            let error = Err (0, "Invalid tokenizations. The list of tokens contains a single token which isn't EOF.")
            assert false
            Error ([error], [])
    else
        ParseMultipleDeclarations (tokensAsList) []



    



