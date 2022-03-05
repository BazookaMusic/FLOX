module Parser

open Scanner
open Errors
open System.Collections.Generic
open AST
open Tokens

let MaxArgumentCount = 255;
    
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
        | Assign (VarIdentifier v, expr) -> sprintf "(Assign (%s) (%s))" v (TreeToString expr)
        | other -> "Unknown"

let rec private CountFollowingOccurencesImpl (tokenType: TokenType) (tokens: ScannerToken list) (acc:int) =
    match tokens with
        | (h::t) when h.Type = tokenType -> CountFollowingOccurencesImpl tokenType t (acc + 1)
        | _ -> acc

let CountFollowingOccurences (tokenType: TokenType) (tokens: ScannerToken list) =
    CountFollowingOccurencesImpl tokenType tokens 0

type ParseResult<'T> =
    | Ok of 'T * ScannerToken list
    | Error of Errors list * ScannerToken list

let UnexpectedEOFParser<'a> : ParseResult<'a> =
    let error = Err (-1, "Unexpected EOF while parsing")
    Error ([error], [])

let ParseToken (tokenType: TokenType) (tokens: ScannerToken list) (lexeme: string) : ParseResult<bool> =
    match tokens with
        | (first::rest) when first.Type = tokenType -> Ok (true, rest)
        | (first::rest) -> 
            let error = Err (first.Line, sprintf "Expected '%s' but got '%s' instead." lexeme first.Lexeme)
            Error ([error], tokens)
        | [] ->
            UnexpectedEOFParser

let AndThen<'T, 'R> (v: ParseResult<'T>) (f: 'T * ScannerToken list -> ParseResult<'R>) : ParseResult<'R> =
    match v with
        | Ok (ok, rest) -> 
            f (ok, rest)
        | Error (e, rest) -> Error (e, rest)

let (.>>.) item next = AndThen item next

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
    | tokens -> ParseCall tokens

and ParseArgumentsImpl (tokens: ScannerToken list) (arguments: Expression list) (argCount: int): ParseResult<Expression list> =
    match tokens with
        | (h::t) when h.Type = RIGHT_PAREN ->
            Ok ((List.rev arguments), t)
        | _ ->
            let expr = ParseExpression tokens

            let next = fun (expr, (rest: ScannerToken list)) ->
                match rest with
                    | (h::t) when h.Type = COMMA ->
                        ParseArgumentsImpl t (expr::arguments) (argCount + 1)
                    | (h::t) when h.Type = RIGHT_PAREN ->
                        Ok ((List.rev (expr::arguments)), t)
                    | (h::t) -> 
                        let err = Err (h.Line, sprintf "Expected ',' or ')' at function call but got '%s' instead'" h.Lexeme)
                        Error ([err], t)
                    | _ -> UnexpectedEOFParser

            expr .>>. next
and HandleMaximumArgumentCount (tokens: ScannerToken list) (arguments: Expression list) (argCount: int): ParseResult<Expression list> =
    if (argCount > MaxArgumentCount) then
        match tokens with
            | (h::t) -> 
                let err = Err (h.Line, sprintf "Exceeded max amount of arguments which is '%d'" MaxArgumentCount)
                Error ([err], t)
            | _ -> UnexpectedEOFParser
    else
        ParseArgumentsImpl tokens arguments argCount

and ParseArguments (tokens: ScannerToken list) (arguments: Expression list): ParseResult<Expression list> =
    HandleMaximumArgumentCount tokens arguments 0

and ParseCall (tokens: ScannerToken list): ParseResult<Expression> =
    let primary = ParsePrimary tokens
    primary .>>. 
        fun (prim, rest) ->
            match rest with
                | (h::t) when h.Type = LEFT_PAREN ->
                    let arguments = ParseArguments t []

                    arguments .>>. 
                        (fun (args, rest) ->
                            Ok (Call (prim, args), rest))
                | _ -> primary
                
and ParseGroup (tokens: ScannerToken list): ParseResult<Expression> =
        match ParseExpression tokens with
        | Ok (expr, newTail) ->
            match newTail with
            | (nh::nt) when nh.Type = RIGHT_PAREN -> Ok (Grouping expr, nt)
            | (nh::nt) -> 
                let error = Err (nh.Line, sprintf "Expected ')' after expression but got '%s' instead." nh.Lexeme)
                Error ([error], nt)
            | _ -> 
                UnexpectedEOFParser
        | error -> error
    
and ParsePrimary (tokens: ScannerToken list): ParseResult<Expression> =
    match tokens with
    | (h::t) ->
        match h.Type with
        | TokenType.NUMBER -> Ok (Literal (Literal.NUMBER (ObjToNumeric h.Literal)), t)
        | TokenType.STRING -> Ok (Literal (Literal.STRING (h.Literal :?> string)), t)
        | TokenType.TRUE -> Ok (Literal (Literal.TRUEVAL), t)
        | TokenType.FALSE -> Ok (Literal (Literal.FALSEVAL), t)
        | TokenType.NIL -> Ok (Literal (Literal.NIL), t)
        | TokenType.IDENTIFIER -> Ok (Literal (Literal.IDENTIFIER (VarIdentifier h.Lexeme)), t)
        | TokenType.LEFT_PAREN ->
            ParseGroup t
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

and ParseAND (tokens: ScannerToken list): ParseResult<Expression> =
    let parseEq = ParseEquality tokens
    match parseEq with
    | Ok (expr, rest) -> 
        let eqTokens (token: ScannerToken) = match token.Type with
        | TokenType.AND -> true
        | _ -> false

        ParseMultipleOfSameType rest ParseAND expr eqTokens "and"
    | _ -> parseEq

and ParseOR (tokens: ScannerToken list): ParseResult<Expression> =
    let parseAND = ParseAND tokens
    match parseAND with
    | Ok (expr, rest) -> 
        let restOfTokens (token: ScannerToken) = match token.Type with
        | TokenType.OR -> true
        | _ -> false

        ParseMultipleOfSameType rest ParseOR expr restOfTokens "or"
    | _ -> parseAND
    
and ParseComparison (tokens: ScannerToken list): ParseResult<Expression> = 
    let parseTerms = ParseTerm tokens
    match parseTerms with
    | Ok (expr, rest) -> 
        let comparisonTokens (token: ScannerToken) = match token.Type with
            | TokenType.LESS | TokenType.LESS_EQUAL | TokenType.GREATER | TokenType.GREATER_EQUAL -> true
            | _ -> false

        ParseMultipleOfSameType rest ParseTerm expr comparisonTokens "comparison"
    | _ -> parseTerms

and ParseEquality (tokens: ScannerToken list): ParseResult<Expression> = 
    let parseComp = ParseComparison tokens
    match parseComp with
    | Ok (expr, rest) -> ParseMultipleOfSameType rest ParseComparison expr  (fun token -> token.Type = BANG_EQUAL || token.Type = EQUAL_EQUAL) "equality"
    | Error (errors, rest) -> parseComp

and IsAssignment (tokens: ScannerToken list): bool =
    match tokens with
    | (identifier::eq::rest) when identifier.Type = IDENTIFIER && eq.Type = EQUAL -> true
    | _ -> false

and Assignment (identifier:Identifier) (tokens: ScannerToken list) : ParseResult<Expression> =
    let expressionResult = ParseExpression tokens

    match expressionResult with
    | Ok (expression, restOfTokens) -> Ok (Expression.Assign (identifier, expression), restOfTokens)
    | error -> error
    
and ParseExpression (tokens: ScannerToken list): ParseResult<Expression> =
    let expressionResult = ParseOR tokens

    match expressionResult with
    // Assignment
    | Ok (Expression.Literal (Literal.IDENTIFIER identifier), (h::rest)) when h.Type = EQUAL -> Assignment identifier rest
    // Equality
    | _ -> expressionResult

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

let ParseReturnStatement (tokens: ScannerToken list) : ParseResult<Statement> =
    ParseStatement tokens (fun expr -> ReturnStatement expr)

let rec ParseMultipleDeclarations (tokens: ScannerToken list) (parsedDeclarations: Declaration list) (expectedEnd: TokenType): ParseResult<Declaration list> =
    match tokens with
    | (h::t) when h.Type = expectedEnd ->
        Ok (List.rev parsedDeclarations, (h::t))
    | _ ->
        let declarationResult = ParseDeclaration tokens
        match declarationResult with
            | Ok (declaration, rest) -> ParseMultipleDeclarations rest (declaration::parsedDeclarations) expectedEnd
            // skip statements with errors
            | Error (e, []) -> Error (e, [])
            | Error (e, rest) -> ParseMultipleDeclarations rest parsedDeclarations expectedEnd

and ParseBlock (tokens: ScannerToken list): ParseResult<Statement> =
    let declarationsResult = ParseMultipleDeclarations tokens [] RIGHT_BRACE

    match declarationsResult with
    | Ok (declarationList, (h::rest)) when h.Type = RIGHT_BRACE -> Ok (Statement.Block declarationList, rest)
    | Ok (_, (h::rest)) ->
        let error = Err (h.Line, sprintf "Expected '}' at the end of a block, got '%s' instead." h.Lexeme)
        Error ([error], h::rest)
    | Ok (_, []) ->
        let error = Err (-1, "Unexpected EOF")
        Error ([error], [])
    | Error (errors, rest) -> Error (errors, rest)

and ParseIfStatement (tokens: ScannerToken list): ParseResult<Statement> =
    match tokens with
    | (h::t) when h.Type = LEFT_PAREN ->
        let predicateResult = ParseGroup t

        match predicateResult with
        | Ok (Grouping predicateExpr, rest) -> 
            let statementResult = ParseStatementByType rest

            match statementResult with
                | Ok (statement, rest) ->
                    let elseStatemementResult = match rest with
                        | (h::t) when h.Type = ELSE ->
                            Some (ParseStatementByType t)
                        | _ -> None

                    match elseStatemementResult with
                        | Some (Error _ as error) -> error
                        | None -> Ok (IfStatement (predicateExpr, statement, None), rest)
                        | Some (Ok (elseStatement, afterElse)) -> Ok (IfStatement (predicateExpr, statement, Some elseStatement), afterElse)
                | error -> error
        | Error (e, rest) -> Error (e, rest)
    | (h::t) as li ->
        let error = Err (h.Line, sprintf "Expected '(' after 'if' but got '%s' instead." h.Lexeme)
        Error ([error], li)
    | [] -> 
        UnexpectedEOFParser

and ParseWhileStatement (tokens: ScannerToken list): ParseResult<Statement> =
    match tokens with
    | (h::t) when h.Type = LEFT_PAREN ->
        let predicateResult = ParseGroup t

        match predicateResult with
        | Ok (Grouping predicateExpr, rest) -> 
            let statementResult = ParseStatementByType rest

            match statementResult with
                | Ok (statement, rest) ->
                    Ok (WhileStatement (predicateExpr, statement), rest)
                | error -> error
        | Error (e, rest) -> Error (e, rest)
    | (h::t) as li ->
        let error = Err (h.Line, sprintf "Expected '(' after 'while' but got '%s' instead." h.Lexeme)
        Error ([error], li)
    | [] ->
        UnexpectedEOFParser

and ParseFirstForStatement (tokens: ScannerToken list) =
    let declaration = ParseDeclaration tokens

    match declaration with
        | Ok (VariableDeclaration (identifier, stmt) as vd, afterStmt) ->
            match stmt with
                | Some expresion -> Ok (vd, afterStmt)
                | _ ->
                    let token = List.head tokens
                    let error = Err (token.Line, "Expected statement")
                    Error ([error], afterStmt)
        | other -> other

and ParseSecondStatement (tokens: ScannerToken list) =
    ParseExpressionStatement tokens

and ParseForStatement (tokens: ScannerToken list) =
    let token = ParseToken LEFT_PAREN tokens "("
    
    AndThen token (fun (_, afterParsedToken) ->
        let firstFor = match afterParsedToken with
            |  (h::t) when h.Type = SEMICOLON -> Ok (StatementDeclaration (ExpressionStatement Empty), t)
            | e -> ParseFirstForStatement afterParsedToken

        AndThen firstFor (fun (firstDeclaration, afterFirstDeclaration) ->
            let conditionExpr = match afterFirstDeclaration with
                                    | (h::t) when h.Type = SEMICOLON -> Ok (ExpressionStatement Empty, t)
                                    | _ -> ParseExpressionStatement afterFirstDeclaration

            AndThen conditionExpr (fun (conditionExpression, afterConditionExpression) ->
                let finalStatementExpr = match afterConditionExpression with
                                                | (h::t) when h.Type = SEMICOLON -> Ok (ExpressionStatement Empty, t)
                                                | _ -> ParseExpressionStatement afterConditionExpression

                AndThen finalStatementExpr (fun (finalStatementExpr, afterFinalCondition) ->
                    let rightParen = ParseToken RIGHT_PAREN afterFinalCondition ")"
                    AndThen rightParen (fun (_, afterRightParen) ->
                        let body = ParseStatementByType afterRightParen
                        
                        AndThen body (fun (bodyStatement, afterBody) ->
                            let firstDeclarationOption = if firstDeclaration = StatementDeclaration (ExpressionStatement Empty) then None else Some firstDeclaration
                            let conditionOption = if conditionExpression = ExpressionStatement Empty then None else Some conditionExpression
                            let finalStatementOption = if finalStatementExpr = ExpressionStatement Empty then None else Some finalStatementExpr
                        
                            Ok (ForStatement (firstDeclarationOption, conditionOption, finalStatementOption, bodyStatement), afterBody)
                        )
                    )
                )
            )
        )
    )

and ParseStatementByType (tokens: ScannerToken list): ParseResult<Statement> =
    match tokens with
    | (h::t) when h.Type = FOR -> ParseForStatement t
    | (h::t) when h.Type = WHILE -> ParseWhileStatement t
    | (h::t) when h.Type = IF -> ParseIfStatement t
    | (h::t) when h.Type = LEFT_BRACE -> ParseBlock t
    | (h::t) when h.Type = PRINT -> ParsePrintStatement t
    | (h::t) when h.Type = RETURN -> ParseReturnStatement t
    | (h::t) -> ParseExpressionStatement tokens
    | [h] when h.Type = EOF -> 
        let error = Err (h.Line, "Expected statement but got EOF")
        Error ([error], [h])
    | [] ->
        let error = Err (-1, "This should never happen. Expected tokens.")
        Error ([error], [])

and ParseVariableDeclaration (tokens: ScannerToken list): ParseResult<Declaration> =
    match tokens with
    | (h1::h2::t) when h1.Type = IDENTIFIER && h2.Type = EQUAL ->
        let expressionResult = ParseExpression t

        match expressionResult with
        | Ok (expression, rest) -> 
            let variableDeclaration = VariableDeclaration (VarIdentifier h1.Lexeme, Some expression)
            let (semiFound, rest2, error) = ExpectToken SEMICOLON rest "Expected ';' after expression."

            if semiFound then
                Ok (variableDeclaration, rest2)
            else
                Error ([error], rest2)
        | Error (e, rest) -> Error (e, rest)
    | (h1::t) when h1.Type = IDENTIFIER ->
        let variableDeclaration = VariableDeclaration (VarIdentifier h1.Lexeme, None)
        let (semiFound, rest2, error) = ExpectToken SEMICOLON t "Expected ';' after variable declaration."
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
        UnexpectedEOFParser<Declaration>

and ParseArgumentDeclarations (tokens: ScannerToken list) (acc: Identifier list): ParseResult<Identifier list> =
    match tokens with
        | (h::t) when h.Type = RIGHT_PAREN ->
            Ok (List.rev acc, t)
        | _ ->
            let identifer = ParsePrimary tokens

            identifer .>>. (fun (id, afterId) ->
                match id with
                    | Expression.Literal ((Literal.IDENTIFIER identifier)) ->
                        match afterId with
                            | (h::t) when h.Type = COMMA ->
                                ParseArgumentDeclarations t (identifier::acc)
                            | (h::t) when h.Type = RIGHT_PAREN ->
                                ParseArgumentDeclarations (h::t) (identifier::acc)
                            | (h::t) ->
                                let error = Err (h.Line, sprintf "Expected ',' or ')' in argument declaration but got '%s' instead." h.Lexeme)
                                Error ([error], t)
                            | _ -> UnexpectedEOFParser
                    | _ ->
                        match afterId with
                            | (h::t) ->
                                let error = Err (h.Line, sprintf "Expected identifier in argument declaration.")
                                Error ([error], t)
                            | _ -> UnexpectedEOFParser)

and ParseFunctionArgumentDeclarations (tokens: ScannerToken list): ParseResult<Identifier list> =
    ParseArgumentDeclarations tokens []

and ParseFunctionDeclaration (tokens: ScannerToken list): ParseResult<Declaration> =
    let identifier = ParsePrimary tokens

    identifier .>>. (fun (id, rest) ->
        match id with
            | Expression.Literal (Literal.IDENTIFIER identifierName) ->
                let leftParen = ParseToken LEFT_PAREN rest "("

                leftParen .>>. (fun (_, afterParen) ->
                        let arguments = ParseFunctionArgumentDeclarations afterParen

                        arguments .>>. (fun (argumentList, after) ->
                            let leftBrace = ParseToken LEFT_BRACE after "{"

                            leftBrace .>>. (fun (_, after) ->
                                let block = ParseBlock after
                                block .>>. (function (functionBody, after) ->
                                                Ok (FunctionDeclaration (identifierName, argumentList, functionBody), after)
                                           )
                                )
                        )
                    )
            | _ ->
                let firstToken = List.head tokens
                let error = Err (firstToken.Line, sprintf "Expected identifier but got '%s' instead." firstToken.Lexeme)
                Error ([error], rest)
            )

and ParseDeclaration (tokens: ScannerToken list): ParseResult<Declaration> =
    match tokens with
    | (h::t) when h.Type = VAR -> ParseVariableDeclaration t
    | (h::t) when h.Type = FUN -> ParseFunctionDeclaration t
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
        ParseMultipleDeclarations (tokensAsList) [] EOF



    



