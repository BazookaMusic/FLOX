module Scanner

open System.Collections.Generic
open Errors
open System
open Tokens

type ScanState = {Start: int; Current: int; Line: int}

let IsNext (source: string) (expected: char) (state: ScanState) : bool = 
    if state.Current = (source.Length - 1) then
        false
    else
        source.[state.Current + 1] = expected

// State transitions

let OneCharNextState (state: ScanState) : ScanState =
    {Start = state.Current + 1; Current = state.Current + 1; Line = state.Line}

let TwoCharNextState (state: ScanState) : ScanState =
    {Start = state.Current + 2; Current = state.Current + 2; Line = state.Line}

let NewlineNextState (state: ScanState) : ScanState =
    {Start = state.Start + 1; Current = state.Current + 1; Line = state.Line + 1}

let WhiteSpaceNextState = OneCharNextState

// Finds the index of a char in the source string and returns that and the line it belongs on.
let rec findCharLocation (c: char, source: string, start: int, line: int, forbiddenCharacters: List<char>): int * int =
    if start = source.Length then
        let error = Err (line, sprintf "Expected char '%c' not found" c)
        source.Length, line
    else
        let character = source.[start];

        if (forbiddenCharacters.Contains(character)) then
            -1, line
        elif character = c then
            start, line
        elif character = '\n' then
            findCharLocation (c, source, start + 1, line + 1, forbiddenCharacters)
        else
            findCharLocation (c, source, start + 1, line, forbiddenCharacters)

let findQuote source start line =
    let forbiddenCharacters = new List<char>(0)
    findCharLocation ('"',source,start + 1, line, forbiddenCharacters)
 
let findNewLineLocation source start line =
    let forbiddenCharacters = new List<char>(0);
    findCharLocation ('\n',source,start, line, forbiddenCharacters)

let CommentNextState (source: string) (state:ScanState): ScanState =
    let newLineLocation,_ = findNewLineLocation source state.Start state.Line
    {Start = newLineLocation + 1; Current = newLineLocation + 1; Line = state.Line + 1}

let StringNextState (source: string) (state:ScanState): ScanState =
    let quoteLocation, quoteLine = findQuote source state.Start state.Line

    if (quoteLocation < 0) then
        Err (state.Line, sprintf "Could not parse string") |> ignore
        // end parsing
        {Start = source.Length; Current = source.Length; Line = quoteLine}
    else
        {Start = quoteLocation + 1; Current = quoteLocation + 1; Line = quoteLine}


// Numerics

let IsDigit c = c >= '0' && c <= '9'

let rec ParseNumber (source: string, state: ScanState, decimalPointFound: bool): ScanState =
    if state.Current = source.Length then
        state
    else
        let character = source.[state.Current]
        if IsDigit character then
            ParseNumber (source, OneCharNextState state, decimalPointFound)
        // lookahead one. There should be digits after .
        elif character = '.' && (not decimalPointFound) && (state.Current + 1) < source.Length && IsDigit source.[state.Current + 1]  then
            ParseNumber (source, OneCharNextState state, true)
        else
            state

let NumberNextState (source:string) (state:ScanState) =
    ParseNumber (source, state, false)

let NumberLexemeToLiteral (lexeme:string) : obj =
    match System.Int32.TryParse lexeme with
        | true,integer -> upcast integer
        | _ -> match System.Double.TryParse lexeme with
            | true, doubleLiteral -> upcast doubleLiteral
            | _ -> null

// Identifiers
let IsAlpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let IsAlphaNum c = IsDigit c || IsAlpha c

let rec ParseIdentifier (source: string, state: ScanState): ScanState =
    if state.Current = source.Length then
        state
    else
        let character = source.[state.Current]
        if IsAlphaNum character then
            ParseIdentifier (source, OneCharNextState state)
        else
            state

let IdentifierNextState (source: string) (state: ScanState) = 
    ParseIdentifier (source, state)
    

let LexemeToLiteral (tokenType:TokenType) (lexeme:string) : obj =
    match tokenType with
    | STRING -> upcast lexeme.[1..(lexeme.Length - 2)]
    | NUMBER -> NumberLexemeToLiteral lexeme
    | _ -> null

let DetermineType tokenType lexeme line: TokenType =
    match tokenType with
        | IDENTIFIER -> 
            let specialType = MatchSpecialIdentifiers lexeme line
            match (specialType) with
            | UNKNOWN -> tokenType
            | _ -> specialType
        | _ -> tokenType
    
        
        
let MatchToken (source: string) (state: ScanState) : ScannerToken * ScanState =
    let c = source.[state.Current]

    let parsedToken = match c with
        | '/' -> if (IsNext source '/' state) then (IGNORE, CommentNextState source state) else (SLASH, OneCharNextState state) 
        | '\n' ->(IGNORE, NewlineNextState state)
        | ' ' | '\r' | '\t' -> (IGNORE, WhiteSpaceNextState state)
        | '(' -> (LEFT_PAREN, OneCharNextState state)
        | ')' -> (RIGHT_PAREN, OneCharNextState state)
        | '{' -> (LEFT_BRACE, OneCharNextState state)
        | '}' -> (RIGHT_BRACE, OneCharNextState state)
        | ',' -> (COMMA, OneCharNextState state)
        | '-' -> (MINUS, OneCharNextState state)
        | '+' -> (PLUS, OneCharNextState state)
        | ';' -> (SEMICOLON, OneCharNextState state)
        | '*' -> (STAR, OneCharNextState state)
        | '!' -> if (IsNext source '=' state) then (BANG_EQUAL, TwoCharNextState state) else (BANG, OneCharNextState state)
        | '=' -> if (IsNext source '=' state) then (EQUAL_EQUAL, TwoCharNextState state) else (EQUAL, OneCharNextState state)
        | '<' -> if (IsNext source '=' state) then (LESS_EQUAL, TwoCharNextState state) else (LESS, OneCharNextState state)
        | '>' -> if (IsNext source '=' state) then (GREATER_EQUAL, TwoCharNextState state) else (GREATER, OneCharNextState state)
        | '"' -> (STRING, StringNextState source state)
        | c when IsDigit c -> (NUMBER, NumberNextState source state)
        | c when IsAlpha c -> (IDENTIFIER, IdentifierNextState source state)
        | _   -> 
            Err (state.Line, sprintf "Unexpected character %c" c) |> ignore
            (UNKNOWN, OneCharNextState state)
    
    let currentState = snd parsedToken
    let lexeme = source.[state.Start..currentState.Current - 1]
    let tokenType =  fst parsedToken
    ({Type = DetermineType tokenType lexeme state.Line; Lexeme = lexeme; Literal = LexemeToLiteral tokenType lexeme; Line = state.Line}, currentState)

let rec ScanTokensIteration (source: string, listOfTokens: List<ScannerToken>, state: ScanState): List<ScannerToken> =
    if (state.Current >= source.Length) then
        // add EOF for clarity
        let eofToken = {Type = EOF; Lexeme = ""; Literal = null; Line = state.Line}
        listOfTokens.Add(eofToken)
        listOfTokens
    else
        let token, nextState = MatchToken source state

        if (not (token.Type = IGNORE)) then
            listOfTokens.Add(token)
        else
            () |> ignore
        
        ScanTokensIteration (source, listOfTokens, nextState)

// Scanning function
let ScanTokens (source: string): List<ScannerToken> =
    let li = new List<ScannerToken>()
    ScanTokensIteration (source, li, {Start = 0; Current = 0; Line = 1})

let ScanTokensSafe (source: string): List<ScannerToken> =
    let tokens = ScanTokens source

    if (not (HasErrors())) then
        tokens
    else
        new List<ScannerToken>(0)


