module Interpreter

open System
open Errors
open Scanner
open Parser
open AST
open TreeWalkRuntime
open RuntimeTypes
open FlowException

let UnwrapExpression (parseResult:ParseResult<Expression>): Expression = 
    match parseResult with
    | ParseResult.Ok (expression,_) -> expression
    | ParseResult.Error (e, rest) -> 
        Err (0, (sprintf "%A" e)) |> ignore
        Empty

let PrintResult (result: EvaluationResult<FLOXValue>) =
    match result with
        | Ok result -> 
            printf "%A" result
        | Error error ->
            eprintf "%A" error

let ValidateParsing (parsingResult: ParseResult<Declaration list>) : Declaration list =
    match parsingResult with
        | ParseResult.Ok (declList, _) as okList -> declList
        | ParseResult.Error (_, _) ->
            failwith "Parsing failed"

let ParseDeclarations source : Declaration list = source |> ScanTokens |> ParseProgram |> ValidateParsing

let rec EvaluateDeclarations (declarations: Declaration list) : EvaluationResult<FLOXValue> =
    match declarations with
        | [] -> Ok (VOID)
        | (h::t) ->
            try
                let declarationResult = EvaluateDeclaration (GlobalEnvironment) h
                if (t.IsEmpty) then
                    declarationResult
                else
                    EvaluateDeclarations t
            with
                | FlowReturn v -> Ok v

let RunProgram (content: string): unit =
    (ParseDeclarations content) |> EvaluateDeclarations |> PrintResult

// executes a particular file
let RunFile (filePath: string) = System.IO.File.ReadAllText filePath |> RunProgram