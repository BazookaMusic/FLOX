module Interpreter

open System
open Errors
open Scanner
open Parser
open AST
open TreeWalkRuntime
open RuntimeTypes

let UnwrapExpression (parseResult:ParseResult<Expression>): Expression = 
    match parseResult with
    | ParseResult.Ok (expression,_) -> expression
    | ParseResult.Error (e, rest) -> 
        Err (0, (sprintf "%A" e)) |> ignore
        Empty

let PrintResult (result: EvaluationResult) =
    match result with
        | Ok result -> 
            printf "%A" result
        | Error error ->
            eprintf "%A" error

let run (content: string): unit =
    ()
    // ScanTokens content |> Parse |> UnwrapExpression |> Evaluate |> PrintResult

// executes a particular file
let RunFile (filePath: string) = System.IO.File.ReadAllText filePath |> run

let read _ = Console.ReadLine()
let isValid = function null -> false | _ -> true

let RunPrompt = Seq.initInfinite read |> Seq.takeWhile isValid |> Seq.toList |> List.fold (+) "\n"

//let Interpret (source: string) : EvaluationResult =
//    source |> ScanTokensSafe |> ParseSafe |> Evaluate
    