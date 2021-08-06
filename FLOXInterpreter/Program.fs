module Main 

open System
open Interpreter
open Errors

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        let results = RunFile argv.[0]
        if (not (HasErrors())) then
            exit 0
        else
            exit 64
    elif argv.Length = 0 then
        RunPrompt |> ignore
        exit 0
    else
        printfn "Usage: jlox [script file] | jlox"
        exit 64
        
        
        
