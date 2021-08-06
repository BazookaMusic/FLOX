module Errors

open System
open System.Collections.Generic

type Errors =
    Error of int * string
    | NoError

let ErrorList = new List<Errors>()

let HasErrors: unit -> bool = fun () -> ErrorList.Count > 0

let Err (line:int, message:string) : Errors =
    eprintfn "Error (line %d): %s" line message
    let error = Error (line,message)
    ErrorList.Add(error)
    error
