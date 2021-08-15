module RuntimeTypes

open RuntimeErrors

type FLOXValue =
    | Object of obj
    | Nil
    | String of string
    | Boolean of bool
    | Double of double
    | VOID

type EvaluationResult = 
    | Ok of FLOXValue
    | Error of RuntimeError

let StringifyValue (v: FLOXValue) : string =
    match v with
        | Object o -> o.ToString()
        | Nil -> "nil"
        | String str -> str
        | Boolean b -> if b then "true" else "false"
        | Double d -> d.ToString()
        | VOID -> "void"