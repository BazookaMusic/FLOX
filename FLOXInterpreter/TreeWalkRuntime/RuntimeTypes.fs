module RuntimeTypes

open RuntimeErrors

[<Struct>]
type FLOXValue =
    | Object of o:obj
    | Nil
    | String of s:string
    | Boolean of b:bool
    | Double of d: double
    | VOID

[<Struct>]
type EvaluationResult = 
    | Ok of v: FLOXValue
    | Error of e: RuntimeError

let StringifyValue (v: FLOXValue) : string =
    match v with
        | Object o -> o.ToString()
        | Nil -> "nil"
        | String str -> str
        | Boolean b -> if b then "true" else "false"
        | Double d -> d.ToString()
        | VOID -> "void"