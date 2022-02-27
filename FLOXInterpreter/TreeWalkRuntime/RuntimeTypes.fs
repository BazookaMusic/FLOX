module RuntimeTypes

open System.Collections.Generic

open RuntimeErrors

type FunctionIdentifier = string

[<Struct>]
type EvaluationResult<'T> = 
    | Ok of v: 'T
    | Error of e: RuntimeError

[<Struct>]
type FLOXValue =
    | Object of o:obj
    | Nil
    | String of s:string
    | Boolean of b:bool
    | Double of d: double
    | Callable of FunctionIdentifier * ArgumentNameList * (Environment -> EvaluationResult<FLOXValue>)
    | VOID

and Environment =
    | Environment of Dictionary<string, FLOXValue> * Option<Environment>
    | ImmutableEnvironment of Dictionary<string, FLOXValue> * Option<Environment>

and ArgumentNameList = List<string>

let StringifyValue (v: FLOXValue) : string =
    match v with
        | Object o -> o.ToString()
        | Nil -> "nil"
        | String str -> str
        | Boolean b -> if b then "true" else "false"
        | Double d -> d.ToString()
        | VOID -> "void"
        | Callable (id,args, _) -> sprintf "func %s(%d)" id args.Count