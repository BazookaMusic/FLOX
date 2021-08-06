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