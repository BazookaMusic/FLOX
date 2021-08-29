module BuiltinFunctions

open System.Collections.Generic
open System

open RuntimeTypes
open Environment
open AST
open RuntimeErrors;

let currentTimeFunction environment =
    let currentTicks = DateTime.Now.Ticks
    let currentMilliseconds = double (currentTicks / (TimeSpan.TicksPerMillisecond))
    Ok (Double currentMilliseconds)

let ClockFunction = Callable ("clock", new List<string>(0), currentTimeFunction)

let powerFunction environment =
    let number = GetVariableValue environment (VarIdentifier "number")
    let exponent = GetVariableValue environment (VarIdentifier "exponent")

    match number with
        | Some (Double n) ->
            match exponent with
                | Some (Double e) ->
                    Ok (Double (Math.Pow (n,e)))
                | Some v -> Error (ValueCastError (sprintf "Could not cast value {%s} to double." (StringifyValue v)))
                | None -> Error (DefaultUndefinedVariableError "exponent")
        | Some v -> Error (ValueCastError (sprintf "Could not cast value {%s} to double." (StringifyValue v)))
        | None -> Error (DefaultUndefinedVariableError "number")


let PowerFunction = 
    let arguments = new List<string>(2)
    arguments.Add "number"
    arguments.Add "exponent"
    Callable ("pow",  arguments, powerFunction)

let BuiltinFunctions = [
    ClockFunction
    PowerFunction
]

let DefineBuiltinFunctions environment =
    let defineFunction func =
        let Callable (name, _, _) as c = func
        DefineVariable environment (VarIdentifier name) func

    List.iter defineFunction BuiltinFunctions