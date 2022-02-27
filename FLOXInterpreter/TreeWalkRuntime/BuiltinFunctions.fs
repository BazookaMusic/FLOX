module BuiltinFunctions

open System.Collections.Generic
open System

open RuntimeTypes
open Environment
open AST
open RuntimeErrors;

let NoArguments = new List<string>(0)
let RandomGenerator = new Random()

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

// ToString

let toStringFunction (environment : Environment) =
    let value = GetVariableValue environment (VarIdentifier "value")
    match value with
        | Some v -> Ok (String (StringifyValue v))
        | None -> Ok (Nil)

let ToStringFunction =
    let arguments = new List<string>(1)
    arguments.Add "value"
    Callable ("toString", arguments, toStringFunction)

// MATH

let randomInt (environment : Environment) : EvaluationResult<FLOXValue> = 
    let maxValue = GetVariableValue environment (VarIdentifier "maxValue")
    match maxValue with
        | Some v -> 
            match v with
                | Double dv -> (RandomGenerator.Next (int dv)) |>  (fun x -> Ok (Double (float x)))
                | otherValue ->
                    let otherType = otherValue.GetType()
                    Error (ValueCastError (sprintf "Cannot cast type '%s' to int." otherType.Name))
        | None -> Ok (Nil)

let RandomIntFunction =
    let arguments = new List<string>(1)
    arguments.Add "maxValue"

    Callable ("random", arguments, randomInt)

let PowerFunction = 
    let arguments = new List<string>(2)
    arguments.Add "number"
    arguments.Add "exponent"
    Callable ("pow",  arguments, powerFunction)

// IO

let tryParseInt s = 
    try 
        s |> int |> Some
    with :? FormatException -> 
        None

let tryParseDouble s =
    try
        s |> double |> Some
    with :? FormatException ->
        None

let ConvertToNumber (s : string) =
    match tryParseInt s with
        | Some intValue -> Ok (Double (float intValue))
        | None ->
            match tryParseDouble s with
            | Some doubleValue -> Ok (Double doubleValue)
            | None -> Error (ValueCastError (sprintf "Could not convert value '%s' to a number." s))

let parseNumberFunction environment =
    let value = GetVariableValue environment (VarIdentifier "value")
    match value with
        | Some v ->
            match v with
                | String sv ->
                    ConvertToNumber sv
                | otherValue -> Error (ValueCastError (sprintf "Could not convert value '%A' to a number because it is not a string." otherValue))
        | None -> Error (ValueCastError ("Could not convert null value to a number because it is not a string."))

let readLineFunction environment=
    let line = Console.ReadLine()
    Ok (String line)

let writeLineFunction environment =
    let number = GetVariableValue environment (VarIdentifier "line")
    match number with
        | Some v -> Console.WriteLine(StringifyValue v)
        | None -> Console.WriteLine()
    Ok VOID

let ParseNumberFunction =
    let arguments = new List<string>(1)
    arguments.Add "value"
    Callable ("parseNumber", arguments, parseNumberFunction)

let ReadLineFunction =
    let arguments = NoArguments
    Callable ("readLine", arguments, readLineFunction)

let WriteLineFunction = 
    let arguments = new List<string>(1)
    arguments.Add "line"
    Callable ("writeLine", arguments, writeLineFunction)

let BuiltinFunctions = [
    ClockFunction
    PowerFunction
    ReadLineFunction
    WriteLineFunction
    RandomIntFunction
    ToStringFunction
    ParseNumberFunction
]

let DefineBuiltinFunctions environment =
    let defineFunction func =
        let Callable (name, _, _) as c = func
        DefineVariable environment (VarIdentifier name) func

    List.iter defineFunction BuiltinFunctions
    environment