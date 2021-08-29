module Environment

open System.Collections.Generic
open RuntimeTypes
open AST
open RuntimeErrors

let rec GetVariableValue (environment: Environment) (identifier:Identifier): Option<FLOXValue> =
    match environment with
    | Environment (dictionary, linked) | ImmutableEnvironment (dictionary, linked) ->
        match identifier with
        | VarIdentifier varName ->
            let mutable value: FLOXValue = Nil
            let foundVariable = dictionary.TryGetValue(varName, &value)
            if foundVariable then
                Some value
            else
                match linked with
                    | Some env -> GetVariableValue env identifier
                    | None -> None
                
let GetVariableValueOrError (environment: Environment) (identifier: Identifier) : EvaluationResult<FLOXValue> =
    match GetVariableValue environment identifier with
    | Some value -> Ok value
    | None ->
        let VarIdentifier varName as v = identifier
        Error (DefaultUndefinedVariableError varName)
        

let DefineVariable (environment: Environment) (identifier:Identifier) (value: FLOXValue): unit  =
    match environment with
        | Environment (dictionary, _) ->
            let VarIdentifier (varName) as v = identifier
            dictionary.[varName] <- value
        | ImmutableEnvironment _  -> () // immutable environments cannot be modified

let rec SetVariableValue (environment: Environment) (identifier:Identifier) (value: FLOXValue): bool =
    match environment with
    | Environment (dictionary, linked) ->
        match identifier with
        | VarIdentifier varName ->
            let foundVariable = dictionary.ContainsKey(varName)
            if foundVariable then
                dictionary.[varName] <- value
                true
            else
                match linked with
                    | Some env -> SetVariableValue env identifier value
                    | None -> false
    | ImmutableEnvironment (dictionary, linked) ->
        match linked with
            | None -> false
            | Some env -> SetVariableValue env identifier value

let NewEnvironment (parent: Option<Environment>) =
    Environment (new Dictionary<string,FLOXValue>(), parent)

let NewImmutableEnvironment (env: Environment) =
    match env with
        | ImmutableEnvironment _ -> env
        | Environment (dictionary, parent) -> ImmutableEnvironment (dictionary, parent)

let rec EnvironmentWithNameValueMapping (environment: Environment) (names: List<string>) (values: List<FLOXValue>) (index: int) =
    if index = names.Count then
        environment
    else
        DefineVariable environment (VarIdentifier names.[index]) values.[index]
        EnvironmentWithNameValueMapping environment names values (index + 1)

let EnvironmentFromArguments (parent: Environment) (argumentNames: List<string>) (argumentValues: List<FLOXValue>) =
    let functionEnvironment = NewEnvironment (Some parent)
    EnvironmentWithNameValueMapping functionEnvironment argumentNames argumentValues 0
    
    
    