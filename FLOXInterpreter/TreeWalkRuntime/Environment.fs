module Environment

open System.Collections.Generic
open RuntimeTypes
open AST
open RuntimeErrors

type Environment =
    | Environment of Dictionary<string, FLOXValue> * Option<Environment>

let rec GetVariableValue (environment: Environment) (identifier:Identifier): Option<FLOXValue> =
    match environment with
    | Environment (dictionary, linked) ->
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
                
let GetVariableValueOrError (environment: Environment) (identifier: Identifier) : EvaluationResult =
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

let NewEnvironment (parent: Option<Environment>) =
    Environment (new Dictionary<string,FLOXValue>(), parent)
    
    