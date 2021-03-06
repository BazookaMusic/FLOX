module RuntimeErrors

type RuntimeError =
 | ValueCastError of string
 | FatalError of string
 | NullReferenceError of string
 | VoidComparisonError of string
 | UndefinedVariableError of string
 | ArgumentEvaluationError of string * RuntimeError
 | ArgumentCountError of string
 | InvalidArgumentError of string

let DefaultNullRef = NullReferenceError "NullReferenceError: Attempted to de-reference a null reference."

let DefaultCastError (type1:string) (type2:string) = ValueCastError (sprintf "ValueCastError: Tried to convert a value of type '%s' to a value of type '%s'." type1 type2)

let DefaultVoidComparisonError = VoidComparisonError "Cannot compare two values of type 'void'."

let DefaultUndefinedVariableError varName = UndefinedVariableError (sprintf "The variable '%s' cannot be accessed because it is undefined." varName)

let DefaultArgumentCountError (functionName: string) (expectedArity:int) (actualArgumentCount: int) =
    ArgumentCountError (sprintf "The function '%s' expected %d arguments but got %d instead." functionName expectedArity actualArgumentCount)

