module RuntimeErrors

type RuntimeError =
 | ValueCastError of string
 | FatalError of string
 | NullReferenceError of string
 | VoidComparisonError of string

let DefaultNullRef = NullReferenceError "NullReferenceError: Attempted to de-reference a null reference."

let DefaultCastError (type1:string) (type2:string) = ValueCastError (sprintf "ValueCastError: Tried to convert a value of type '%s' to a value of type '%s'." type1 type2)

let DefaultVoidComparisonError = VoidComparisonError "Cannot compare two values of type 'void'."

