module TreeWalkRuntime

open AST
open RuntimeErrors
open RuntimeTypes
open Environment

let ConvertToNumeric (value: FLOXValue): EvaluationResult =
    match value with
        | Double num -> Ok (Double num)
        | Object object -> Ok (Double (object :?> double))
        | Nil -> Error DefaultNullRef
        | String _ -> Error (ValueCastError "Cannot convert a value of type 'string' to a number.")
        | VOID -> Error (ValueCastError "Cannot convert a value a value of type 'void' to a number.")
        | Boolean _ -> Error (ValueCastError "Cannot convert a value a value of type 'boolean' to a number.")

let CombineNumericValues (leftResult: EvaluationResult) (rightResult: EvaluationResult) (operation: (double -> double -> double) ) =
    match (leftResult, rightResult) with
        | (Ok a, Ok b) ->
            let numericA = ConvertToNumeric a
            let numericB = ConvertToNumeric b

            match (numericA,numericB) with
            | (Ok (Double num1), Ok (Double num2)) -> Ok (Double (operation num1 num2))
            | (Error error as e, _) -> e
            | (_, (Error error as e)) -> e
            | _ -> Error (FatalError "Failed to apply numeric operation to values.")
        | (Error error as e, _) -> e
        | (_, (Error error as e)) -> e

let CompareNumericValues (leftResult: EvaluationResult) (rightResult: EvaluationResult) (operation: (double -> double -> bool) ) =
    match (leftResult, rightResult) with
        | (Ok a, Ok b) ->
            let numericA = ConvertToNumeric a
            let numericB = ConvertToNumeric b

            match (numericA,numericB) with
            | (Ok (Double num1), Ok (Double num2)) -> Ok (Boolean (operation num1 num2))
            | (Error error as e, _) -> e
            | (_, (Error error as e)) -> e
            | _ -> Error (FatalError "Failed to apply comparison operation to values.")
        | (Error error as e, _) -> e
        | (_, (Error error as e)) -> e

let AreEqual (leftResult: EvaluationResult) (rightResult: EvaluationResult): EvaluationResult =
    match (leftResult, rightResult) with
        | (Error e as error, _) -> error
        | (_, (Error e as error) ) -> error
        | (Ok a, Ok b) ->
            match (a,b) with
                | (Double num1, Double num2) ->  Ok (Boolean (num1.Equals num2))
                | (Boolean b1, Boolean b2) -> Ok (Boolean (b1 = b2))
                | (Nil, Nil) -> Ok (Boolean true)
                | (String str1, String str2) -> Ok (Boolean (str1.Equals str2))
                | (Object b1, b2) -> Ok (Boolean (b1.Equals b1))
                | (b1, Object b2) -> Ok (Boolean (b2.Equals b1))
                | (v1, v2) -> Ok (Boolean (obj.ReferenceEquals(v1, v2)))

let NegateValue (result: EvaluationResult) : EvaluationResult =
    match result with
        | Ok nestedExprValue ->
            match nestedExprValue with
            | Double num -> Ok (Double -num)
            | Object object -> Ok (Double -(object :?> double))
            | Nil -> Error (ValueCastError "Cannot negate a value of type 'nil'.")
            | String _ -> Error (ValueCastError "Cannot negate a value of type 'string'.")
            | VOID -> Error (ValueCastError "Cannot negate a value of type 'void'.")
            | Boolean _ -> Error (ValueCastError "Cannot negate a value of type 'boolean'.")
        | error -> error

let InvertBoolean (result: EvaluationResult) : EvaluationResult =
    match result with
        | Ok (Boolean b) -> Ok (Boolean (not b))
        | Ok v2 -> 
            let typeName = v2.GetType().Name
            Error (ValueCastError (sprintf "Cannot invert value of type '%s'" typeName))
        | error -> error

let IsTruthy (value: FLOXValue): bool =
    match value with
    |   Object object -> 
            match object with
            | :? bool as boolVal -> boolVal
            | null -> false
            | _ -> false
    |   Nil -> false
    |   Boolean b -> b
    | _ -> true

let PlusImplementation (left: EvaluationResult) (right: EvaluationResult) =
    match (left, right) with
        | (Ok (String str1), Ok (String str2)) -> Ok (String (str1 + str2))
        | (Error _ as e1, _) -> e1
        | (_, (Error _ as e2)) -> e2
        | (left, right) -> CombineNumericValues left right (+)

let EvaluateUnary (evaluationFn: Expression -> EvaluationResult) (op: UnaryOperator) (expression: Expression) : EvaluationResult =
    match op with
        | UnaryOperator.MINUS ->
                let nestedExprEvaluation = evaluationFn expression
                NegateValue nestedExprEvaluation
        | UnaryOperator.BANG ->
                let nestedExprEvaluation = evaluationFn expression
                match nestedExprEvaluation with
                |   Ok value -> Ok (Boolean (IsTruthy value))
                |   error -> error
        | _ -> 
            assert false
            Error (FatalError "Invalid operator.")

let EvaluateLiteral (env: Environment) (literal: Literal) : EvaluationResult = 
    match literal with
        | NUMBER num -> Ok (Double num)
        | STRING str -> Ok (String str)
        | NIL -> Ok Nil
        | TRUEVAL -> Ok (Boolean true)
        | FALSEVAL -> Ok (Boolean false)
        | IDENTIFIER varName -> GetVariableValueOrError env (VarIdentifier varName)

let EvaluateBinary (evaluationFn: Expression -> EvaluationResult) (left: Expression) (op: BinaryOperator) (right: Expression) =
    match op with
        | BinaryOperator.PLUS -> PlusImplementation (evaluationFn left) (evaluationFn right)
        | BinaryOperator.MINUS -> CombineNumericValues (evaluationFn left) (evaluationFn right) (-)
        | BinaryOperator.MULT -> CombineNumericValues (evaluationFn left) (evaluationFn right) (*)
        | BinaryOperator.DIV -> CombineNumericValues (evaluationFn left) (evaluationFn right) (/)
        | BinaryOperator.LESS ->  CompareNumericValues (evaluationFn left) (evaluationFn right) (<)
        | BinaryOperator.LESSEQ ->  CompareNumericValues (evaluationFn left) (evaluationFn right) (<=)
        | BinaryOperator.GREATER ->  CompareNumericValues (evaluationFn left) (evaluationFn right) (>)
        | BinaryOperator.GREATEREQ ->  CompareNumericValues (evaluationFn left) (evaluationFn right) (>=)
        | BinaryOperator.EQ -> AreEqual (evaluationFn left) (evaluationFn right)
        | BinaryOperator.NEQ -> AreEqual (evaluationFn left) (evaluationFn right) |> InvertBoolean
        | BinaryOperator.INVALID -> 
            // should never happen
            assert false
            Error (FatalError "Invalid binary operator")

let rec EvaluateExpression (environment: Environment) (expression: Expression): EvaluationResult =
    match expression with
        | Literal lit -> EvaluateLiteral environment lit
        | Assign (identifier, expression) ->
            let result = EvaluateExpression environment expression

            match result with
            | Ok v -> 
                let variableWasSet = SetVariableValue environment  identifier v

                let VarIdentifier varName as v = identifier
                if variableWasSet then
                    result
                else
                    Error (DefaultUndefinedVariableError varName)
            | error -> error
        | Unary (op, expr) -> EvaluateUnary (EvaluateExpression environment) op expr
        | Grouping expr -> EvaluateExpression environment expr
        | BinaryExpression (left, op, right) -> EvaluateBinary (EvaluateExpression environment) left op right
        | Empty -> Ok VOID
        | Invalid ->
            // should be unreachable
            assert false
            Error (FatalError "Invalid value detected by runtime.")

let rec EvaluateBlock (parentEnvironment: Environment) (block: Statement): EvaluationResult =
    let newEnvironment = NewEnvironment (Some parentEnvironment)
    let Block declarations as b = block
    List.map (EvaluateDeclaration newEnvironment) declarations |> ignore
    Ok VOID

and EvaluateStatement (environment: Environment) (statement: Statement): EvaluationResult =
    match statement with
    | ExpressionStatement expression ->
        EvaluateExpression environment expression
    | PrintStatement expression ->
        let evalResult = EvaluateExpression environment expression

        match evalResult with
            | Ok v -> 
                printf "%s" (StringifyValue v) |> ignore
                Ok VOID
            | e -> e
    | IfStatement (predicate, trueStatement, elseStatement) ->
        let predicateEvaluation = EvaluateExpression environment predicate

        match predicateEvaluation with
            | Ok value ->
                if (IsTruthy value) then
                    EvaluateStatement environment trueStatement
                else
                    match elseStatement with
                        | Some statement -> EvaluateStatement environment statement
                        | None -> Ok VOID
            | error -> error
        
    | Block declarations as block -> EvaluateBlock environment block

and EvaluateDeclaration (environment: Environment) (declaration: Declaration): EvaluationResult =
    match declaration with
    | StatementDeclaration statement -> EvaluateStatement environment statement
    | VariableDeclaration (identifier, maybeExpression) ->
        let value = match maybeExpression with
            | Some expression -> EvaluateExpression environment expression
            | None -> Ok Nil

        match value with
        | Ok v -> 
            DefineVariable environment identifier v |> ignore
            value
        | error -> error

let GlobalEnvironment = NewEnvironment None

let ProgramEvaluate (env: Environment) (declarations: Declaration list): EvaluationResult list =
    List.map (EvaluateDeclaration env) declarations