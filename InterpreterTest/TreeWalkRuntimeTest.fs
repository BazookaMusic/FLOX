namespace TreeWalkRuntimeTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Tokens
open Parser
open Scanner
open AST
open TreeWalkRuntime
open RuntimeErrors
open RuntimeTypes
open Interpreter

[<TestClass>]
type TreeWalkRuntimeTest () =
    let ExpressionOrPanic result = match result with
    | ParseResult.Error (e, rest) -> 
        Assert.Fail (sprintf "%A" e)
        Expression.Invalid
    | ParseResult.Ok (expression, rest) ->
        match rest with
        | (h::t) when h.Type = EOF -> ()
        | _ -> Assert.Fail (sprintf "No tokens should remain but instead got '%A'" rest)
        expression

    let ResultsOrPanic result = match result with
    | ParseResult.Error (e, rest) -> 
        Assert.Fail (sprintf "%A" e)
        []
    | ParseResult.Ok (declarationList, rest) ->
        match rest with
        | (h::t) when h.Type = EOF -> ()
        | _ -> Assert.Fail (sprintf "No tokens should remain but instead got '%A'" rest)
        declarationList
    
    let AddBoilerPlate source = source 

    let ParseSource source = source |> AddBoilerPlate |> ScanTokens |> Seq.toList |> ParseExpression |> ExpressionOrPanic

    let ParseDeclaration source : Declaration list = source |> AddBoilerPlate |> ScanTokens |> ParseProgram |> ResultsOrPanic

    let rec getn n xs =
        match n, xs with
          | 0, (x::_)   -> x
          | _, (_::xs') -> getn (n - 1) xs'
          | _, []       -> invalidArg "n" "n is too large"

    let rec AssertEvaluationsMatch (sources: string list) (expected: EvaluationResult list) =
        let actual = List.map (fun s -> s |> ParseSource |> (EvaluateExpression GlobalEnvironment)) sources

        Assert.AreEqual (sources.Length, expected.Length)

        let actualExpectedPairs = List.zip expected actual

        let mutable mutableIndex = 0

        let equalityFun (ex,act) =
            Assert.AreEqual (ex, act, sprintf "The source '%s' was expected to evaluate to '%A' but instead evaluated to '%A'" (getn mutableIndex sources) ex act)
            mutableIndex <- mutableIndex + 1

        List.iter equalityFun actualExpectedPairs

    let rec AssertDeclarationEvaluationsMatch (sources: string list) (expected: EvaluationResult list list) =
        let actual = List.map (fun s -> s |> ParseDeclaration |> (List.map (EvaluateDeclaration GlobalEnvironment))) sources

        Assert.AreEqual (sources.Length, expected.Length, sprintf "Expected number of expected %d and number of actual results %d to match." expected.Length sources.Length)

        let actualExpectedPairs = List.zip expected actual

        let equalityFun (xs: EvaluationResult list, ys: EvaluationResult list) =
            Assert.AreEqual (xs.Length, ys.Length, sprintf "Different no of actual results then expected for expected:'%A' and actual: '%A'" ys xs)

            let innerEqualityFun (x: EvaluationResult list, y: EvaluationResult list) =
                Assert.AreEqual (x.Length, y.Length, sprintf "Different no of actual results then expected for expected:'%A' and actual: '%A'" y x)

                let innerActualExpectedPairs = List.zip y x

                let resultComparison (x, y) =
                    match (x,y) with
                    | (Error e1, Error e2) ->
                        Assert.AreEqual (e2.GetType(), e1.GetType())
                    | (v1, v2) -> Assert.AreEqual (v2,v1)

                List.iter resultComparison innerActualExpectedPairs

            List.iter innerEqualityFun actualExpectedPairs
        
        List.iter equalityFun actualExpectedPairs

    let rec AssertErrorTypesMatch (sources: string list) (expected: EvaluationResult list) =
        let actual = List.map (fun s -> s |> ParseSource |> (EvaluateExpression GlobalEnvironment)) sources

        Assert.AreEqual (sources.Length, expected.Length)

        let actualExpectedPairs = List.zip expected actual

        let mutable mutableIndex = 0

        let matchingErrorTypes (e1,e2) =
            match (e1,e2) with
                | (Error re1, Error re2) ->
                    let type1 = re1.GetType()
                    let type2 = re2.GetType()
                    Assert.AreEqual (type1, type2, sprintf "Evaluating the source '%s' was expected to evaluate to a runtime error of type '%s' but instead evaluated to '%s'" (getn mutableIndex sources) type1.Name type2.Name)
                    mutableIndex <- mutableIndex + 1
                | (e1,e2) -> 
                    Assert.Fail (sprintf "The source '%s' was expected to evaluate to an error but instead evaluated to '%A'" (getn mutableIndex sources) e2)

        List.iter matchingErrorTypes actualExpectedPairs

    

    [<TestMethod>]
    member this.LiteralEvaluationTest () =
        let sources = [
            "5"
            "\"hello\""
            "true"
            "false"
            "nil"
        ]

        let expected: EvaluationResult list = [
            Ok (Double 5.0)
            Ok (String "hello")
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Nil)
        ]

        AssertEvaluationsMatch sources expected

    [<TestMethod>]
    member this.ArithmeticEvaluationTest () =
        let sources = [
            "5+5"
            "5*5"
            "5*(5 + 5)"
            "\"hello\" + \"hello\""
            "5-5+5-5"
            "10*200/1000"
            "10 + (5+7) * 100 / 10"
            "10*(10.5 + 10.5) + 10/2 + ((3+2)*5 + (1.5 + 1.5) * 2)"
        ]

        let expected: EvaluationResult list = [
            Ok (Double 10.0)
            Ok (Double 25.0)
            Ok (Double 50.0)
            Ok (String "hellohello")
            Ok (Double 0.0)
            Ok (Double 2.0)
            Ok (Double 130.0)
            Ok (Double 246.0)
        ]

        AssertEvaluationsMatch sources expected

    [<TestMethod>]
    member this.EqualityEvaluationTest () =
        let sources = [
            "5==5"
            "5!=5"
            "5 == 1 + 4"
            "\"hello\" == \"hello\""
            "5 == 100 / 20"
            "5 == (1000 - 999)*5"
            "5 != (1000 - 999)*5"
            "6 != (1000 - 999)*5"
            "6 == (1000 - 999)*5"
            "\"hello\" + \"hello\" != \"goodbye\""
        ]

        let expected: EvaluationResult list = [
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Boolean true)
        ]

        AssertEvaluationsMatch sources expected

    [<TestMethod>]
    member this.ComparisonEvaluationTest () =
        let sources = [
            "5 > 6"
            "6 > 5"
            "5 <= 6"
            "5 <= 5"
            "1000 + 1 < 1000 + 2"
            "1*5*6 < 2*5*6"
            "1 / 12 + 1 < 3"
            "(5+5)*3 + 4 < (6*5)*3 + 4"
            "(5+5)*3 + 4 > (6*5)*3 + 4"
            "(5+5)*3 + 4 >= (6*5)*3 + 4"
        ]

        let expected: EvaluationResult list = [
            Ok (Boolean false)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Boolean false)
        ]

        AssertEvaluationsMatch sources expected

    [<TestMethod>]
    member this.AndOrTest () =
        let sources = [
            "false and false"
            "false and true"
            "true and false"
            "true and true"
            "false or false"
            "false or true"
            "true or false"
            "true or true"
        ]

        let expected: EvaluationResult list = [
            Ok (Boolean false)
            Ok (Boolean false)
            Ok (Boolean false)
            Ok (Boolean true)
            Ok (Boolean false)
            Ok (Boolean true)
            Ok (Boolean true)
            Ok (Boolean true)
        ]

        AssertEvaluationsMatch sources expected

        let declarations = [
            // a should remain at 5 due to short circuit
            "var a = 5; var b = false and (a = 6 and false); a;"
            "var a = 5; var b = true or ((a = 6) and false); a;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 5.0); Ok (Boolean false); Ok (Double 5.0);]
            [Ok (Double 5.0); Ok (Boolean true); Ok (Double 5.0);]
        ]

        AssertDeclarationEvaluationsMatch declarations expected

    [<TestMethod>]
    member this.RuntimeErrorTest () =
        let sources = [
            "5 + \"hello\""
            "true + 5"
            "nil + 5"
            "\"hello\" + nil"
        ]

        let expected: EvaluationResult list = [
            Error (DefaultCastError "" "")
            Error (DefaultCastError "" "")
            Error (DefaultNullRef)
            Error (DefaultCastError "" "")
        ]

        AssertErrorTypesMatch sources expected

    [<TestMethod>]
    member this.VariableTest () =
        let sources = [
            "var peopleAmount = 14; peopleAmount + 1; peopleAmount * 1; 1024 * 2;"
            "var boys = \"boys\"; boys + \" and girls\";"
            "var hello = (1996 + 1 - 1 + 2 - 2) * (1024 / 1024); hello;"
            "var hello = 2048; var hello = 2019; var hello = 2024;"
            "var goodbye = 1234; goodbye = goodbye + 1; goodbye = goodbye + 1;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 14.0); Ok (Double 15.0); Ok (Double 14.0); Ok (Double 2048.0)]
            [Ok (String "boys"); Ok (String "boys and girls");]
            [Ok (Double 1996.0); Ok (Double 1996.0)]
            [Ok (Double 2048.0); Ok (Double 2019.0); Ok (Double 2024.0)]
            [Ok (Double 1234.0); Ok (Double 1235.0); Ok (Double 1236.0)]
        ]

        AssertDeclarationEvaluationsMatch sources expected

    [<TestMethod>]
    member this.VariableErrorsTest () =
        let sources = [
            "var peopleAmount = 14; peopleamount + 1;"
            "var peopleAmount = 14; peopleAmount + \"egg\";"
            "var peopleAmount = nil; peopleAmount + 1;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 14.0); Error (UndefinedVariableError "undefined");]
            [Ok (Double 14.0); Error (ValueCastError "cast");]
            [Ok Nil; Error (NullReferenceError "cast")]
        ]

        AssertDeclarationEvaluationsMatch sources expected

    [<TestMethod>]
    member this.BlockTest() =
        let sources = [
            "var peopleAmount = 14; { var hello = 1; var hello = 2; }"
            // change in block
            "var mutableInBlock = 10; { mutableInBlock = mutableInBlock - 1; mutableInBlock = mutableInBlock - 1; } mutableInBlock;"
            // shadowing
            "var mutableInBlock = 10; { var mutableInBlock = 1024; mutableInBlock = mutableInBlock - 1; } mutableInBlock;"
            // shadowning and use existing variable
            "var mutableInBlock = 10; { var mutableInBlock = mutableInBlock - 1; mutableInBlock = mutableInBlock - 1; } mutableInBlock;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 14.0); Ok VOID]
            [Ok (Double 10.0); Ok VOID; Ok (Double 8.0)]
            [Ok (Double 10.0); Ok VOID; Ok (Double 10.0)]
            [Ok (Double 10.0); Ok VOID; Ok (Double 10.0)]
        ]

        AssertDeclarationEvaluationsMatch sources expected

    [<TestMethod>]
    member this.IfTest() =
        let sources = [
            "if (true) 1; else 5;"
            "if (false) 1; else 5;"
            "var a = true; if (a) 5; else 6;"
            "if (1 + 2 + 3 + 5 < 1 + 2 + 1000) 1; else 2; "
            "if (true) print \"hello\";"
            "if (true) if (false) 1.0; else 2.0; else 3.0;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 1.0)]
            [Ok (Double 5.0)]
            [Ok (Boolean true); Ok (Double 5.0)]
            [Ok (Double 1.0)]
            [Ok VOID]
            [Ok (Double 2.0)]
        ]

        AssertDeclarationEvaluationsMatch sources expected

    [<TestMethod>]
    member this.WhileTest() =
        let sources = [
            "var a = 0; while (a < 100) { a = a + 1; } a;"
            "var a = 0; while (false) { a = a + 1; } a;"
        ]

        let expected: EvaluationResult list list = [  
            [Ok (Double 0.0); Ok VOID; Ok (Double 100.0)]
            [Ok (Double 0.0); Ok VOID; Ok (Double 0.0)]
        ]

        AssertDeclarationEvaluationsMatch sources expected

    // TODOS: 
    // 1) Add tests for a mix of expressions
    // 2) Add better coverage for runtime errors

    
