namespace ParserTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Scanner
open Parser
open TreeUtils
open AST
open Tokens

[<TestClass>]
type ParserTest () =
    let UnwrapExpressionOrPanic result = match result with
    | Error (e, rest) -> 
        Assert.Fail (sprintf "%A" e)
        Invalid
    | Ok (item, rest) ->
        match rest with
        | (h::t) when h.Type = EOF -> ()
        | _ -> Assert.Fail (sprintf "No tokens should remain but instead got '%A'" rest)

        
        item

    let UnwrapListOrPanic result = match result with
    | Error (e, rest) -> 
        Assert.Fail (sprintf "%A" e)
        []
    | Ok (statements, rest) ->
        match rest with
        | (h::t) when h.Type = EOF -> ()
        | _ -> Assert.Fail (sprintf "No tokens should remain but instead got '%A'" rest)

        statements

    let AddBoilerPlate source = source 
    let ParsingPipeline source = source |> ScanTokens |> ParseProgram
    let ParsingPipelineExpression source = source |> ScanTokens |> Seq.toList |> ParseExpression |> UnwrapExpressionOrPanic

    let ParseExpressionSource (source: string) = source |> AddBoilerPlate |> ParsingPipelineExpression
    let ParseSource (source: string) = source |> AddBoilerPlate |> ParsingPipeline
    let ParseSourceRaw source = source |> ParsingPipeline

    [<TestMethod>]
    member this.LiteralParseTest () =
        let source = "5"

        let expectedTree = Expression.Literal (Literal.NUMBER 5.0)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "\"HELLO\""
        
        let expectedTree = Expression.Literal (Literal.STRING "HELLO")
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "nil"
        
        let expectedTree = Expression.Literal (Literal.NIL)
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "true"
        
        let expectedTree = Expression.Literal (Literal.TRUEVAL)
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "false"
        
        let expectedTree = Expression.Literal (Literal.FALSEVAL)
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "identifier1"
        
        let expectedTree = Expression.Literal (Literal.IDENTIFIER "identifier1")
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

    [<TestMethod>]
    member this.UnaryParseTest () =
        let source = "-5"

        let expectedTree = Expression.Unary (UnaryOperator.MINUS, Expression.Literal (Literal.NUMBER 5.0))

        let source = "-5"
        let expectedTree = Expression.Unary (UnaryOperator.MINUS, Expression.Literal (Literal.NUMBER 5.0))

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

    [<TestMethod>]
    member this.FactorParseTest () =
        let source = "5 * 5"
        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, numberLiteral5)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 * 7 * 6 / 5"

        let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)
        let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)

        let nest1 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.DIV, numberLiteral5)
        let nest2 = Expression.BinaryExpression (numberLiteral7, BinaryOperator.MULT, nest1)
        let nest3 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, nest2)

        let actualTree = ParseExpressionSource source
        AssertExpressionTreesEqual nest3 (actualTree)

    [<TestMethod>]
       member this.TermParseTest () =
           let source = "5 + 5"
           let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
           let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.PLUS, numberLiteral5)

           AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

           let source = "5 + 7 - 6 + 5"

           let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)
           let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)

           let nest1 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.PLUS, numberLiteral5)
           let nest2 = Expression.BinaryExpression (numberLiteral7, BinaryOperator.MINUS, nest1)
           let nest3 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.PLUS, nest2)

           let actualTree = ParseExpressionSource source
           AssertExpressionTreesEqual nest3 (actualTree)

    [<TestMethod>]
    member this.ComparisonParseTest () =
        let source = "5 > 5"
        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.GREATER, numberLiteral5)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 >= 7 < 6 >= 5 > 5 <= 6 > 5"

        let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)
        let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)

        let nest1 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.GREATER, numberLiteral5)
        let nest2 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.LESSEQ, nest1)
        let nest3 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.GREATER, nest2)
        let nest4 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.GREATEREQ, nest3)
        let nest5 = Expression.BinaryExpression (numberLiteral7, BinaryOperator.LESS, nest4)
        let nest6 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.GREATEREQ, nest5)

        let actualTree = ParseExpressionSource source
        AssertExpressionTreesEqual nest6 (actualTree)

    [<TestMethod>]
    member this.EqualityParseTest () =
        let source = "5 == 5"
        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, numberLiteral5)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 != 7 == 6 != 5 == 5 == 6 != 5"

        let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)
        let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)

        let nest1 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.NEQ, numberLiteral5)
        let nest2 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, nest1)
        let nest3 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, nest2)
        let nest4 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.NEQ, nest3)
        let nest5 = Expression.BinaryExpression (numberLiteral7, BinaryOperator.EQ, nest4)
        let nest6 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.NEQ, nest5)

        let actualTree = ParseExpressionSource source
        AssertExpressionTreesEqual nest6 (actualTree)

    [<TestMethod>]
    member this.GroupingParseTest () =
        let source = "(5 != 7 == 6 != 5 == 5 == 6 != 5)"

        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)
        let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)

        let nest1 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.NEQ, numberLiteral5)
        let nest2 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, nest1)
        let nest3 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, nest2)
        let nest4 = Expression.BinaryExpression (numberLiteral6, BinaryOperator.NEQ, nest3)
        let nest5 = Expression.BinaryExpression (numberLiteral7, BinaryOperator.EQ, nest4)
        let nest6 = Expression.BinaryExpression (numberLiteral5, BinaryOperator.NEQ, nest5)

        let expectedTree = Expression.Grouping nest6

        let actualTree = ParseExpressionSource source
        AssertExpressionTreesEqual expectedTree (actualTree)

    [<TestMethod>]
    member this.PrecedenceTest () =
        let source = "5 * 5 + 5"
        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let multExpression = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, numberLiteral5)

        let expectedTree = Expression.BinaryExpression (multExpression, BinaryOperator.PLUS, numberLiteral5)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 * (5 + 5)"
        let grouping = Expression.Grouping (Expression.BinaryExpression (numberLiteral5, BinaryOperator.PLUS, numberLiteral5))
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, grouping)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 > 5 * 5"
        let multExpression = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, numberLiteral5)
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.GREATER, multExpression)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "5 == 5 * 5"
        let multExpression = Expression.BinaryExpression (numberLiteral5, BinaryOperator.MULT, numberLiteral5)
        let expectedTree = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, multExpression)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

    [<TestMethod>]
    member this.StatementParseTest () =
        let source = "5 + 6 + 7; 5 + 6 + 7; \"hello\" + \"hello\"; print 5 + 5;"

        let statements = source |> ParseSource |> UnwrapListOrPanic
        let statementFromDeclaration declaration =
            match declaration with
            | StatementDeclaration statement -> statement
            | _ -> 
                Assert.Fail "Expected declaration but got statement"
                ExpressionStatement (Expression.Empty)

        let expressionFromStatement stmt =
            match stmt with
            | ExpressionStatement s -> s
            | PrintStatement s -> s

        let actualExpressions = List.map (statementFromDeclaration >> expressionFromStatement) statements

        let numberLiteral5 = Expression.Literal (Literal.NUMBER 5.0)
        let numberLiteral6 = Expression.Literal (Literal.NUMBER 6.0)
        let numberLiteral7 = Expression.Literal (Literal.NUMBER 7.0)

        let strLiteral = Expression.Literal (Literal.STRING "hello")

        let numeric = Expression.BinaryExpression (numberLiteral5, BinaryOperator.PLUS, Expression.BinaryExpression (numberLiteral6, BinaryOperator.PLUS, numberLiteral7))
        let stringAdd = Expression.BinaryExpression (strLiteral, BinaryOperator.PLUS, strLiteral)
        let printStmt = Expression.BinaryExpression (numberLiteral5, BinaryOperator.PLUS, numberLiteral5)

        let expectedExpressions = [numeric; numeric; stringAdd; printStmt]

        let expectedActual = List.zip expectedExpressions actualExpressions

        List.iter (fun (x,y) -> AssertExpressionTreesEqual x y) expectedActual

    [<TestMethod>]
    member this.VariableDeclarationParseTest () =
        let source = "var eggman = 23; var eggwoman = \"hello\"; var empty;";

        let declarations = source |> ParseSource |> UnwrapListOrPanic

        Assert.AreEqual (3, declarations.Length, "Expected 2 declarations")

        let eggman = VariableDeclaration (VarIdentifier "eggman", Some (Expression.Literal (Literal.NUMBER 23.0) ))
        let eggwoman = VariableDeclaration (VarIdentifier "eggwoman", Some (Expression.Literal (Literal.STRING "hello") ))
        let empty = VariableDeclaration (VarIdentifier "empty", None )

        let [actualEggman; actualEggwoman; actualEmpty] = declarations

        Assert.AreEqual (eggman, actualEggman)
        Assert.AreEqual (eggwoman, actualEggwoman)
        Assert.AreEqual (empty, actualEmpty)

    [<TestMethod>]
    member this.MixedDeclarationParseTest () =
        let source = "var eggman = 23; 1 + 2; 12 / 2; var empty;";

        let declarations = source |> ParseSource |> UnwrapListOrPanic

        Assert.AreEqual (4, declarations.Length, "Expected 2 declarations")

        let numLiteral x = Expression.Literal (Literal.NUMBER x)
        let eggman = VariableDeclaration (VarIdentifier "eggman", Some (numLiteral 23.0 ))
        let sum = StatementDeclaration (ExpressionStatement (Expression.BinaryExpression ( numLiteral 1.0, BinaryOperator.PLUS, numLiteral 2.0)))
        let division = StatementDeclaration (ExpressionStatement (Expression.BinaryExpression ( numLiteral 12.0, BinaryOperator.DIV, numLiteral 2.0)))
        let empty = VariableDeclaration (VarIdentifier "empty", None )

        let [actualEggman; actualSum; actualDivision; actualEmpty] = declarations

        Assert.AreEqual (eggman, actualEggman)
        Assert.AreEqual (sum, actualSum)
        Assert.AreEqual (division, actualDivision)
        Assert.AreEqual (empty, actualEmpty)



        

        








