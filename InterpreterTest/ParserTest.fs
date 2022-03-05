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

    let rec ParsedSourceEqualsExpectedTree sources expected =
        match (sources, expected) with
            | (s::st, e::et) ->
                let parsedSource = ParseSource s
                match parsedSource with
                    | Ok (declarations, [h]) when h.Type = EOF -> Assert.AreEqual (declarations, e)
                    | Ok (declarations, (h::t)) ->
                        Assert.Fail (sprintf "Non-parsed tokens: %A" (h::t))
                    | Ok (declarations, []) ->
                        Assert.Fail ("Unexpected end of tokens")
                    | e -> Assert.AreEqual (parsedSource, expected)
                ParsedSourceEqualsExpectedTree st et
            | ([], (e::et)) ->
                Assert.Fail (sprintf "Found no source for %A" e)
            | ((s::st), ([])) ->
                Assert.Fail (sprintf "Found no expected value for for '%s'" s)
            | ([], []) -> ()

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
        
        let expectedTree = Expression.Literal (Literal.IDENTIFIER (VarIdentifier "identifier1"))
        
        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

    [<TestMethod>]
    member this.FunctionCallParseTest () =
        let source = "ignite()"

        let expectedTree = Expression.Call (Expression.Literal (Literal.IDENTIFIER (VarIdentifier "ignite")), [])

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "transmogrify(\"gold\", \"silver\", 1024.0)"

        let expected = Call (Literal (Literal.IDENTIFIER (VarIdentifier "transmogrify")), [Literal (Literal.STRING "gold"); Literal (Literal.STRING "silver"); Literal (Literal.NUMBER 1024.0)])
        let actual = ParseExpressionSource source
        AssertExpressionTreesEqual expected actual

        let complexSource = "foo(4 * pow(4,2), \"foo\" + barfun())"
        let stringFun = BinaryExpression (Literal (Literal.STRING "foo"), BinaryOperator.PLUS, Call (Literal (Literal.IDENTIFIER (VarIdentifier "barfun")), []))
        let numFun = BinaryExpression (Literal (Literal.NUMBER 4.0), BinaryOperator.MULT, Call (Literal (Literal.IDENTIFIER (VarIdentifier "pow")), [Literal (Literal.NUMBER 4.0); Literal (Literal.NUMBER 2.0)]))
        let expected = Call (Literal (Literal.IDENTIFIER (VarIdentifier "foo")), [numFun ; stringFun])

        AssertExpressionTreesEqual expected (ParseExpressionSource complexSource)

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
    member this.AndOrParseTest () =
        let source = "true and false"
        let trueLiteral = Expression.Literal (Literal.TRUEVAL)
        let falseLiteral = Expression.Literal (Literal.FALSEVAL)
        let expectedTree = Expression.BinaryExpression (trueLiteral, BinaryOperator.AND, falseLiteral)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

        let source = "true or false"
        let trueLiteral = Expression.Literal (Literal.TRUEVAL)
        let falseLiteral = Expression.Literal (Literal.FALSEVAL)
        let expectedTree = Expression.BinaryExpression (trueLiteral, BinaryOperator.OR, falseLiteral)

        AssertExpressionTreesEqual expectedTree (ParseExpressionSource source)

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

        let source = "5 == 5 or 5 == 5"
        let eqExpression = Expression.BinaryExpression (numberLiteral5, BinaryOperator.EQ, numberLiteral5)
        let expectedTree = Expression.BinaryExpression (eqExpression, BinaryOperator.OR, eqExpression)

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
    member this.BlockDeclarationParseTest () =
        let source = "var a = 0; { var eggman = 23; var eggwoman = \"hello\"; var empty; } print a;";

        let declarations = source |> ParseSource |> UnwrapListOrPanic

        Assert.AreEqual (3, declarations.Length, "Expected 3 declarations")

        let eggman = VariableDeclaration (VarIdentifier "eggman", Some (Expression.Literal (Literal.NUMBER 23.0) ))
        let eggwoman = VariableDeclaration (VarIdentifier "eggwoman", Some (Expression.Literal (Literal.STRING "hello") ))
        let empty = VariableDeclaration (VarIdentifier "empty", None )

        let block = StatementDeclaration (Block [eggman; eggwoman; empty])

        let alpha = VariableDeclaration (VarIdentifier "a", Some (Expression.Literal (Literal.NUMBER 0.0) ))
        let printAlpha = StatementDeclaration (PrintStatement (Expression.Literal (Literal.IDENTIFIER (VarIdentifier "a")) ))

        let expectedDeclarations = [alpha; block; printAlpha] 

        Assert.AreEqual (expectedDeclarations, declarations)

    [<TestMethod>]
    member this.FunctionDeclarationParseTest () =
        let sources = [
            "fun foo(bar, boo, ni) { print 5; }";
            "fun foo(bar, boo, ni) { }";
            "fun foo() { print 5; }";
        ]

        let body = Statement.Block ([StatementDeclaration (Statement.PrintStatement (Expression.Literal (Literal.NUMBER 5.0)))])
        let emptyBody = Statement.Block([])

        let arguments = [VarIdentifier "bar"; VarIdentifier "boo"; VarIdentifier "ni"]

        let funDecl = FunctionDeclaration (VarIdentifier "foo", arguments, body)
        let funDeclEmptyBody = FunctionDeclaration (VarIdentifier "foo", arguments, emptyBody)
        let funDeclEmptyArguments =  FunctionDeclaration (VarIdentifier "foo", [], body)

        let expected = [
            [funDecl]
            [funDeclEmptyBody]
            [funDeclEmptyArguments]
        ]

        ParsedSourceEqualsExpectedTree sources expected

    [<TestMethod>]
    member this.FunctionWithReturnParseTest () =
        let sources = [
            "fun foo(bar, boo, ni) { return 5; }";
            "fun foo(bar, boo, ni) { if (false) return 4; else return 5;}";
        ]

        let singleReturnBody = Statement.Block ([StatementDeclaration (Statement.ReturnStatement (Expression.Literal (Literal.NUMBER 5.0)))])

        let ifWithTwoReturnsBody = Statement.Block [StatementDeclaration (IfStatement (Literal FALSEVAL, ReturnStatement (Literal (Literal.NUMBER 4.0)), Some (ReturnStatement (Literal (Literal.NUMBER 5.0))))) ]

        let arguments = [VarIdentifier "bar"; VarIdentifier "boo"; VarIdentifier "ni"]

        let singleReturnFunctionDecl = FunctionDeclaration (VarIdentifier "foo", arguments, singleReturnBody)

        let twoReturnsWithIfFunctionDecl = FunctionDeclaration (VarIdentifier "foo", arguments, ifWithTwoReturnsBody)

        let expected = [
            [singleReturnFunctionDecl]
            [twoReturnsWithIfFunctionDecl]
        ]

        ParsedSourceEqualsExpectedTree sources expected

    [<TestMethod>]
    member this.IfStatementParseTest() =
        let source = "if (true) 5; else 6;";

        let declarations = source |> ParseSource |> UnwrapListOrPanic

        Assert.AreEqual (1, declarations.Length, "Expected 1 declaration")

        let predicate = Literal Literal.TRUEVAL
        let trueStatement =  ExpressionStatement (Literal (Literal.NUMBER 5.0))
        let elseStatement = ExpressionStatement (Literal (Literal.NUMBER 6.0))

        let ifElseStatement = IfStatement (predicate, trueStatement, Some elseStatement)

        let expected = [StatementDeclaration ifElseStatement]

        Assert.AreEqual (expected, declarations)

        let source = "if (true) 5;";
        
        let declarations = source |> ParseSource |> UnwrapListOrPanic
        
        Assert.AreEqual (1, declarations.Length, "Expected 1 declaration")
        
        let predicate = Literal Literal.TRUEVAL
        let trueStatement =  ExpressionStatement (Literal (Literal.NUMBER 5.0))
        
        let ifStatement = IfStatement (predicate, trueStatement, None)

        let expected = [StatementDeclaration ifStatement]

        Assert.AreEqual (expected, declarations)

        // nested if-else

        let source = "if (true) \n if (true) { 5; } else { 6; } else { 7; }";
        
        let declarations = source |> ParseSource |> UnwrapListOrPanic
        
        Assert.AreEqual (1, declarations.Length, "Expected 1 declaration")
        
        let expected = [
            StatementDeclaration (IfStatement
                (Literal TRUEVAL,
                 IfStatement
                   (Literal TRUEVAL,
                    Block
                      [StatementDeclaration (ExpressionStatement (Literal (Literal.NUMBER 5.0)))],
                    Some
                      (Block
                         [StatementDeclaration (ExpressionStatement (Literal (Literal.NUMBER 6.0)))])),
                 Some
                   (Block
                      [StatementDeclaration (ExpressionStatement (Literal (Literal.NUMBER 7.0)))])))
                  ]

        Assert.AreEqual (expected, declarations)

    [<TestMethod>]
    member this.WhileStatementParseTest() =
        let source = "while (true) 5;";

        let declarations = source |> ParseSource |> UnwrapListOrPanic

        Assert.AreEqual (1, declarations.Length, "Expected 1 declaration")

        let predicate = Literal Literal.TRUEVAL
        let statement =  ExpressionStatement (Literal (Literal.NUMBER 5.0))

        let whileStatement = WhileStatement (predicate, statement)

        let expected = [StatementDeclaration whileStatement]

        Assert.AreEqual (expected, declarations)

        let source = "var a = 0; while (a < 5) { a = a + 1;} a;";
        
        let declarations = source |> ParseSource |> UnwrapListOrPanic
        
        Assert.AreEqual (3, declarations.Length, "Expected 1 declaration")
        
        let expected = [
            VariableDeclaration (VarIdentifier "a", Some (Literal (Literal.NUMBER 0.0))); 
            StatementDeclaration (WhileStatement
               (BinaryExpression (Literal (Literal.IDENTIFIER (VarIdentifier "a")), BinaryOperator.LESS, Literal (Literal.NUMBER 5.0)),
                Block
                  [StatementDeclaration
                     (ExpressionStatement
                        (Assign
                           (VarIdentifier "a",
                            BinaryExpression
                              (Literal (Literal.IDENTIFIER (VarIdentifier "a")), BinaryOperator.PLUS, Literal (Literal.NUMBER 1.0)))))]));
            StatementDeclaration (ExpressionStatement (Literal (Literal.IDENTIFIER (VarIdentifier "a"))))
            ]

        Assert.AreEqual (expected, declarations)

    [<TestMethod>]
    member this.ForStatementParseTest() =
        let sources = [
            "for (;;;) 5;"
            "for (var a = 5;;;) 5;"
            "for (;5 < 5;;) 5;"
            "for (;;a = 5;) 5;"
            "for (var a = 5;5 < 5; a = 5;) 5;"
            // nested for
            "for (var a = 5;5 < 5; a = 5;) for (var a = 5;5 < 5; a = 5;) 5;"
        ]
        
        let fiveLiteral = Expression.Literal (Literal.NUMBER 5.0)

        let defaultVarDecl = Some (VariableDeclaration (VarIdentifier "a", Some (Expression.Literal (Literal.NUMBER 5.0))))
        let defaultCond = Some (ExpressionStatement (BinaryExpression (fiveLiteral, BinaryOperator.LESS, fiveLiteral)))
        let defaultFinal = Some (ExpressionStatement (Assign (VarIdentifier "a", fiveLiteral)))
        let fullFor = ForStatement (defaultVarDecl, defaultCond, defaultFinal, ExpressionStatement fiveLiteral)
        let expectedValues = [
            [StatementDeclaration (ForStatement (None, None, None, ExpressionStatement fiveLiteral))]
            [StatementDeclaration (ForStatement (defaultVarDecl, None, None, ExpressionStatement fiveLiteral))]
            [StatementDeclaration (ForStatement (None, defaultCond, None, ExpressionStatement fiveLiteral))]
            [StatementDeclaration (ForStatement (None, None, defaultFinal, ExpressionStatement fiveLiteral))]
            [StatementDeclaration (ForStatement (defaultVarDecl, defaultCond, defaultFinal, ExpressionStatement fiveLiteral))]
            [StatementDeclaration (ForStatement (defaultVarDecl, defaultCond, defaultFinal, fullFor))]
        ]

        ParsedSourceEqualsExpectedTree sources expectedValues

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



        

        








