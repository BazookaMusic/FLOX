namespace ScannerTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open Scanner
open Utilities
open Tokens

[<TestClass>]
type ScannerTests () =

    let fail = fun (a,b) -> Assert.Fail (sprintf "%A and %A are not equal." a b)
    let comparison (a,b) = Assert.AreEqual (a,b)

    [<TestMethod>]
    member this.IdentifierScanningTest() =
        let source = "hello world HELLO WORLD HELLO1 HELLO2 HELLO3   HELLO4"
        let tokens = ScanTokens source
        Assert.AreEqual (9, tokens.Count)

        let expectedTokens = [IDENTIFIER; IDENTIFIER; IDENTIFIER; IDENTIFIER; IDENTIFIER; IDENTIFIER; IDENTIFIER; IDENTIFIER; EOF]
        let tokenTypes = Seq.toList tokens |> List.map (fun (item: ScannerToken) -> item.Type)

        AssertSequenceComparison (expectedTokens, tokenTypes, comparison, fail)

        let lexemes = Seq.toList tokens |> List.map (fun (item: ScannerToken) -> item.Lexeme)
        let expectedLexemes = ["hello"; "world"; "HELLO"; "WORLD"; "HELLO1"; "HELLO2"; "HELLO3"; "HELLO4"; ""]

        AssertSequenceComparison (expectedLexemes, lexemes, comparison, fail)

    [<TestMethod>]
    member this.NumberScanningTest() =
        let tokens = ScanTokens "3.14159 0 12 123456 10000000.23456";
        Assert.AreEqual (6, tokens.Count)

        let expectedTokens = [NUMBER; NUMBER; NUMBER; NUMBER; NUMBER; EOF]
        let tokenTypes = Seq.toList tokens |> List.map (fun (item: ScannerToken) -> item.Type)

        AssertSequenceComparison (expectedTokens, tokenTypes, comparison, fail)

        let expectedNumbers: obj list = [3.14159; 0; 12; 123456; 10000000; 10000000.23456]
        AssertSequenceComparison (expectedTokens, tokenTypes, comparison, fail)

    [<TestMethod>]
    member this.CommentScanningTest() =
        let tokens = ScanTokens "// This is a nasty comment some politician wrote to annoy people \n ()";
        Assert.AreEqual (3, tokens.Count)

        let token = tokens.[0]
        let expectedToken: ScannerToken = {Type = LEFT_PAREN; Lexeme = "(";  Literal = null; Line = 2}
        Assert.AreEqual (expectedToken, token)

        let token = tokens.[1]
        let expectedToken: ScannerToken = {Type = RIGHT_PAREN; Lexeme = ")";  Literal = null; Line = 2}
        Assert.AreEqual (expectedToken, token)

    [<TestMethod>]
    member this.StringScanningTest() =
        let tokens = ScanTokens "\"\""
        Assert.AreEqual (2, tokens.Count)
        let token = tokens.[0]
        let expectedToken: ScannerToken = {Type = STRING; Lexeme = "\"\"";  Literal = ""; Line = 1}
        Assert.AreEqual (expectedToken, token)

        let tokens = ScanTokens "\" THIS IS A STRING \""
        Assert.AreEqual (2, tokens.Count)

        let token = tokens.[0]
        let expectedToken: ScannerToken = {Type = STRING; Lexeme = "\" THIS IS A STRING \"";  Literal = " THIS IS A STRING "; Line = 1}
        Assert.AreEqual (expectedToken, token)

    [<TestMethod>]
    member this.StringScanningWithNewlineTest() =
        let tokens = ScanTokens "\" THIS IS A STRING \n \n \n ANOTHER STRING \" ("
        Assert.AreEqual (3, tokens.Count)

        let token = tokens.[0]
        let expectedToken: ScannerToken = {Type = STRING; Lexeme = "\" THIS IS A STRING \n \n \n ANOTHER STRING \"";  Literal = " THIS IS A STRING \n \n \n ANOTHER STRING "; Line = 1}
        Assert.AreEqual (expectedToken, token)

        let token = tokens.[1]
        let expectedToken: ScannerToken = {Type = LEFT_PAREN; Lexeme = "(";  Literal = null; Line = 4}
        Assert.AreEqual (expectedToken, token)

    [<TestMethod>]
    member this.SimpleScanTest() =
        let tokens = Seq.toList (ScanTokens "(){}!!=><>=<=+-/*@*")
        let tokenTypes = List.map (fun (item: ScannerToken) -> item.Type)  tokens
        let expectedTokens = [LEFT_PAREN; RIGHT_PAREN; LEFT_BRACE; RIGHT_BRACE; BANG; BANG_EQUAL; GREATER; LESS; GREATER_EQUAL; LESS_EQUAL; PLUS; MINUS; SLASH; STAR; UNKNOWN; STAR; EOF]

        AssertSequenceComparison (expectedTokens, tokenTypes, comparison, fail)


    [<TestMethod>]
    member this.ComplexScanTest() =
           let source = "while(1) \n { \n a = 1024 \n b = 1234.5 \n c = a + b \n if (c >= 1523) return 15 \n}"

           let tokens = ScanTokens source
           let tokenTypes = Seq.toList tokens |> List.map (fun (item: ScannerToken) -> item.Type)
           let tokenLexemes = Seq.toList tokens |> List.map (fun (item: ScannerToken) -> item.Lexeme)

           let expectedTokenTypes = [
                                        WHILE; LEFT_PAREN; NUMBER; RIGHT_PAREN; 
                                        LEFT_BRACE; 
                                             IDENTIFIER; EQUAL; NUMBER; 
                                             IDENTIFIER; EQUAL; NUMBER; 
                                             IDENTIFIER; EQUAL; IDENTIFIER; PLUS; IDENTIFIER;
                                             IF; LEFT_PAREN; IDENTIFIER; GREATER_EQUAL; NUMBER; RIGHT_PAREN;
                                             RETURN; NUMBER; 
                                        RIGHT_BRACE; 
                                        EOF;
                                     ]
           let expectedTokenLexemes = ["while"; "("; "1"; ")"; 
               "{"; 
               "a"; "="; "1024"; 
               "b"; "="; "1234.5"; 
               "c"; "="; "a"; "+"; "b";
               "if"; "("; "c"; ">="; "1523"; ")";
               "return"; "15";
               "}";""]

           AssertSequenceComparison (expectedTokenTypes, tokenTypes , comparison, fail)
           AssertSequenceComparison (expectedTokenLexemes, tokenLexemes, comparison, fail)


