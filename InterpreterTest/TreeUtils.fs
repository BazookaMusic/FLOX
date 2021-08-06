module TreeUtils

open Parser
open AST
open Microsoft.VisualStudio.TestTools.UnitTesting

let rec AssertExpressionTreesEqual (expr1: Expression) (expr2: Expression) =
    match (expr1, expr2) with
        | (Invalid, Invalid) -> Assert.Fail "Invalid parsing detected."
        | (Empty, Empty) -> ()
        | (BinaryExpression (left1, op1, right1), BinaryExpression (left2, op2, right2)) ->
            Assert.AreEqual (op1, op2)
            AssertExpressionTreesEqual left1 left2
            AssertExpressionTreesEqual right1 right2
        | (Grouping g1, Grouping g2) -> AssertExpressionTreesEqual g1 g2
        | (Unary (op1,u1), Unary (op2,u2)) ->
            Assert.AreEqual (op1, op2)
            AssertExpressionTreesEqual u1 u2
        | (Literal lit1, Literal lit2) ->
            Assert.AreEqual (lit1, lit2)
        | (a,b) -> Assert.Fail (sprintf "'%A' and '%A' are not equal as expected." a b)