package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.parsing.ParserFacade
import de.hhu.stups.alloy2b.ast.Operator.*
import org.junit.Assert.assertEquals
import java.util.Arrays.asList
import org.junit.Test as test

class PrecedencesTest {
    @test
    fun boxJoinVsDotJoin() {
        val code = "sig test {} {a.b[c]}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = AlloySpecification(declarations=asList(SignatureDeclaration(names=asList("test"), expressions=asList(BoxJoinExpression(left=BinaryOperatorExpression(operator= Operator.JOIN, left=IdentifierExpression(name="a"), right=IdentifierExpression(name="b")), right=asList(BinaryOperatorExpression(operator= Operator.JOIN, left=IdentifierExpression(name="a"), right=IdentifierExpression(name="b")), IdentifierExpression(name="c")))))))
        assertEquals(expectedAst, ast)
    }

    @test
    fun dotJoinAndQuantifier() {
        val code = "assert oneRoot { one d: Dir | no d.parent }"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = AlloySpecification(declarations=asList(AssertionStatement(name="oneRoot", expressions=asList(QuantifiedExpression(operator=ONE, decls=asList(Decl(name="d", expression=IdentifierExpression(name="Dir"))), expressions=asList(UnaryOperatorExpression(operator=NO, expression=BinaryOperatorExpression(operator= Operator.JOIN, left=IdentifierExpression(name="d"), right=IdentifierExpression(name="parent")))))))))
        assertEquals(expectedAst, ast)
    }
}
