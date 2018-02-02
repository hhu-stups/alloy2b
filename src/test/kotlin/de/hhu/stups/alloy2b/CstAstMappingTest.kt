package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.parsing.ParserFacade
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import java.util.Arrays.asList
import org.junit.Test as test

class CstAstMappingTest {
    @test
    fun mapEmptySignaturDeclaration() {
        val code = "sig FSObject {}"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        assertTrue(errors.isEmpty())

        val expectedAst = AlloySpecification(asList(SignatureDeclarations(asList(SignatureDeclaration(name = IdentifierExpression("FSObject"))))))
        assertEquals(expectedAst, ast)
    }

    @test
    fun mapSimpleSignaturDeclaration() {
        val code = "sig FSObject { parent: lone Dir }"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        assertTrue(errors.isEmpty())

        val expectedAst = AlloySpecification(asList(SignatureDeclarations(asList(SignatureDeclaration(name = IdentifierExpression("FSObject"), decls = asList(Decl(names = asList(IdentifierExpression("parent")), expression = QuantifiedExpression(operator = Operator.LONE, expression = IdentifierExpression(name = "Dir")))))))))
        assertEquals(expectedAst, ast)
    }

    @test
    fun mapNegatedCompareExpression() {
        val code = "sig FSObject { parent: x != y }"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        assertTrue(errors.isEmpty())

        val expectedAst = AlloySpecification(asList(SignatureDeclarations(asList(SignatureDeclaration(name=IdentifierExpression("FSObject"), decls=asList(Decl(names=asList(IdentifierExpression("parent")), expression=QuantifiedExpression(operator= Operator.ONE, expression=UnaryOperatorExpression(Operator.NOT,BinaryOperatorExpression(operator=Operator.EQUAL, left=IdentifierExpression(name="x"), right=IdentifierExpression(name="y"))), position=null))))))))
        assertEquals(expectedAst, ast)
    }

    @test
    fun arrowOpAndOne() {
        val code = "sig s { f: setX lone -> one setX}"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        assertTrue(errors.isEmpty())

        val expectedAst = AlloySpecification(asList(SignatureDeclarations(signatures=asList(SignatureDeclaration(name=IdentifierExpression(name="s"), decls= asList(Decl(names=asList(IdentifierExpression(name="f")), expression=QuantifiedExpression(operator= Operator.ONE, expression=ArrowOperatorExpression(operator=Operator.TOTAL_INJECTION, left=IdentifierExpression(name="setX"), right=IdentifierExpression(name="setX"))))))))))
        assertEquals(expectedAst, ast)
    }
}