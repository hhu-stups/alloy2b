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

        val expectedAst = AlloySpecification(asList(SignatureDeclaration(names = asList("FSObject"))))
        assertEquals(expectedAst, ast)
    }

    @test
    fun mapSimpleSignaturDeclaration() {
        val code = "sig FSObject { parent: lone Dir }"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        assertTrue(errors.isEmpty())

        val expectedAst = AlloySpecification(asList(SignatureDeclaration(names = asList("FSObject"), decls = asList(Decl(names = asList(IdentifierExpression("parent")), expression = QuantifiedExpression(operator = Operator.LONE, expression = IdentifierExpression(name = "Dir")))))))
        assertEquals(expectedAst, ast)
    }
}