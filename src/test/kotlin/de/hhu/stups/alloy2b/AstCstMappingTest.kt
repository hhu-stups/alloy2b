package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.parsing.ParserFacade
import org.junit.Assert.assertEquals
import java.util.Arrays.asList
import org.junit.Test as test

class MappingTest {
    @test
    fun mapEmptySignaturDeclaration() {
        val code = "sig FSObject {}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = AlloySpecification(asList(SignatureDeclaration(name = "FSObject")))
        assertEquals(expectedAst, ast)
    }

    @test
    fun mapSimpleSignaturDeclaration() {
        val code = "sig FSObject { parent: lone Dir }"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = AlloySpecification(asList(SignatureDeclaration(name = "FSObject", decls = asList(Decl(name = "parent", expression = UnaryOperatorExpression(operator = Operator.LONE, expression = IdentifierExpression(name = "Dir")))))))
        assertEquals(expectedAst, ast)
    }
}