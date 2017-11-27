package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.parsing.ParserFacade
import org.junit.Assert.assertEquals
import org.junit.Test as test

class AssociativityTest {
    @test
    fun assocOfImplication() {
        val code = "sig test {} {a => b => c}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = ParserFacade.parse("sig test {} {a => (b => c)}").root!!.toAst()
        assertEquals(expectedAst, ast)
    }

    @test
    fun assocOfJoin() {
        val code = "sig test {} {a.b.c}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = ParserFacade.parse("sig test {} {(a.b).c}").root!!.toAst()
        assertEquals(expectedAst, ast)
    }
}
