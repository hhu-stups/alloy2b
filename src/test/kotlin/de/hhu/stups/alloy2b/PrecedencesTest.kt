package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.parsing.ParserFacade
import org.junit.Assert.assertEquals
import org.junit.Test as test

class PrecedencesTest {
    @test
    fun boxJoinVsDotJoin() {
        val code = "sig test {} {a.b[c]}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = ParserFacade.parse("sig test {} {(a.b)[c]}").root!!.toAst()
        assertEquals(expectedAst, ast)
    }

    @test
    fun dotJoinAndQuantifier() {
        val code = "assert oneRoot { one d: Dir | no d.parent }"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = ParserFacade.parse("assert oneRoot { one d: Dir | no (d.parent) }").root!!.toAst()
        assertEquals(expectedAst, ast)
    }

    @test
    fun unionAndDotJoin() {
        val code = "sig test {} {a + b.c}"
        val ast = ParserFacade.parse(code).root!!.toAst()
        val expectedAst = ParserFacade.parse("sig test {} {a + (b.c)}").root!!.toAst()
        assertEquals(expectedAst, ast)
    }
}
