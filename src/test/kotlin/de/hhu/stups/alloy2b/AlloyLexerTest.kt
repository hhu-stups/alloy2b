package de.hhu.stups.alloy2b

import org.junit.Assert.assertEquals
import org.junit.Test as test

class AlloyLexerTest {
    @test
    fun tokenizeEmptySignatureDeclaration() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "RBRACKET", "EOF"),
                tokens(lexerForCode("sig SomeName { }")))
    }

    @test
    fun tokenizeBoxJoin() {
        assertEquals(listOf("ID", "DOT", "ID","LSQBRACKET","ID","RSQBRACKET","EOF"),
                tokens(lexerForCode("a.b[c]")))
    }
}