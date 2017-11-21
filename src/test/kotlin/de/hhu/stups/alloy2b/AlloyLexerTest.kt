package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.antlr.*
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CharStreams
import org.junit.Assert.assertEquals
import java.io.*
import java.util.*
import org.junit.Test as test
//import kotlin.test.*

class AlloyLexerTest {

    fun lexerForCode(code: String) = AlloyLexer(CharStreams.fromString(code))

    fun lexerForResource(resourceName: String) = AlloyLexer(CharStreams.fromStream(this.javaClass.getResourceAsStream("/${resourceName}")))

    fun tokens(lexer: AlloyLexer): List<String> {
        val tokens = LinkedList<String>()
        do {
            val t = lexer.nextToken()
            when (t.type) {
                -1 -> tokens.add("EOF")
                else -> if (t.type != AlloyLexer.WS) tokens.add(lexer.ruleNames[t.type - 1])
            }
        } while (t.type != -1)
        return tokens
    }

    @test fun tokenizeEmptySignatureDeclaration() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "RBRACKET", "EOF"),
                tokens(lexerForCode("sig SomeName { }")))
    }
}