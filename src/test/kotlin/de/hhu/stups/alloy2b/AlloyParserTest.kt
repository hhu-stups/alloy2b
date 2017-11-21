package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.antlr.AlloyLexer
import de.hhu.stups.alloy2b.antlr.AlloyParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Assert.assertEquals
import java.util.*
import org.junit.Test as test

class AlloyParserTest {

    fun lexerForCode(code: String) = AlloyLexer(CharStreams.fromString(code))

    fun lexerForResource(resourceName: String) = AlloyLexer(CharStreams.fromStream(this.javaClass.getResourceAsStream("/${resourceName}.als")))

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

    fun parse(lexer: AlloyLexer) : AlloyParser.SpecificationContext = AlloyParser(CommonTokenStream(lexer)).specification()

    fun parseResource(resourceName: String) : AlloyParser.SpecificationContext = AlloyParser(CommonTokenStream(lexerForResource(resourceName))).specification()

    @test fun parseFileSystemModel() {
        val expected = this.javaClass.getResourceAsStream("/FileSystem_Tree.txt");
        val expectedAsString = expected.bufferedReader().use { it.readText() }
        assertEquals(expectedAsString, toParseTree(parseResource("FileSystem")).multiLineString())
    }



}