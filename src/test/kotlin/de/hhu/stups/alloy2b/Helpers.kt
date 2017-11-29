package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.antlr.AlloyLexer
import de.hhu.stups.alloy2b.antlr.AlloyParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.util.*

fun lexerForCode(code: String) = AlloyLexer(CharStreams.fromString(code))

fun lexerForResource(resourceName: String) =
        AlloyLexer(CharStreams.fromStream(object {}.javaClass.getResourceAsStream("/$resourceName")))

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

fun parseResource(resourceName: String): AlloyParser.SpecificationContext =
        AlloyParser(CommonTokenStream(lexerForResource(resourceName))).specification()

fun getResourceAsString(resourceName: String): String {
    val expected = object {}.javaClass.getResourceAsStream("/$resourceName")
    return expected.bufferedReader().use { it.readText() }
}