package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.antlr.AlloyLexer
import de.hhu.stups.alloy2b.antlr.AlloyParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Assert.assertEquals
import java.util.*
import org.junit.Test as test

class AlloyParserTest {
    @test fun parseFileSystemModel() {
        val expected = getResourceAsString("FileSystem_CST.txt");
        assertEquals(expected, toParseTree(parseResource("FileSystem.als")).multiLineString())
    }
}