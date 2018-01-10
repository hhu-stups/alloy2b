package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.parsing.ParserFacade
import junit.framework.TestCase.assertTrue
import org.junit.Assert.assertEquals
import org.junit.Test as test

class AlloyParserTest {
    @test
    fun parseFileSystem() {
        val res = ParserFacade.parse(getResourceAsString("FileSystem.als"))
        assertTrue(res.errors.isEmpty())

        val expected = getResourceAsString("FileSystem_CST.txt")
        assertEquals(expected, toParseTree(res.root!!).multiLineString())
    }

    @test
    fun parseFileSystem3() {
        val res = ParserFacade.parse(getResourceAsString("FileSystem3.als"))
        assertTrue(res.errors.isEmpty())

        val expected = getResourceAsString("FileSystem3_CST.txt")
        assertEquals(expected, toParseTree(res.root!!).multiLineString())
    }
}