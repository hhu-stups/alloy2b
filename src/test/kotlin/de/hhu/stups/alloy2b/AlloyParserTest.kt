package de.hhu.stups.alloy2b

import org.junit.Assert.assertEquals
import org.junit.Test as test

class AlloyParserTest {
    @test
    fun parseFileSystem() {
        val expected = getResourceAsString("FileSystem_CST.txt");
        assertEquals(expected, toParseTree(parseResource("FileSystem.als")).multiLineString())
    }

    @test
    fun parseFileSystem3() {
        val expected = getResourceAsString("FileSystem3_CST.txt");
        assertEquals(expected, toParseTree(parseResource("FileSystem3.als")).multiLineString())
    }
}