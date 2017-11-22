package de.hhu.stups.alloy2b

import org.junit.Assert.assertEquals
import org.junit.Test as test

class AlloyParserTest {
    @test fun parseFileSystemModel() {
        val expected = getResourceAsString("FileSystem_CST.txt");
        assertEquals(expected, toParseTree(parseResource("FileSystem.als")).multiLineString())
    }
}