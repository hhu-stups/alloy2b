package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.Alloy2BParser
import org.junit.Assert
import org.junit.Test

class Alloy2BParserTests {

    fun getResourceAsString(resourceName: String): String {
        val expected = object {}.javaClass.getResourceAsStream("/$resourceName")
        return expected.bufferedReader().use { it.readText() }.trim()
    }

    @Test
    fun translateFileSystem() {
        val expected = getResourceAsString("filesystem.pl")
        Assert.assertEquals(expected, Alloy2BParser("/FileSystem.als").getPrologTerm())
    }

    @Test
    fun translateFileSystem2() {
        val expected = getResourceAsString("filesystem2.pl")
        Assert.assertEquals(expected, Alloy2BParser("/FileSystem2.als").getPrologTerm())
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        Assert.assertEquals(expected, Alloy2BParser("/cards.als").getPrologTerm())
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("selfgrandpas.pl")
        Assert.assertEquals(expected, Alloy2BParser("/SelfGrandpas.als").getPrologTerm())
    }

    @Test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.pl")
        Assert.assertEquals(expected, Alloy2BParser("/crewalloc.als").getPrologTerm())
    }

    @Test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.pl")
        Assert.assertEquals(expected, Alloy2BParser("/queens2.als").getPrologTerm())
    }

    @Test
    fun translateRiverCrossing() {
        val expected = getResourceAsString("rivercrossingpuzzle.pl")
        Assert.assertEquals(expected, Alloy2BParser("/RiverCrossingPuzzle.als").getPrologTerm())
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        Assert.assertEquals(expected, Alloy2BParser("/hanoi.als").getPrologTerm())
    }

    @Test
    fun translateEnumTest() {
        val expected = getResourceAsString("enumtest.pl")
        Assert.assertEquals(expected, Alloy2BParser("/EnumTest.als").getPrologTerm())
    }

    @Test
    fun translateGraphiso() {
        val expected = getResourceAsString("graphiso.pl")
        Assert.assertEquals(expected, Alloy2BParser("/graphiso.als").getPrologTerm())
    }

    @Test
    fun translateDisjFieldTest() {
        val expected = getResourceAsString("disjfieldtest.pl")
        Assert.assertEquals(expected, Alloy2BParser("/DisjFieldTest.als").getPrologTerm())
    }

    @Test
    fun translateFamilyV1() {
        val expected = getResourceAsString("family-v1.pl")
        Assert.assertEquals(expected, Alloy2BParser("/family-v1.als").getPrologTerm())
    }

    @Test
    fun translateRelations() {
        val expected = getResourceAsString("relations.pl")
        Assert.assertEquals(expected, Alloy2BParser("/relations.als").getPrologTerm())
    }

    @Test
    fun translateViews() {
        val expected = getResourceAsString("views.pl")
        Assert.assertEquals(expected, Alloy2BParser("/views.als").getPrologTerm())
    }

    @Test
    fun translateSequenceTests() {
        val expected = getResourceAsString("sequence_test.pl")
        Assert.assertEquals(expected, Alloy2BParser("/sequence_test.als").getPrologTerm())
    }

    @Test
    fun translateMutex() {
        val expected = getResourceAsString("mutex.pl")
        Assert.assertEquals(expected, Alloy2BParser("/mutex.als").getPrologTerm())
    }
}