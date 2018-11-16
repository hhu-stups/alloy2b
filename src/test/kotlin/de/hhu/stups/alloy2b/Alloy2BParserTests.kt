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
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/FileSystem.als"))
    }

    @Test
    fun translateFileSystem2() {
        val expected = getResourceAsString("filesystem2.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/FileSystem2.als"))
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/cards.als"))
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("selfgrandpas.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/SelfGrandpas.als"))
    }

    @Test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/crewalloc.als"))
    }

    @Test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/queens2.als"))
    }

    @Test
    fun translateRiverCrossing() {
        val expected = getResourceAsString("rivercrossingpuzzle.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/RiverCrossingPuzzle.als"))
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/hanoi.als"))
    }

    @Test
    fun translateEnumTest() {
        val expected = getResourceAsString("enumtest.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/EnumTest.als"))
    }

    @Test
    fun translateGraphiso() {
        val expected = getResourceAsString("graphiso.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/graphiso.als"))
    }

    @Test
    fun translateDisjFieldTest() {
        val expected = getResourceAsString("disjfieldtest.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/DisjFieldTest.als"))
    }

    @Test
    fun translateFamilyV1() {
        val expected = getResourceAsString("family-v1.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/family-v1.als"))
    }

    @Test
    fun translateRelations() {
        val expected = getResourceAsString("relations.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/relations.als"))
    }

    @Test
    fun translateViews() {
        val expected = getResourceAsString("views.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/views.als"))
    }

    @Test
    fun translateSequenceTests() {
        val expected = getResourceAsString("sequence_test.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/sequence_test.als"))
    }

    @Test
    fun translateMutex() {
        val expected = getResourceAsString("mutex.pl")
        Assert.assertEquals(expected, Alloy2BParser().alloyToPrologTerm("/mutex.als"))
    }
}