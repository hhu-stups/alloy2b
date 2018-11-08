package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.AlloyAstToProlog
import org.junit.Assert
import org.junit.Test

class AlloyAstToPrologTests {

    fun getResourceAsString(resourceName: String): String {
        val expected = object {}.javaClass.getResourceAsStream("/$resourceName")
        return expected.bufferedReader().use { it.readText() }.trim()
    }

    @Test
    fun translateFileSystem() {
        val expected = getResourceAsString("filesystem.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/FileSystem.als").getPrologTerm())
    }

    @Test
    fun translateFileSystem2() {
        val expected = getResourceAsString("filesystem2.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/FileSystem2.als").getPrologTerm())
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/cards.als").getPrologTerm())
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("selfgrandpas.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/SelfGrandpas.als").getPrologTerm())
    }

    @Test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/crewalloc.als").getPrologTerm())
    }

    @Test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/queens2.als").getPrologTerm())
    }

    @Test
    fun translateRiverCrossing() {
        val expected = getResourceAsString("rivercrossingpuzzle.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/RiverCrossingPuzzle.als").getPrologTerm())
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/hanoi.als").getPrologTerm())
    }

    @Test
    fun translateEnumTest() {
        val expected = getResourceAsString("enumtest.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/EnumTest.als").getPrologTerm())
    }

    @Test
    fun translateGraphiso() {
        val expected = getResourceAsString("graphiso.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/graphiso.als").getPrologTerm())
    }

    @Test
    fun translateDisjFieldTest() {
        val expected = getResourceAsString("disjfieldtest.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/DisjFieldTest.als").getPrologTerm())
    }

    @Test
    fun translateFamilyV1() {
        val expected = getResourceAsString("family-v1.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/family-v1.als").getPrologTerm())
    }

    @Test
    fun translateRelations() {
        val expected = getResourceAsString("relations.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/relations.als").getPrologTerm())
    }

    @Test
    fun translateViews() {
        val expected = getResourceAsString("views.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/views.als").getPrologTerm())
    }

    @Test
    fun translateSequenceTests() {
        val expected = getResourceAsString("sequence_test.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/sequence_test.als").getPrologTerm())
    }

    @Test
    fun translateMutex() {
        val expected = getResourceAsString("mutex.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/mutex.als").getPrologTerm())
    }
}