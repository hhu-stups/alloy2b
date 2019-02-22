package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.Alloy2BParser
import org.junit.Assert
import org.junit.Test

class Alloy2BParserTests {

    private fun getResourceAsString(resourceName: String): String {
        val expected = object {}.javaClass.getResourceAsStream("/$resourceName")
        return expected.bufferedReader().use { it.readText() }.trim()
    }

    @Test
    fun translateFileSystem() {
        val expected = getResourceAsString("filesystem.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/FileSystem.als").prologTerm)
    }

    @Test
    fun translateFileSystem2() {
        val expected = getResourceAsString("filesystem2.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/FileSystem2.als").prologTerm)
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/cards.als").prologTerm)
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("selfgrandpas.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/SelfGrandpas.als").prologTerm)
    }

    @Test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/crewalloc.als").prologTerm)
    }

    @Test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/queens2.als").prologTerm)
    }

    @Test
    fun translateRiverCrossing() {
        val expected = getResourceAsString("rivercrossingpuzzle.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/RiverCrossingPuzzle.als").prologTerm)
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/hanoi.als").prologTerm)
    }

    @Test
    fun translateEnumTest() {
        val expected = getResourceAsString("enumtest.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/EnumTest.als").prologTerm)
    }

    @Test
    fun translateGraphiso() {
        val expected = getResourceAsString("graphiso.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/graphiso.als").prologTerm)
    }

    @Test
    fun translateDisjFieldTest() {
        val expected = getResourceAsString("disjfieldtest.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/DisjFieldTest.als").prologTerm)
    }

    @Test
    fun translateFamilyV1() {
        val expected = getResourceAsString("family-v1.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/family-v1.als").prologTerm)
    }

    @Test
    fun translateRelations() {
        val expected = getResourceAsString("relations.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/relations.als").prologTerm)
    }

    @Test
    fun translateViews() {
        val expected = getResourceAsString("views.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/views.als").prologTerm)
    }

    @Test
    fun translateSequenceTests() {
        val expected = getResourceAsString("sequence_test.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/sequence_test.als").prologTerm)
    }

    @Test
    fun translateMutex() {
        val expected = getResourceAsString("mutex.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/mutex.als").prologTerm)
    }

    @Test
    fun translateHttp() {
        val expected = getResourceAsString("http.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/http.als").prologTerm)
    }

    @Test
    fun translateKnightsAndKnaves() {
        val expected = getResourceAsString("knights_and_knaves.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/knights_and_knaves.als").prologTerm)
    }

    @Test
    fun translateJoinBinary() {
        val expected = getResourceAsString("joinbinary.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/JoinBinary.als").prologTerm)
    }

    @Test
    fun translateArithmetic() {
        val expected = getResourceAsString("arithmetic.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/Arithmetic.als").prologTerm)
    }

    @Test
    fun translateRelLaw() {
        val expected = getResourceAsString("laws/rellaws.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/laws/RelLaws.als").prologTerm)
    }

    @Test
    fun translateAllocLarge() {
        val expected = getResourceAsString("alloc_large.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/alloc_large.als").prologTerm)
    }

    @Test
    fun translateOrderingTest() {
        val expected = getResourceAsString("ordering_test.pl")
        Assert.assertEquals(expected, Alloy2BParser().parseFromFile("/ordering_test.als").prologTerm)
    }
}