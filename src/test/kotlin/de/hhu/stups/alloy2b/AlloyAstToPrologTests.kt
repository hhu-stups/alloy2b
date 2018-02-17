package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.prolog.AlloyAstToProlog
import org.junit.Assert
import org.junit.Test

class AlloyAstToPrologTests {
    @Test
    fun translateFileSystem() {
        val expected = getResourceAsString("FileSystem.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/FileSystem.als").getPrologTerm())
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/cards.als").getPrologTerm())
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("SelfGrandpas.pl")
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
        val expected = getResourceAsString("RiverCrossingPuzzle.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/RiverCrossingPuzzle.als").getPrologTerm())
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        Assert.assertEquals(expected, AlloyAstToProlog("/hanoi.als").getPrologTerm())
    }
}