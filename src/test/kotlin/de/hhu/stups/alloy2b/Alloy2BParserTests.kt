package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.Alloy2BParser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Alloy2BParserTests {

    private fun getResourceAsString(resourceName: String): String {
        val expected = object {}.javaClass.getResourceAsStream("/$resourceName")
        return expected.bufferedReader().use { it.readText() }.trim()
    }

    @Test
    fun translateFileSystem() {
        val expected = getResourceAsString("filesystem.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/FileSystem.als").prologTerm)
    }

    @Test
    fun translateFileSystem2() {
        val expected = getResourceAsString("filesystem2.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/FileSystem2.als").prologTerm)
    }

    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/cards.als").prologTerm)
    }

    @Test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("selfgrandpas.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/SelfGrandpas.als").prologTerm)
    }

    @Test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/crewalloc.als").prologTerm)
    }

    @Test
    fun translateQueens() {
        val expected = getResourceAsString("queens.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/queens.als").prologTerm)
    }

    @Test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/queens2.als").prologTerm)
    }

    @Test
    fun translateRiverCrossing() {
        val expected = getResourceAsString("rivercrossingpuzzle.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/RiverCrossingPuzzle.als").prologTerm)
    }

    @Test
    fun translateHanoi() {
        val expected = getResourceAsString("hanoi.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/hanoi.als").prologTerm)
    }

    @Test
    fun translateEnumTest() {
        val expected = getResourceAsString("enumtest.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/EnumTest.als").prologTerm)
    }

    @Test
    fun translateGraphiso() {
        val expected = getResourceAsString("graphiso.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/graphiso.als").prologTerm)
    }

    @Test
    fun translateDisjFieldTest() {
        val expected = getResourceAsString("disjfieldtest.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/DisjFieldTest.als").prologTerm)
    }

    @Test
    fun translateFamilyV1() {
        val expected = getResourceAsString("family-v1.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/family-v1.als").prologTerm)
    }

    @Test
    fun translateRelations() {
        val expected = getResourceAsString("relations.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/relations.als").prologTerm)
    }

    @Test
    fun translateViews() {
        val expected = getResourceAsString("views.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/views.als").prologTerm)
    }

    @Test
    fun translateSequenceTests() {
        val expected = getResourceAsString("sequence_test.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/sequence_test.als").prologTerm)
    }

    @Test
    fun translateMutex() {
        val expected = getResourceAsString("mutex.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/mutex.als").prologTerm)
    }

    @Test
    fun translateHttp() {
        val expected = getResourceAsString("http.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/http.als").prologTerm)
    }

    @Test
    fun translateKnightsAndKnaves() {
        val expected = getResourceAsString("knights_and_knaves.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/knights_and_knaves.als").prologTerm)
    }

    @Test
    fun translateJoinBinary() {
        val expected = getResourceAsString("joinbinary.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/JoinBinary.als").prologTerm)
    }

    @Test
    fun translateJoinBinaryComplex() {
        val expected = getResourceAsString("joinbinarycomplex.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/JoinBinaryComplex.als").prologTerm)
    }

    @Test
    fun translateArithmetic() {
        val expected = getResourceAsString("arithmetic.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/Arithmetic.als").prologTerm)
    }

    @Test
    fun translateRelLaw() {
        val expected = getResourceAsString("laws/rellaws.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/laws/RelLaws.als").prologTerm)
    }

    @Test
    fun translateAllocLarge() {
        val expected = getResourceAsString("alloc_large.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/alloc_large.als").prologTerm)
    }

    @Test
    fun translateOrderingTest() {
        val expected = getResourceAsString("ordering_test.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/ordering_test.als").prologTerm)
    }

    @Test
    fun translateGenerated200() {
        val expected = getResourceAsString("generated200.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/generated200.als").prologTerm)
    }

    @Test
    fun translateSlotData() {
        val expected = getResourceAsString("slot_data_v2.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/slot_data_v2.als").prologTerm)
    }

    @Test
    fun translateJobPuzzle() {
        val expected = getResourceAsString("job_puzzle.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/job_puzzle.als").prologTerm)
    }

    /*@Test
    fun translateGenerated400() {
        val expected = getResourceAsString("generated400.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/generated400.als").prologTerm)
    }*/

    @Test
    fun translateNamespaces() {
        val expected = getResourceAsString("namespace_module_a.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/namespace_module_a.als").prologTerm)
    }

    @Test
    fun translateNamespaces2() {
        val expected = getResourceAsString("moduleb.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/ModuleB.als").prologTerm)
    }

    @Test
    fun translateUtilInteger() {
        val expected = getResourceAsString("util_integer.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/util_integer.als").prologTerm)
    }

    @Test
    fun translateQuantifiers() {
        val expected = getResourceAsString("quantifiers.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/quantifiers.als").prologTerm)
    }

    @Test
    fun translateSeqPreconditions() {
        val expected = getResourceAsString("seq_preconditions.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/seq_preconditions.als").prologTerm)
    }

    @Test
    fun translateNaturals() {
        val expected = getResourceAsString("natural.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/natural.als").prologTerm)
    }

    @Test
    fun translateBinaryDifferentlyTyped() {
        val expected = getResourceAsString("binary_differently_typed.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/binary_differently_typed.als").prologTerm)
    }

    @Test
    fun translateLists() {
        val expected = getResourceAsString("lists.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/lists.als").prologTerm)
    }

    @Test
    fun translateRestrictions() {
        val expected = getResourceAsString("restrictions.pl")
        assertEquals(expected, Alloy2BParser().parseFromFile("/Restrictions.als").prologTerm)
    }
}