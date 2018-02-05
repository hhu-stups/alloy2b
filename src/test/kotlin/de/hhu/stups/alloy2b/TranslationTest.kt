package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.translation.BTranslation
import org.junit.Assert.*
import org.junit.Ignore
import org.junit.Test as test

class TranslationTest {
    @test
    fun translateCards() {
        val expected = getResourceAsString("cards.mch")
        assertEquals(expected, BTranslation(parseResource("cards.als").toAst(false)).getTranslation())
    }

    @test
    fun translateCrewAlloc() {
        val expected = getResourceAsString("crewalloc.mch")
        assertEquals(expected, BTranslation(parseResource("crewalloc.als").toAst(false)).getTranslation())
    }

    @test
    fun translateCrewAllocLarge() {
        val expected = getResourceAsString("crewAllocLarge.mch")
        assertEquals(expected, BTranslation(parseResource("crewAllocLarge.als").toAst(false)).getTranslation())
    }

    @test
    fun translateFileSystem() {
        val expected = getResourceAsString("FileSystem.mch")
        assertEquals(expected, BTranslation(parseResource("FileSystem.als").toAst(false)).getTranslation())
    }

    @test
    fun translateFileSystem2() {
        val expected = getResourceAsString("FileSystem2.mch")
        assertEquals(expected, BTranslation(parseResource("FileSystem2.als").toAst(false)).getTranslation())
    }

    @test
    @Ignore
    fun translateFileSystem3() {
        val expected = getResourceAsString("FileSystem3.mch")
        assertEquals(expected, BTranslation(parseResource("FileSystem3.als").toAst(false)).getTranslation())
    }

    @test
    @Ignore
    fun translateExtendedFileSystem() {
        val expected = getResourceAsString("ExtendedFileSystem.mch")
        assertEquals(expected, BTranslation(parseResource("ExtendedFileSystem.als").toAst(false)).getTranslation())
    }

    @test
    fun translatePeano() {
        val expected = getResourceAsString("Peano.mch")
        assertEquals(expected, BTranslation(parseResource("Peano.als").toAst(false)).getTranslation())
    }

    @test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("SelfGrandpas.mch")
        assertEquals(expected, BTranslation(parseResource("SelfGrandpas.als").toAst(false)).getTranslation())
    }

    @test
    @Ignore
    fun translateHanoi() {
        println(BTranslation(parseResource("hanoi.als").toAst(false)).getTranslation())
        //val expected = getResourceAsString("hanoi.mch")
        //assertEquals(expected, BTranslation(parseResource("hanoi.als").toAst(false)).getTranslation())
    }

    @test
    fun translateGraphIso() {
        val expected = getResourceAsString("graphiso.mch")
        assertEquals(expected, BTranslation(parseResource("graphiso.als").toAst(false)).getTranslation())
    }

    @test
    fun translateQueens() {
        val expected = getResourceAsString("queens.mch")
        assertEquals(expected, BTranslation(parseResource("queens.als").toAst(false)).getTranslation())
    }

    @test
    fun translateQueens2() {
        val expected = getResourceAsString("queens2.mch")
        assertEquals(expected, BTranslation(parseResource("queens2.als").toAst(false)).getTranslation())
    }

    @test
    @Ignore
    fun translateQueens3() {
        val expected = getResourceAsString("queens3.mch")
        assertEquals(expected, BTranslation(parseResource("queens3.als").toAst(false)).getTranslation())
    }

    @test
    @Ignore
            // TODO: union of two different signatures -> universe?
    fun translateQueens4() {
        val expected = getResourceAsString("queens4.mch")
        assertEquals(expected, BTranslation(parseResource("queens4.als").toAst(false)).getTranslation())
    }

    @test
    fun translateSudoku() {
        val expected = getResourceAsString("sudoku1.mch")
        assertEquals(expected, BTranslation(parseResource("sudoku1.als").toAst(false)).getTranslation())
    }

    @test
    fun translateRiverCrossingPuzzle() {
        val expected = getResourceAsString("RiverCrossingPuzzle.mch")
        assertEquals(expected, BTranslation(parseResource("RiverCrossingPuzzle.als").toAst(false)).getTranslation())
    }

    @test
    fun translateEnumTest() {
        val expected = getResourceAsString("EnumTest.mch")
        assertEquals(expected, BTranslation(parseResource("EnumTest.als").toAst(false)).getTranslation())
    }

    @test
    fun translateAddressBook1() {
        val expected = getResourceAsString("book/appendixA/addressBook1.mch")
        assertEquals(expected, BTranslation(parseResource("book/appendixA/addressBook1.als").toAst(false)).getTranslation())
    }

    @test
    fun translateAddressBook1a() {
        val expected = getResourceAsString("book/chapter2/addressBook1a.mch")
        assertEquals(expected, BTranslation(parseResource("book/chapter2/addressBook1a.als").toAst(false)).getTranslation())
    }

    @test
    fun translateSetLaws() {
        val expected = getResourceAsString("laws/SetLaws.mch")
        assertEquals(expected, BTranslation(parseResource("laws/SetLaws.als").toAst(false)).getTranslation())
    }

    @test
    fun translateRelLaws() {
        val expected = getResourceAsString("laws/RelLaws.mch")
        assertEquals(expected, BTranslation(parseResource("laws/RelLaws.als").toAst(false)).getTranslation())
    }

    @test
    fun translateAddAddr() {
        val expected = getResourceAsString("add_addr_correct.mch")
        assertEquals(expected, BTranslation(parseResource("add_addr_correct.als").toAst(false)).getTranslation())
    }

    @test
    fun translateDelUndoesAdd() {
        val expected = getResourceAsString("delUndoesAdd.mch")
        assertEquals(expected, BTranslation(parseResource("delUndoesAdd.als").toAst(false)).getTranslation())
    }
}