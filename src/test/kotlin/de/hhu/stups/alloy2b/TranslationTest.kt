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
    fun translateFileSystem3() {
        val expected = getResourceAsString("FileSystem3.mch")
        assertEquals(expected, BTranslation(parseResource("FileSystem3.als").toAst(false)).getTranslation())
    }

    @test
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
}