package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.translation.BTranslation
import org.junit.Assert
import org.junit.Test as test

class TranslationTest {

    @test
    fun translateFileSystem() {
        val expected = getResourceAsString("FileSystem.mch")
        Assert.assertEquals(expected, BTranslation(parseResource("FileSystem.als").toAst(false)).getTranslation())
    }

    @test
    fun translateFileSystem2() {
        val expected = getResourceAsString("FileSystem2.mch")
        Assert.assertEquals(expected, BTranslation(parseResource("FileSystem2.als").toAst(false)).getTranslation())
    }

    @test
    fun translateFileSystem3() {
        val expected = getResourceAsString("FileSystem3.mch")
        Assert.assertEquals(expected, BTranslation(parseResource("FileSystem3.als").toAst(false)).getTranslation())
    }

    @test
    fun translateExtendedFileSystem() {
        val expected = getResourceAsString("ExtendedFileSystem.mch")
        Assert.assertEquals(expected, BTranslation(parseResource("ExtendedFileSystem.als").toAst(false)).getTranslation())
    }

    @test
    fun translateSelfGrandpas() {
        val expected = getResourceAsString("SelfGrandpas.mch")
        Assert.assertEquals(expected, BTranslation(parseResource("SelfGrandpas.als").toAst(false)).getTranslation())
    }

    @test
    fun translateRiverCrossingPuzzle() {
        println(BTranslation(parseResource("RiverCrossingPuzzle.als").toAst(false)).getTranslation())
        //val expected = getResourceAsString("RiverCrossingPuzzle.mch")
        //Assert.assertEquals(expected, BTranslation(parseResource("RiverCrossingPuzzle.als").toAst(false)).getTranslation())
    }
}