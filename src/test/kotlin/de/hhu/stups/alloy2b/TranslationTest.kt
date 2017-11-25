package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.translation.BTranslation
import org.junit.Assert
import org.junit.Test as test

class TranslationTest {

    @test
    fun translateFileSystemModel() {
        val expected = getResourceAsString("FileSystem.mch");
        Assert.assertEquals(expected, BTranslation(parseResource("FileSystem.als").toAst(false)).getTranslation())
    }

    @test
    fun translateFileSystemModel2() {
        val expected = getResourceAsString("FileSystem2.mch");
        Assert.assertEquals(expected, BTranslation(parseResource("FileSystem2.als").toAst(false)).getTranslation())
    }
}