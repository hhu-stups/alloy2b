package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.AlloyAstTranslation
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test

class AlloyAstTests {
    private fun compareFilesTest(expectedBFile: String, alloyFile: String) {
        val expected = getResourceAsString(expectedBFile)

        val res = object {}.javaClass.getResource("/$alloyFile")
        val astRoot = CompUtil.parseEverything_fromFile(A4Reporter(), null, res.file)

        Assert.assertEquals(expected, AlloyAstTranslation(astRoot).getTranslation())
    }

    @Test @Ignore
    fun translateCards() {
        compareFilesTest("cards.mch", "cards.als")
    }

    @Test @Ignore
    fun translateCrewAlloc() {
        compareFilesTest("crewalloc.mch", "crewalloc.als")

    }

    @Test @Ignore
    fun translateFileSystem() {
        compareFilesTest("FileSystem.mch","FileSystem.als")
    }

    @Test @Ignore
    fun translateFileSystem2() {
        compareFilesTest("FileSystem2.mch","FileSystem2.als")
    }

    @Test @Ignore
    fun translatePeano() {
        compareFilesTest("Peano.mch","Peano.als")
    }

    @Test @Ignore
    fun translateSelfGrandpas() {
        compareFilesTest("SelfGrandpas.mch","SelfGrandpas.als")
    }
}