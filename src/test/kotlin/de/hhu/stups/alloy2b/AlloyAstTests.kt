package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.AlloyAstTranslation
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
import org.junit.Assert
import org.junit.Test

class AlloyAstTests {
    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.mch")

        val res = object {}.javaClass.getResource("/cards.als")
        val astRoot = CompUtil.parseEverything_fromFile(A4Reporter(), null, res.file)

        Assert.assertEquals(expected, AlloyAstTranslation(astRoot).getTranslation())
    }
}