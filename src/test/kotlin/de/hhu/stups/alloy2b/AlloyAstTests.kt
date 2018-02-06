package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.translation.BTranslation
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
import org.junit.Assert
import org.junit.Test

class AlloyAstTests {
    @Test
    fun translateCards() {
        val expected = getResourceAsString("cards.mch")

        val astRoot = CompUtil.parseOneModule(getResourceAsString("cards.als"))
        astRoot.rootModule.allSigs.forEach {println(it)}

        //Assert.assertEquals(expected, BTranslation(parseResource("cards.als").toAst(false)).getTranslation())
    }
}