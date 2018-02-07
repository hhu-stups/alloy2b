package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.prolog.AlloyAstToProlog
import org.junit.Ignore
import org.junit.Test

class AlloyAstToPrologTest {
    @Test
    @Ignore
    fun translateFileSystem() {
        println(AlloyAstToProlog("/FileSystem.als").getPrologTerm())
    }
}