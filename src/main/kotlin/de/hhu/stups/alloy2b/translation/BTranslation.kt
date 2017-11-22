package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*

class BTranslation(spec: AlloySpecification) {
    val sets = emptyList<String>()

    fun getTranslation() = sets.joinToString(",")

    init {
        spec.declarations.forEach({ stmt -> translate(stmt) })
    }

    private fun translate(stmt: Statement) {
        when(stmt) {
            is CheckStatement -> translate(stmt)
            is AssertionStatement -> print("assertion")
            is SignatureDeclaration -> print("signature")
            is FactDeclaration -> print("fact")
            else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
        }
    }

    private fun translate(stmt: CheckStatement) {
        print("check statement")
    }
}

