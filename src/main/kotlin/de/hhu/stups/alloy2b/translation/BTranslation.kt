package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*

class BTranslation(spec: AlloySpecification) {
    var sets = emptyList<String>()
    var constants = emptyList<String>()
    var properties = emptyList<String>()

    fun getTranslation() : String {
        val builder = StringBuilder()

        builder.appendln("MACHINE alloytranslation")

        builder.appendln("SETS")
        builder.appendln("    " + sets.joinToString(", "))

        builder.appendln("CONSTANTS")
        builder.appendln("    " + constants.joinToString(", "))

        builder.appendln("PROPERTIES")
        builder.appendln("    " + properties.joinToString(" &\n    "))

        return builder.toString()
    }

    init {
        spec.declarations.forEach({ stmt -> translate(stmt) })
    }

    private fun translate(stmt: Statement) {
        when(stmt) {
            is CheckStatement -> translate(stmt)
            is AssertionStatement -> print("assertion")
            is SignatureDeclaration -> translate(stmt)
            is FactDeclaration -> print("fact")
            else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
        }
    }

    private fun translate(stmt: CheckStatement) {
        print("check statement")
    }

    private fun translate(sdec: SignatureDeclaration) {
        if(sdec.signatureExtension == null) {
            // basic signature -> B set
            sets = sets + sdec.name
        } else {
            constants = constants + sdec.name
            when(sdec.signatureExtension) {
                is NameSignatureExtension -> properties = properties + (sdec.name + " <: " + sdec.signatureExtension.name)
                else -> throw UnsupportedOperationException(sdec.signatureExtension.javaClass.canonicalName)
            }
        }
    }
}

