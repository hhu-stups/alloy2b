package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*

class BTranslation(spec: AlloySpecification) {
    var sets = emptyList<String>()
    var constants = emptyList<String>()
    var properties = emptyList<String>()

    var extendingSignatures = mutableMapOf<String,List<String>>();

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
        addSignatureExtensionProperties()
    }

    private fun addSignatureExtensionProperties() {
        // two signatures extending the same base signature are disjuct
        extendingSignatures.values.forEach({ signatures ->
            for (i1 in 0..signatures.size-1) {
                for(i2 in i1..signatures.size-1) {
                    if(!signatures[i1].equals(signatures[i2])) {
                        properties += "${signatures[i1]} /\\ ${signatures[i2]} = {}"
                    }
                }
            }
        })
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
                is NameSignatureExtension -> {
                    properties = properties + "${sdec.name} <: ${sdec.signatureExtension.name}"
                    extendingSignatures[sdec.signatureExtension.name] = extendingSignatures.getOrDefault(sdec.signatureExtension.name, emptyList()) + sdec.name
                }
                else -> throw UnsupportedOperationException(sdec.signatureExtension.javaClass.canonicalName)
            }
        }

        // quantifiers handled by cardinality
        if(NO in sdec.qualifiers) {
            properties = properties + "card(${sdec.name}) = 0"
        }
        if(LONE in sdec.qualifiers) {
            properties = properties + "card(${sdec.name}) =< 1"
        }
        if(ONE in sdec.qualifiers) {
            properties = properties + "card(${sdec.name}) = 1"
        }
        if(SOME in sdec.qualifiers) {
            properties = properties + "card(${sdec.name}) >= 1"
        }
    }
}

