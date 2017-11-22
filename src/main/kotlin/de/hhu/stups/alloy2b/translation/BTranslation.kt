package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*

class BTranslation(spec: AlloySpecification) {
    var sets = emptyList<String>()
    var constants = emptyList<String>()
    var properties = emptyList<String>()
    var assertions = emptyList<String>()

    var signatures = emptyList<String>()
    var extendingSignatures = mutableMapOf<String,List<String>>();
    var alloyAssertions = mutableMapOf<String,String>();

    fun getTranslation() : String {
        val builder = StringBuilder()

        builder.appendln("MACHINE alloytranslation")

        builder.appendln("SETS")
        builder.appendln("    " + sets.joinToString(", "))

        builder.appendln("CONSTANTS")
        builder.appendln("    " + constants.joinToString(", "))

        builder.appendln("PROPERTIES")
        builder.appendln("    " + properties.joinToString(" &\n    "))

        builder.appendln("ASSERTIONS")
        builder.appendln("    " + assertions.joinToString(" &\n    "))

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
            is AssertionStatement -> translate(stmt)
            is SignatureDeclaration -> translate(stmt)
            is FactDeclaration -> translate(stmt)
            else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
        }
    }

    private fun translate(stmt: AssertionStatement) {
        alloyAssertions[stmt.name] = stmt.expressions.map { e -> translateExpression(e) }.joinToString(" & ")
    }

    private fun translate(fdec: FactDeclaration) {
        properties += fdec.expressions.map { e-> translateExpression(e) }.joinToString(" & ")
    }

    private fun translate(stmt: CheckStatement) {
        stmt.expressions.forEach({e ->
            when(e) {
                is IdentifierExpression -> assertions += alloyAssertions[e.name].orEmpty()
                else -> assertions += translateExpression(e)
            }
        })
    }

    private fun translate(sdec: SignatureDeclaration) {
        signatures += sdec.name

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

    private fun translateExpression(e: Expression) =
        when(e) {
            is QuantifiedExpression -> translateExpression(e)
            is IdentifierExpression -> e.name
            is BinaryOperatorExpression -> translateExpression(e)
            is UnaryOperatorExpression -> translateExpression(e)
            else -> throw UnsupportedOperationException(e.javaClass.canonicalName)
        }


    private fun translateExpression(qe: QuantifiedExpression) : String =
        when(qe.operator) {
            ALL -> "!(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} => ${qe.expressions.map { e->translateExpression(e) }.joinToString(" & ")})"
            NO -> "not(#(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} => ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")})"
            ONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")}}) = 1"
            LONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")}}) =< 1"
            else -> throw UnsupportedOperationException(qe.operator.name)
        }

    private fun translateExpression(qe: BinaryOperatorExpression) : String =
        when(qe.operator) {
            JOIN -> translateJoin(qe)
            IN -> "${translateExpression(qe.left)} : ${translateExpression(qe.right)}"
            EQUAL -> "${translateExpression(qe.left)} = ${translateExpression(qe.right)}"
            UNION -> "${translateExpression(qe.left)} \\/ ${translateExpression(qe.right)}"
            else -> throw UnsupportedOperationException(qe.operator.name)
        }

    private fun translateExpression(qe: UnaryOperatorExpression) : String =
        when(qe.operator) {
            CLOSURE -> "closure(${translateExpression(qe.expression)})"
            CLOSURE1 -> "closure1(${translateExpression(qe.expression)})"
            NO -> "${translateExpression(qe.expression)} = {}"
            else -> throw UnsupportedOperationException(qe.operator.name)
        }

    private fun translateJoin(je: BinaryOperatorExpression) : String {
        if(translateExpression(je.left) in signatures) {
            return "${translateExpression(je.left)}[${translateExpression(je.right)}]"
        } else if(translateExpression(je.right) in signatures) {
            return "${translateExpression(je.left)}~[${translateExpression(je.right)}]"
        } else {
            return "(${translateExpression(je.left)} ; ${translateExpression(je.right)})"
        }
    }

    private fun translateDeclsIDList(decls: List<Decl>) =
        decls.map { d -> d.name }.joinToString(", ")

    private fun translateDeclsExprList(decls: List<Decl>) =
        decls.map { d -> "${d.name} : ${translateExpression(d.expression)}" }.joinToString(", ")
}

