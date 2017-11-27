package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*
import de.hhu.stups.alloy2b.typechecker.Type.*
import de.hhu.stups.alloy2b.typechecker.TypeChecker

class BTranslation(spec: AlloySpecification) {
    val sets = mutableListOf<String>()
    val constants = mutableListOf<String>()
    val definitions = mutableListOf<String>()
    val properties = mutableListOf<String>()
    val assertions = mutableListOf<String>()

    val signatures = mutableListOf<String>()
    val extendingSignatures = mutableMapOf<String, List<String>>();

    val fields = mutableListOf<String>()

    val alloyAssertions = mutableMapOf<String, String>();

    init {
        TypeChecker(spec)
        spec.declarations.forEach({ stmt -> translate(stmt) })
        addSignatureExtensionProperties()
    }

    fun getTranslation(): String {
        val builder = StringBuilder()

        builder.appendln("MACHINE alloytranslation")

        appendIfNotEmpty(builder, sets, ", ", "SETS")
        appendIfNotEmpty(builder, constants, ", ", "CONSTANTS")
        appendIfNotEmpty(builder, definitions, " ;\n    ", "DEFINITIONS")
        appendIfNotEmpty(builder, properties, " &\n    ", "PROPERTIES")
        appendIfNotEmpty(builder, assertions, " &\n    ", "ASSERTIONS")

        builder.appendln("END")

        return builder.toString()
    }

    private fun appendIfNotEmpty(builder: StringBuilder, list: MutableList<String>, delimiter: String, sectionName: String) {
        if (list.isEmpty()) {
            return
        }
        builder.appendln(sectionName)
        builder.appendln("    " + list.joinToString(delimiter))
    }

    private fun addSignatureExtensionProperties() {
        // two signatures extending the same base signature are disjuct
        extendingSignatures.values.forEach({ signatures ->
            for (i1 in 0..signatures.size - 1) {
                for (i2 in i1..signatures.size - 1) {
                    addSignatureExtensionIfNotEqual(signatures[i1], signatures[i2])
                }
            }
        })
    }

    private fun addSignatureExtensionIfNotEqual(sig1: String, sig2: String) {
        if (!sig1.equals(sig2)) {
            properties.add("${sig1} /\\ ${sig2} = {}")
        }
    }

    /**
     * Translate {@link Statement statements}.
     */
    private fun translate(stmt: Statement) {
        when (stmt) {
            is CheckStatement -> translate(stmt)
            is AssertionStatement -> translate(stmt)
            is SignatureDeclaration -> translate(stmt)
            is FactDeclaration -> translate(stmt)
            is FunDeclaration -> translate(stmt)
            is PredDeclaration -> translate(stmt)
            else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
        }
    }

    private fun translate(stmt: AssertionStatement) {
        alloyAssertions[stmt.name] = stmt.expressions.map { e -> translateExpression(e) }.joinToString(" & ")
    }

    private fun translate(fdec: FactDeclaration) {
        properties.add(fdec.expressions.map { e -> translateExpression(e) }.joinToString(" & "))
    }

    private fun translate(fdec: FunDeclaration) {
        val builder = StringBuilder()
        if (fdec.decls.isEmpty()) {
            builder.append("${fdec.name}")
        } else {
            builder.append("${fdec.name}(${fdec.decls.map { it.name }.joinToString(", ")}) == ")
        }
        val decls = fdec.decls.map { "${it.name} : ${translateExpression(it.expression)}" }.joinToString(" & ")
        val paramdecls = fdec.decls.map { "p_${it.name} : ${translateExpression(it.expression)}" }.joinToString(" & ")
        val returnVals = fdec.decls.map { "p_${it.name}" }
        val expressions = fdec.expressions.map { translateExpression(it) }
        val parameterExpressions = returnVals.map { returnVal -> expressions.map { "${returnVal} : ${it}" }.joinToString(" & ") }.joinToString(" & ")
        builder.append("{${returnVals.joinToString(", ")} | ${paramdecls} & ${decls} & ${parameterExpressions}}")
        definitions.add(builder.toString())
    }

    private fun translate(pdec: PredDeclaration) {
        val builder = StringBuilder()
        if (pdec.decls.isEmpty()) {
            builder.append("${pdec.name} == ")
        } else {
            builder.append("${pdec.name}(${pdec.decls.map { it.name }}) == ")
        }
        val decls = pdec.decls.map { "${it.name} : ${translateExpression(it.expression)}" }.joinToString(" & ")
        builder.append(decls)
        val blocks = pdec.expressions.map { translateExpression(it) }.joinToString(" & ")
        if (blocks.isEmpty().not()) {
            builder.append(" & ${blocks}")
        }
        definitions.add(builder.toString())
    }

    private fun translate(stmt: CheckStatement) {
        stmt.expressions.forEach({ e ->
            when (e) {
                is IdentifierExpression -> assertions.add(alloyAssertions[e.name].orEmpty())
                else -> assertions.add(translateExpression(e))
            }
        })
    }

    private fun translate(sdec: SignatureDeclaration) {
        signatures.addAll(sdec.names) // used to decide how to translate dot join

        handleQuantifiersByCardinality(sdec)

        // field declarations are mapped to constants and properties
        sdec.decls.forEach({ d ->
            constants += d.name
            sdec.names.forEach { sdecName -> translateFieldDeclarations(d, sdecName) }
        })

        if (sdec.signatureExtension == null) {
            // basic signature -> B set
            sets.addAll(sdec.names)
            return
        }
        constants.addAll(sdec.names)
        when (sdec.signatureExtension) {
            is NameSignatureExtension -> {
                sdec.names.forEach({
                    properties.add("${it} <: ${sdec.signatureExtension.name}")
                    extendingSignatures[sdec.signatureExtension.name] =
                            extendingSignatures.getOrDefault(sdec.signatureExtension.name, emptyList()) + it
                })
            }
            else -> throw UnsupportedOperationException(sdec.signatureExtension.javaClass.canonicalName)
        }
    }

    private fun handleQuantifiersByCardinality(sdec: SignatureDeclaration) {
        if (NO in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${it}) = 0") }
        }
        if (LONE in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${it}) <= 1") }
        }
        if (ONE in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${it}) = 1") }
        }
        if (SOME in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${it}) >= 1") }
        }
    }

    private fun translateFieldDeclarations(decl: Decl, name: String) {
        fields.add(decl.name); // used to decide how to translate dot join
        if (decl.expression is UnaryOperatorExpression) {
            if (decl.expression.operator == LONE) {
                // one-to-one mapping, i.e. function
                properties.add("${decl.name} : ${name} +-> ${translateExpression(decl.expression.expression)}")
                return
            }
            properties.add("${decl.name} : ${name} <-> ${translateExpression(decl.expression.expression)}")
            return
        }
        properties.add("${decl.name} : ${name} <-> ${translateExpression(decl.expression)}")
    }

    private fun translateExpression(e: Expression) =
            when (e) {
                is QuantifiedExpression -> translateExpression(e)
                is IdentifierExpression -> e.name
                is BinaryOperatorExpression -> translateExpression(e)
                is UnaryOperatorExpression -> translateExpression(e)
                is LetExpression -> translateExpression(e)
                is BoxJoinExpression -> translateExpression(e)
                else -> throw UnsupportedOperationException(e.javaClass.canonicalName)
            }


    private fun translateExpression(qe: QuantifiedExpression): String =
            when (qe.operator) {
                ALL -> "!(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} => ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")})"
                NO -> "not(#(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} & ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")}))"
                ONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")}}) = 1"
                LONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.map { e -> translateExpression(e) }.joinToString(" & ")}}) <= 1"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateExpression(le: LetExpression): String {
        val builder = StringBuilder()
        builder.append("LET ")
        builder.append(le.letDecls.map { it -> "${it.name}" }.joinToString(", "))
        builder.append(" BE ")
        builder.append(le.letDecls.map { it -> "${it.name} = ${translateExpression(it.expression)}" }.joinToString(" & "))
        builder.append(" IN")
        builder.append(le.expressions.map { it -> "${translateExpression(it)}" }.joinToString(" & "))
        builder.append(" END")
        return builder.toString()
    }

    private fun translateExpression(bje: BoxJoinExpression): String {
        val parameters = bje.right.map { translateExpression(it) }
        return "${translateExpression(bje.left)}(${parameters.joinToString(", ")})"
    }

    private fun translateExpression(qe: BinaryOperatorExpression): String {
        val symbol: String
        when (qe.operator) {
            JOIN -> return translateJoin(qe)
            OVERRIDE -> symbol = "<+"
            DOM_RESTR -> symbol = "<|"
            RAN_RESTR -> symbol = "|>"
            IN -> symbol = ":"
            EQUAL -> symbol = "="
            INTERSECTION -> symbol = "/\\"
            UNION -> symbol = "\\/"
            DIFFERENCE -> symbol = "-"
            GREATER -> symbol = ">"
            GREATER_EQUAL -> symbol = ">="
            LESS -> symbol = "<"
            LESS_EQUAL -> symbol = "<="
            AND -> symbol = "&"
            OR -> symbol = "or"
            IMPLIES -> symbol = "=>"
            IFF -> symbol = "<=>"
            INVERSE -> symbol = "~"
            CARTESIAN -> symbol = "*"
            TOTAL_FUNCTION -> symbol = "-->"
            PARTIAL_FUNCTION -> symbol = "+->"
            BIJECTIVE_FUNCTION -> symbol = ">->>"
            else -> throw UnsupportedOperationException(qe.operator.name)
        }
        return "${translateExpression(qe.left)} $symbol ${translateExpression(qe.right)}"
    }

    private fun translateExpression(qe: UnaryOperatorExpression): String =
            when (qe.operator) {
                CLOSURE -> "closure(${translateExpression(qe.expression)})"
                CLOSURE1 -> "closure1(${translateExpression(qe.expression)})"
                NO -> "${translateExpression(qe.expression)} = {}"
                CARD -> "card(${translateExpression(qe.expression)})"
                INVERSE -> "${translateExpression(qe.expression)}~"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateJoin(je: BinaryOperatorExpression): String {
        if(je.left.type == UNTYPED || je.right.type == UNTYPED) {
            throw UnsupportedOperationException("missing types in join translation")
        }
        if (je.left.type == BINARY && je.right.type == BINARY) {
            return "(${translateExpression(je.left)} ; ${translateExpression(je.right)})"
        } else if(je.left.type == BINARY && je.right.type == UNARY) {
            return "${translateExpression(je.left)}~[${translateExpression(je.right)}]"
        } else if (je.left.type == UNARY && je.right.type == BINARY) {
            return "${translateExpression(je.left)}[${translateExpression(je.right)}]"
        }
        throw UnsupportedOperationException("join not supported this way")
    }

    private fun translateDeclsIDList(decls: List<Decl>) =
            decls.map { d -> d.name }.joinToString(", ")

    private fun translateDeclsExprList(decls: List<Decl>) =
            decls.map { d -> "${d.name} : ${translateExpression(d.expression)}" }.joinToString(" & ")
}

