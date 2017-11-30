package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*
import de.hhu.stups.alloy2b.typechecker.Type
import de.hhu.stups.alloy2b.typechecker.Type.*
import de.hhu.stups.alloy2b.typechecker.TypeChecker

class BTranslation(spec: AlloySpecification) {
    private val sets = mutableListOf<String>()
    private val constants = mutableListOf<String>()
    private val definitions = mutableListOf<String>()
    private val properties = mutableListOf<String>()
    private val assertions = mutableListOf<String>()

    private val signatures = mutableListOf<String>()
    private val abstractSignatures = mutableListOf<String>()
    private val extendingSignatures = mutableMapOf<String, List<String>>()

    private val fields = mutableListOf<String>()

    private val alloyAssertions = mutableMapOf<String, String>()

    init {
        TypeChecker(spec)
        spec.declarations.forEach({ stmt -> translate(stmt) })
        addSignatureExtensionProperties()
    }

    fun getTranslation(): String {
        val builder = StringBuilder()

        builder.appendln("/*@ generated */")
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
            for (i1 in 0 until signatures.size) {
                for (i2 in i1 until signatures.size) {
                    addSignatureExtensionIfNotEqual(signatures[i1], signatures[i2])
                }
            }
        })
        // abstract signatures are exhaustively divided into their subsignatures
        abstractSignatures.forEach({ absSigName ->
            properties.add("${extendingSignatures.get(absSigName).orEmpty().map { sanitizeIdentifier(it) }.joinToString(" \\/ ")} = ${sanitizeIdentifier(absSigName)}")})
    }

    private fun addSignatureExtensionIfNotEqual(sig1: String, sig2: String) {
        if (sig1 != sig2) {
            properties.add("${sanitizeIdentifier(sig1)} /\\ ${sanitizeIdentifier(sig2)} = {}")
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
        alloyAssertions[stmt.name] = stmt.expressions.joinToString(" & ") { e -> translateExpression(e) }
    }

    private fun translate(fdec: FactDeclaration) {
        properties.add(fdec.expressions.joinToString(" & ") { e -> translateExpression(e) })
    }

    private fun translate(fdec: FunDeclaration) {
        val builder = StringBuilder()
        if (fdec.decls.isEmpty()) {
            builder.append(sanitizeIdentifier(fdec.name))
        } else {
            builder.append("${sanitizeIdentifier(fdec.name)}(${fdec.decls.joinToString(", ") { it.names.joinToString(", "){name -> sanitizeIdentifier(name)} }}) == ")
        }
        val decls = fdec.decls.joinToString(" & ") { decl -> decl.names.joinToString(" & ") { "${sanitizeIdentifier(it)} <: ${translateExpression(decl.expression)}" } }
        val parameterExpressions = fdec.expressions.map { translateExpression(it) }.joinToString(" & ") { "temp : $it" }
        // represent functions as set comprehensions
        builder.append("{ temp | $decls & $parameterExpressions}")
        definitions.add(builder.toString())
    }

    private fun translate(pdec: PredDeclaration) {
        val builder = StringBuilder()
        val predCall: String
        val alloyAssertion: String
        if (pdec.decls.isEmpty()) {
            predCall = sanitizeIdentifier(pdec.name)
            alloyAssertion = predCall
        } else {
            val params = pdec.decls.joinToString(", ") { it.names.joinToString(",") { sanitizeIdentifier(it) } }
            predCall = "${sanitizeIdentifier(pdec.name)}($params)"
            alloyAssertion = "#($params).($predCall)"
        }
        val decls = pdec.decls.joinToString(" & ") { it.names.joinToString(" & ") {name -> "${sanitizeIdentifier(name)} <: ${translateExpression(it.expression)}" }}
        builder.append("$predCall == $decls")
        val blocks = pdec.expressions.joinToString(" & ") { translateExpression(it) }
        if (blocks.isNotEmpty()) {
            builder.append(" & $blocks")
        }
        definitions.add(builder.toString())
        // add existantially quantified predicate to be used for CheckStatement
        alloyAssertions[pdec.name] = alloyAssertion
    }

    private fun translate(stmt: CheckStatement) {
        stmt.expressions.forEach({ e ->
            when (e) {
                is IdentifierExpression -> if (alloyAssertions.containsKey(e.name)) assertions.add(alloyAssertions[e.name].orEmpty())
                else -> assertions.add(translateExpression(e))
            }
        })
    }

    private fun translate(sdec: SignatureDeclaration) {
        signatures.addAll(sdec.names) // used to decide how to translate dot join

        if (sdec.qualifiers.contains(ABSTRACT)) {
            abstractSignatures.addAll(sdec.names)
        }

        handleQuantifiersByCardinality(sdec)

        // field declarations are mapped to constants and properties
        sdec.decls.forEach({ d ->
            d.names.map { constants.add(sanitizeIdentifier(it)) }
            sdec.names.forEach { sdecName -> translateFieldDeclarations(d, sdecName) }
        })

        if (sdec.signatureExtension == null) {
            // basic signature -> B set
            sets.addAll(sdec.names.map { n -> sanitizeIdentifier(n) })
            return
        }

        // not a basic signature -> ensure subset relation with extended / extending signatures
        constants.addAll(sdec.names.map { n -> sanitizeIdentifier(n) })
        when (sdec.signatureExtension) {
            is NameSignatureExtension -> {
                sdec.names.forEach({
                    properties.add("${sanitizeIdentifier(it)} <: ${sanitizeIdentifier(sdec.signatureExtension.name)}")
                    extendingSignatures[sdec.signatureExtension.name] =
                            extendingSignatures.getOrDefault(sdec.signatureExtension.name, emptyList()) + it
                })
            }
            else -> throw UnsupportedOperationException(sdec.signatureExtension.javaClass.canonicalName)
        }
    }

    private fun handleQuantifiersByCardinality(sdec: SignatureDeclaration) {
        if (NO in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${sanitizeIdentifier(it)}) = 0") }
        }
        if (LONE in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${sanitizeIdentifier(it)}) <= 1") }
        }
        if (ONE in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${sanitizeIdentifier(it)}) = 1") }
        }
        if (SOME in sdec.qualifiers) {
            sdec.names.forEach { properties.add("card(${sanitizeIdentifier(it)}) >= 1") }
        }
    }

    private fun translateFieldDeclarations(decl: Decl, name: String) {
        decl.names.map {
            fields.add(it) // used to decide how to translate dot join
            if (decl.expression is QuantifiedExpression) {
                if (decl.expression.operator == LONE) {
                    // one-to-one mapping, i.e. function
                    properties.add("${sanitizeIdentifier(it)} : ${sanitizeIdentifier(name)} +-> ${translateExpression(decl.expression.expression)}")
                    return
                }
                properties.add("${sanitizeIdentifier(it)} : ${sanitizeIdentifier(name)} <-> ${translateExpression(decl.expression.expression)}")
                return
            }
            properties.add("${sanitizeIdentifier(it)} : ${sanitizeIdentifier(name)} <-> ${translateExpression(decl.expression)}")
        }
    }

    private fun translateExpression(e: Expression) =
            when (e) {
                is QuantifierExpression -> translateExpression(e)
                is IdentifierExpression -> sanitizeIdentifier(e)
                is BinaryOperatorExpression -> translateExpression(e)
                is UnaryOperatorExpression -> translateExpression(e)
                is LetExpression -> translateExpression(e)
                is BoxJoinExpression -> translateExpression(e)
                is IntegerSetExpression -> "INTEGER"
                is IntegerCastExpression -> translateExpression(e)
                is IntegerExpression -> translateExpression(e)
                is QuantifiedExpression -> translateExpression(e)
                else -> throw UnsupportedOperationException(e.javaClass.canonicalName)
            }

    private fun translateExpression(ie: IntegerExpression): String =
            ie.int.toString()

    private fun translateExpression(ice: IntegerCastExpression): String =
            translateExpression(ice.expr)

    private fun translateExpression(qe: QuantifierExpression): String =
            when (qe.operator) {
                ALL -> "!(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} => ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }})"
                NO -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) = 0"
                ONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) = 1"
                LONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) <= 1"
                SOME -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) > 0"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateExpression(le: LetExpression): String {
        val builder = StringBuilder()
        builder.append("LET ")
        builder.append(le.letDecls.joinToString(", ") { it -> sanitizeIdentifier(it.name) })
        builder.append(" BE ")
        builder.append(le.letDecls.joinToString(" & ") { it -> "${sanitizeIdentifier(it.name)} = ${translateExpression(it.expression)}" })
        builder.append(" IN ")
        builder.append(le.expressions.joinToString(" & ") { it -> translateExpression(it) })
        builder.append(" END")
        return builder.toString()
    }

    private fun translateExpression(bje: BoxJoinExpression): String {
        val parameters = bje.parameters.map { translateExpression(it) }
        return "${translateExpression(bje.left)}(${parameters.joinToString(", ")})"
    }

    private fun translateExpression(qe: BinaryOperatorExpression): String {
        val symbol: String
        when (qe.operator) {
            JOIN -> return translateJoin(qe)
            OVERRIDE -> symbol = "<+"
            DOM_RESTR -> symbol = "<|"
            RAN_RESTR -> symbol = "|>"
            IN -> symbol = "<:"
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
            IMPLICATION -> symbol = "=>"
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
                CARD -> "card(${translateExpression(qe.expression)})"
                INVERSE -> "${translateExpression(qe.expression)}~"
                NOT -> "not(${translateExpression(qe.expression)})"
                Operator.SET -> "POW(${translateExpression(qe.expression)})"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateExpression(qe: QuantifiedExpression): String =
            when (qe.operator) {
                NO -> "${translateExpression(qe.expression)} = {}"
                ONE -> "card(${translateExpression(qe.expression)}) = 1"
                LONE -> "card(${translateExpression(qe.expression)}) <= 1"
                SOME -> "card(${translateExpression(qe.expression)}) >= 1"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateJoin(je: BinaryOperatorExpression): String {
        if (je.left.type == UNTYPED || je.right.type == UNTYPED) {
            throw UnsupportedOperationException("missing types in join translation: ${je.left} . ${je.right}")
        }
        if (je.left.type == RELATION && je.right.type == RELATION) {
            return "(${translateExpression(je.left)} ; ${translateExpression(je.right)})"
        } else if (je.left.type == RELATION && je.right.type == Type.SET) {
            return "${translateExpression(je.left)}~[${translateExpression(je.right)}]"
        } else if (je.left.type == Type.SET && je.right.type == RELATION) {
            return "${translateExpression(je.right)}[${translateExpression(je.left)}]"
        }
        throw UnsupportedOperationException("join not supported this way")
    }

    private fun translateDeclsIDList(decls: List<Decl>) =
            decls.joinToString(", ") { it.names.joinToString(", ") { sanitizeIdentifier(it) } }

    private fun translateDeclsExprList(decls: List<Decl>) =
            decls.joinToString(" & ") { it.names.joinToString(" & ") {n -> "${sanitizeIdentifier(n)} <: ${translateExpression(it.expression)}" }}

    private fun sanitizeIdentifier(id: String) =
            "${id}_"

    private fun sanitizeIdentifier(id: IdentifierExpression) =
            sanitizeIdentifier(id.name)
}

