package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import edu.mit.csail.sdg.alloy4.SafeList
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary
import edu.mit.csail.sdg.alloy4compiler.ast.Func
import edu.mit.csail.sdg.alloy4compiler.ast.Sig
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

class AlloyAstTranslation(spec: CompModule) {

    private val sets = mutableListOf<String>()
    private val constants = mutableListOf<String>()
    private val definitions = mutableListOf<String>()
    private val properties = mutableListOf<String>()
    private val assertions = mutableListOf<String>()
    private val operations = mutableListOf<String>()
    private val orderingAndScopeMap = mutableMapOf<String, Long>()
    private val signatureFieldParents = mutableMapOf<String, String>()
    private val translationPreferences = hashMapOf(
            TranslationPreference.ORDERED_UNORDERED_SIGNATURE_INTERACTION to mutableMapOf<String, String>(),
            TranslationPreference.DISTINCT_SIGNATURE_INTERACTION to mutableMapOf())

    private val exprTranslator = ExpressionTranslator(this)

    private var runCounter = 1
    private var checkCounter = 1
    private var globalScope = Pair<Boolean, Long>(false, 0)

    private val signatures = mutableListOf<IdentifierExpression>()
    private val signatureDeclarations = mutableMapOf<IdentifierExpression, SignatureDeclaration>()
    private val abstractSignatures = mutableListOf<IdentifierExpression>()
    private val singletonSignatures = mutableListOf<IdentifierExpression>() // singleton signatures that do not extend another signature
    private val extendingSignatures = mutableMapOf<IdentifierExpression, List<IdentifierExpression>>()
    private val parentSignature = mutableMapOf<IdentifierExpression, IdentifierExpression>()

    private val fields = mutableListOf<String>()

    private val alloyAssertions = mutableMapOf<String, String>()

    init {
        translateSignatures(spec.rootModule.allSigs)
        translateFunctions(spec.allFunc)
    }

    private fun translateSignatures(allSigs: SafeList<Sig>) {
        allSigs.forEach {
            val sanitizedSigName = sanitizeIdentifier(it.label)
            sets.add(sanitizedSigName)

            it.fields.forEach {
                signatureFieldParents[it.label] = sanitizedSigName
                val sanitizedFieldName = sanitizeIdentifier(it.label)
                constants.add(sanitizedFieldName)

                val declExpr = it.decl().expr
                var symbol = "<->"
                if (declExpr is ExprUnary) {
                    symbol = when (declExpr.op) {
                        ExprUnary.Op.LONE -> "+->" // one-to-one mapping, i.e. function, LONE = 0 or 1 target
                        ExprUnary.Op.ONEOF -> "-->" // one-to-one mapping, i.e. function, ONE = exactly 1 target
                        ExprUnary.Op.SETOF -> "<->"
                        else -> throw UnsupportedOperationException("Unary operator for field declaration of signature " +
                                "not implemented: ${declExpr.op}")
                    }
                }
                properties.add("${sanitizeIdentifier(it.label)} : " +
                        "${it.sig.accept(exprTranslator)} $symbol ${it.decl().expr.accept(exprTranslator)}")
            }
        }
    }

    private fun translateFunctions(allFunc: SafeList<Func>) {
        allFunc.forEach {
            val parameters = if (it.params().isEmpty()) "" else
                "(${it.params().joinToString(",") { it.accept(exprTranslator) }})"
            definitions.add("${sanitizeIdentifier(it.label)}$parameters == " +
                    "${it.decls.map { it.get().accept(exprTranslator) }
                            .zip(it.decls.map { it.expr.accept(exprTranslator) })
                            .map { "${it.first} ${it.second}" }
                            .joinToString(" & ")} & ${it.body.accept(exprTranslator)}")
        }
    }

    fun getTranslation(): String {
        val builder = StringBuilder()

        builder.appendln("/*@ generated */")
        builder.appendln("MACHINE alloytranslation")

        // define orderings since we know the scope now
        //defineDistinctOrderedSignatures(definitions, orderingAndScopeMap)

        val orderedUnorderedInteractions =
                translationPreferences[TranslationPreference.ORDERED_UNORDERED_SIGNATURE_INTERACTION]
        // ordered signatures are defined as intervals in the definitions instead of a deferred set
        if (orderingAndScopeMap.isEmpty() || orderedUnorderedInteractions!!.isEmpty()) {
            appendIfNotEmpty(builder, sets.filter { it !in orderingAndScopeMap }, "; ", "SETS")
        } else {
            appendIfNotEmpty(builder, sets.filter {
                it !in orderingAndScopeMap.keys
                        .union(orderedUnorderedInteractions.values)
                        .union(orderedUnorderedInteractions.keys)
            }, "; ", "SETS")
            //defineDeferredSetsAsSetsOfInteger(sets.filter {
            //    it !in orderedUnorderedInteractions.values
            //            .union(orderedUnorderedInteractions.keys)
            //            .subtract(orderingAndScopeMap.keys)
            //})
        }
        appendIfNotEmpty(builder, constants, ", ", "CONSTANTS")
        appendIfNotEmpty(builder, definitions, " ;\n    ", "DEFINITIONS")
        appendIfNotEmpty(builder, properties, " &\n    ", "PROPERTIES")
        appendIfNotEmpty(builder, assertions, " &\n    ", "ASSERTIONS")
        appendIfNotEmpty(builder, operations, ";\n    ", "OPERATIONS")

        builder.appendln("END")

        return builder.toString()
    }

    private fun appendIfNotEmpty(builder: StringBuilder, list: List<String>, delimiter: String, sectionName: String) {
        if (list.isEmpty()) {
            return
        }
        builder.appendln(sectionName)
        builder.appendln("    " + list.joinToString(delimiter))
    }

    /**
     * Concatenate underscore to all variables in order to prevent collision with B keywords
     * replace all ' with _ (allowed in Alloy)
     * remove this/
     */
    fun sanitizeIdentifier(id: String): String {
        val sanitizedIdentifier = "${id.replace("'", "_").replace("this/", "")}_"
        if (signatureFieldParents.keys.contains(id)) {
            // add parent signature name as a suffix to get unique names for signature fields
            return "$sanitizedIdentifier${signatureFieldParents[id]}"
        }
        return sanitizedIdentifier
    }
}


