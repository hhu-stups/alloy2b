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
    private val translationPreferences = hashMapOf(
            TranslationPreference.ORDERED_UNORDERED_SIGNATURE_INTERACTION to mutableMapOf<String, String>(),
            TranslationPreference.DISTINCT_SIGNATURE_INTERACTION to mutableMapOf())

    private val exprTranslator = ExpressionTranslator()

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
            sets.add(sanitizeIdentifier(it.label))

            it.fields.forEach {
                constants.add(sanitizeIdentifier(it.label))

                val declExpr = it.decl().expr
                var symbol = "<->"
                if(declExpr is ExprUnary) {
                    symbol = when (declExpr.op) {
                                ExprUnary.Op.LONE -> "+->" // one-to-one mapping, i.e. function, LONE = 0 or 1 target
                                ExprUnary.Op.ONEOF -> "-->" // one-to-one mapping, i.e. function, ONE = exactly 1 target
                                ExprUnary.Op.SETOF -> "<->"
                                else -> TODO("implementation missing")
                            }
                }
                properties.add("${sanitizeIdentifier(it.label)} : ${it.sig.accept(exprTranslator)} $symbol ${it.decl().expr.accept(exprTranslator)}")
            }
        }
    }

    private fun translateFunctions(allFunc: SafeList<Func>) {
        allFunc.forEach {
            val parameters = if (it.params().isEmpty()) "" else "(${it.params().map { it.accept(exprTranslator) }.joinToString(",")})"
            definitions.add("${sanitizeIdentifier(it.label)}$parameters == ${it.body.accept(exprTranslator)}")
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
     * replace all / with _ (flatten Alloy pathes)
     */
    private fun sanitizeIdentifier(id: String) =
            "${id.replace("'", "_").replace("/","_")}_"
}


