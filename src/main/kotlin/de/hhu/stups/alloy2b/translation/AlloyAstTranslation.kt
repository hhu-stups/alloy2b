package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.ConstList
import edu.mit.csail.sdg.alloy4.Pair
import edu.mit.csail.sdg.alloy4.SafeList
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

class AlloyAstTranslation(spec: CompModule) {

    private val sets = mutableSetOf<String>()
    private val constants = mutableSetOf<String>()
    private val definitions = mutableSetOf<String>()
    private val properties = mutableSetOf<String>()
    private val assertions = mutableSetOf<String>()
    private val operations = mutableSetOf<String>()
    private val alloyAssertions = mutableMapOf<String, String>()

    private val signatures = mutableSetOf<String>()
    private val predsAndFuncs = mutableSetOf<String>()
    private val abstractSignatures = mutableSetOf<String>()
    private val singletonSignatures = mutableSetOf<String>()
    private val extendingSignatures = mutableMapOf<String, MutableList<String>>()

    private val exprTranslator = ExpressionTranslator(this)

    private var commandCounter = 0

    init {
        translateSignatures(spec.rootModule.allSigs)
        translateFunctions(spec.allFunc)
        translateFacts(spec.allFacts)
        translateAssertions(spec.allAssertions)
        translateCommands(spec.allCommands)
        addSignatureExtensionProperties()
    }

    fun getTranslation(): String {
        val builder = StringBuilder()

        builder.appendln("/*@ generated */")
        builder.appendln("MACHINE alloytranslation")

        appendIfNotEmpty(builder, sets, "; ", "SETS")
        appendIfNotEmpty(builder, constants, ", ", "CONSTANTS")
        appendIfNotEmpty(builder, definitions, " ;\n    ", "DEFINITIONS")
        appendIfNotEmpty(builder, properties, " &\n    ", "PROPERTIES")
        appendIfNotEmpty(builder, assertions, " &\n    ", "ASSERTIONS")
        appendIfNotEmpty(builder, operations, ";\n    ", "OPERATIONS")

        builder.appendln("END")

        return builder.toString()
    }

    private fun translateCommands(allCommands: ConstList<Command>) {
        allCommands.forEach {
            // additional scope for run commands
            val tScope = if (!it.check) translateScopes(it) else ""
            val prefix = if (!it.check) "run" else "check"
            // check and run
            val alloyAssertion = alloyAssertions[sanitizeIdentifier(it.label)]
            if (alloyAssertion != null) {
                operations.add("${prefix}_${it.label} = PRE $tScope${alloyAssertion} THEN skip END")
                return
            }
            val sanitizedSigName = sanitizeIdentifier(it.label)
            val formula: String
            val suffix: String
            if (predsAndFuncs.contains(sanitizedSigName)) {
                formula = sanitizedSigName
                suffix = sanitizedSigName
            } else {
                formula = it.formula.accept(exprTranslator)
                suffix = (++commandCounter).toString()
            }
            operations.add("${prefix}_$suffix = PRE $tScope$formula THEN skip END")
            return
        }
    }

    private fun translateScopes(runCommand: Command): String {
        if (runCommand.check) {
            return ""
        }
        // Alloy already checks that a scope is only defined once so we do not have to deal with this
        val tScopes = mutableSetOf<String>()
        val scopedSignatures = mutableSetOf<String>()
        runCommand.scope.forEach { commandScope ->
            if (commandScope.sig != null && commandScope.sig.label != "Int") {
                // scope refers to a signature
                val sanitizedSigName = sanitizeIdentifier(commandScope.sig.label)
                scopedSignatures.add(sanitizedSigName)
                // TODO: we do not consider endingScope by now, what is the Alloy syntax?
                tScopes.add(getScopeCmpOp(sanitizedSigName, commandScope))
            }
            if (commandScope.sig != null && commandScope.sig.label == "Int") {
                // bitwidth
                setMaxAndMinInt(commandScope.startingScope)
            }
        }
        // set global scopes for the remaining signatures if defined
        if (runCommand.overall != -1) {
            // TODO: how to determine if the global scope in runcommand.overall is exact?
            signatures.subtract(scopedSignatures)
                    .forEach { tScopes.add(getScopeCmpOp(it, true, runCommand.overall)) }
        }
        return if (tScopes.isNotEmpty()) tScopes.joinToString(" & ") + " & " else ""
    }

    private fun translateAssertions(allAssertions: ConstList<Pair<String, Expr>>) {
        allAssertions.forEach {
            alloyAssertions[it.a] = "/* ${it.a} */ " + it.b.accept(exprTranslator)
        }
    }

    private fun translateFacts(allFacts: SafeList<Pair<String, Expr>>) {
        allFacts.forEach { properties.add(it.b.accept(exprTranslator)) }
    }

    private fun translateSignatures(allSigs: SafeList<Sig>) {
        allSigs.forEach {
            val sanitizedSigName = sanitizeIdentifier(it.label)
            signatures.add(sanitizedSigName)
            if (it is Sig.SubsetSig || isExtendingSignature(it)) {
                constants.add(sanitizedSigName)
            } else {
                sets.add(sanitizedSigName)
            }

            it.fields.forEach {
                val sanitizedFieldName = sanitizeIdentifier(it.label) + sanitizeIdentifier(it.sig.label)
                constants.add(sanitizedFieldName)

                val declExpr = it.decl().expr
                val symbol = getUnaryDeclSymbol(declExpr)
                properties.add("$sanitizedFieldName : " +
                        "${it.sig.accept(exprTranslator)} $symbol ${it.decl().expr.accept(exprTranslator)}")
            }
            if (it is Sig.SubsetSig) {
                // in
                it.parents.forEach { parent ->
                    val sanitizedParent = sanitizeIdentifier(parent.label)
                    properties.add("$sanitizedSigName : $sanitizedParent")
                }
            }
            if (isExtendingSignature(it)) {
                // extends
                val sanitizedParent = sanitizeIdentifier((it as Sig.PrimSig).parent.label)
                val subSigs = extendingSignatures[sanitizedParent]
                if (subSigs != null) {
                    subSigs.add(sanitizedSigName)
                } else {
                    extendingSignatures.put(sanitizedParent, mutableListOf(sanitizedSigName))
                }
                properties.add("$sanitizedSigName : ${sanitizeIdentifier(sanitizedParent)}")
            }
            if (it.isAbstract != null) {
                abstractSignatures.add(sanitizedSigName)
            }

            if (it.isOne != null) {
                singletonSignatures.add(sanitizedSigName)
            }
        }
    }

    private fun translateFunctions(allFunc: SafeList<Func>) {
        allFunc.forEach {
            val parameters = if (it.params().isEmpty()) "" else
                "(${it.params().joinToString(",") { it.accept(exprTranslator) }})"
            val tDecls = it.decls.map { it.get().accept(exprTranslator) }
                    .zip(it.decls.map { it.expr.accept(exprTranslator) })
                    .joinToString(" & ") { "${it.first} ${it.second}" }
            val sanitizedName = sanitizeIdentifier(it.label)
            definitions.add("$sanitizedName$parameters == " +
                    if (tDecls.isNotEmpty()) "$tDecls &" else "" + getBodyFromFunction(it))
            predsAndFuncs.add(sanitizedName)
        }
    }

    private fun addSignatureExtensionProperties() {
        // two signatures extending the same base signature are distinct
        extendingSignatures.values.forEach({
            for (i1 in 0 until it.size) {
                for (i2 in i1 until it.size) {
                    addSignatureExtensionIfNotEqual(it[i1], it[i2])
                }
            }
        })
        // abstract signatures are exhaustively divided into their sub signatures
        abstractSignatures.forEach({ absSigName ->
            if (extendingSignatures[absSigName] != null && extendingSignatures[absSigName]!!.isNotEmpty()) {
                properties.add("${extendingSignatures[absSigName]
                        ?.map { if (singletonSignatures.contains(it)) "{$it}" else it } // consider singleton sets
                        ?.joinToString(" \\/ ") { it }} = $absSigName")
            }
        })
    }

    private fun addSignatureExtensionIfNotEqual(sig1: String, sig2: String) {
        if (sig1 == sig2) {
            return
        }
        if (singletonSignatures.contains(sig1) && singletonSignatures.contains(sig2)) {
            // inequality if both are singletons
            properties.add("$sig1 /= $sig2")
            return
        }
        properties.add("$sig1 /\\ $sig2 = {}")
    }

    private fun appendIfNotEmpty(builder: StringBuilder, list: Set<String>, delimiter: String, sectionName: String) {
        if (list.isEmpty()) {
            return
        }
        builder.appendln(sectionName)
        builder.appendln("    " + list.joinToString(delimiter))
    }


    private fun getScopeCmpOp(sanitizedSigName: String, runCommand: CommandScope) =
            getScopeCmpOp(sanitizedSigName, runCommand.isExact, runCommand.startingScope)

    private fun getScopeCmpOp(sanitizedSigName: String, isExact: Boolean, scope: Int) =
            "card($sanitizedSigName) " + if (isExact) "= " else "<= " + scope

    private fun getBodyFromFunction(func: Func): String {
        val tBody = func.body.accept(exprTranslator)
        return if (tBody.isEmpty()) "1=1" else tBody
    }

    private fun setMaxAndMinInt(scope: Int) {
        // TODO: if there are several run commands defining a bitwidth, we need to set min and max int at runtime, i.e. in the precondition of the corresponding operation
        val scope1 = scope - 1
        val bound = Math.pow(2.toDouble(), scope1.toDouble())
        val maxInt = bound - 1
        val minInt = bound * -1
        // set min and max int for ProB
        definitions.add("SET_PREF_MAXINT == ${maxInt.toInt()}")
        definitions.add("SET_PREF_MININT == ${minInt.toInt()}")
    }

    private fun getUnaryDeclSymbol(declExpr: Expr): String {
        if (declExpr is ExprUnary) {
            return when (declExpr.op) {
                ExprUnary.Op.LONEOF -> "+->" // one-to-one mapping, i.e. function, LONE = 0 or 1 target
                ExprUnary.Op.ONEOF -> "-->"  // one-to-one mapping, i.e. function, ONE = exactly 1 target
                ExprUnary.Op.SETOF -> "<->"
                else -> throw UnsupportedOperationException("Unary operator for field declaration of signature " +
                        "not implemented: ${declExpr.op}")
            }
        }
        return "<->"
    }

    /**
     * Concatenate underscore to all variables in order to prevent collision with B keywords
     * replace all ' with _ (allowed in Alloy)
     * remove this/
     */
    fun sanitizeIdentifier(id: String): String {
        if (id == "univ" || id == "iden") {
            return id
        }
        val sanitizedIdentifier = "${id.replace("'", "_").replace("this/", "")}_"
        if (singletonSignatures.contains(sanitizedIdentifier)) {
            return "{$sanitizedIdentifier}"
        }
        return sanitizedIdentifier
    }

    fun isExtendingSignature(sig: Sig): Boolean {
        // TODO: there is most likely a better way to identify extending signatures
        val isSubSig = sig.isSubsig
        return sig is Sig.PrimSig && isSubSig != null && (isSubSig.x != isSubSig.x2 || isSubSig.y != isSubSig.y2)
    }

}