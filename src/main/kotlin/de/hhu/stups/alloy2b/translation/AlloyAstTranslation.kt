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

    private val predsAndFuncs = mutableSetOf<String>()
    private val signatures = mutableSetOf<Expr>()
    private val abstractSignatures = mutableSetOf<Expr>()
    private val extendingSignatures = mutableMapOf<Expr, MutableList<Expr>>()

    private val singletonAnnotator = AlloyAstSingletonAnnotator(spec)
    private val exprTranslator = ExpressionTranslator(this, singletonAnnotator)

    private var commandCounter = 0

    init {
        //
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
        // TODO: precondition has been negated in the prior implementation, why?
        allCommands.forEach {
            // additional scope for run commands
            val tScope = if (!it.check) translateScopes(it) else ""
            val prefix = if (!it.check) "run" else "check"
            // check and run
            val alloyAssertion = alloyAssertions[sanitizeIdentifier(it.label)]
            if (alloyAssertion != null) {
                operations.add("${prefix}_${it.label} = PRE $tScope$alloyAssertion THEN skip END")
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
            // we skip the bitwidth here
        }
        // set global scopes for the remaining signatures if defined
        if (runCommand.overall != -1) {
            // TODO: how to determine if the global scope in runcommand.overall is exact?
            signatures.subtract(scopedSignatures)
                    .forEach {
                        tScopes.add(getScopeCmpOp(sanitizeIdentifier((it as Sig).label), true,
                                runCommand.overall))
                    }
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
            signatures.add(it)
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
                val rhs = if (declExpr is ExprUnary) declExpr.sub.accept(exprTranslator) else
                    declExpr.accept(exprTranslator)
                properties.add("$sanitizedFieldName : ${it.sig.accept(exprTranslator)} $symbol $rhs")
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
                val subSigs = extendingSignatures[it.parent]
                if (subSigs != null) {
                    subSigs.add(it)
                } else {
                    extendingSignatures[it.parent] = mutableListOf()
                    extendingSignatures[it.parent]?.add(it)
                }
                properties.add("$sanitizedSigName : $sanitizedParent")
            }
            if (it.isAbstract != null) {
                abstractSignatures.add(it)
            }
        }
    }

    private fun translateFunctions(allFunc: SafeList<Func>) {
        allFunc.forEach {
            val parameters = if (it.params().isEmpty()) "" else
                "(${it.params().joinToString(",") { it.accept(exprTranslator) }})"
            val tDecls = mutableSetOf<String>()
            it.decls.forEach { decl ->
                val declExpr = decl.expr
                val declLhs = sanitizeTypedIdentifier(decl.get())
                if (declExpr is ExprUnary) {
                    when {
                        declExpr.op == ExprUnary.Op.SETOF ->
                            tDecls.add("$declLhs <: ${decl.expr.accept(exprTranslator)}")
                        declExpr.op == ExprUnary.Op.ONE ->
                            tDecls.add("$declLhs <: ${decl.expr.accept(exprTranslator)} & card(${decl.get()}) = 1")
                        declExpr.op == ExprUnary.Op.SOME ->
                            tDecls.add("$declLhs <: ${decl.expr.accept(exprTranslator)} & card(${decl.get()}) > 0")
                        declExpr.op == ExprUnary.Op.LONE ->
                            tDecls.add("$declLhs <: ${decl.expr.accept(exprTranslator)} & card(${decl.get()}) <= 1")
                        else -> throw UnsupportedOperationException("Field declaration not implemented for unary " +
                                "operator ${declExpr.op}.")
                    }
                }
            }
            val sanitizedName = sanitizeIdentifier(it.label)
            val decls = if (tDecls.isNotEmpty()) "${tDecls.joinToString(" & ")} & " else ""
            definitions.add("$sanitizedName$parameters == ($decls ${getBodyFromFunction(it)})")
            predsAndFuncs.add(sanitizedName)
        }
    }

    private fun addSignatureExtensionProperties() {
        // two signatures extending the same base signature are distinct
        extendingSignatures.values.forEach({ sigs ->
            for (i1 in 0 until sigs.size) {
                for (i2 in i1 until sigs.size) {
                    addSignatureExtensionIfNotEqual(sigs[i1], sigs[i2])
                }
            }
        })
        // abstract signatures are exhaustively divided into their sub signatures
        abstractSignatures.forEach({ absSigName ->
            val extendingSigs = extendingSignatures[absSigName]
            if (extendingSigs != null && extendingSigs.isNotEmpty()) {
                properties.add("${extendingSigs
                        .map { sanitizeTypedIdentifier(it) }
                        .joinToString(" \\/ ") { it }} = ${sanitizeTypedIdentifier(absSigName)}")
            }
        })
    }

    private fun addSignatureExtensionIfNotEqual(sig1: Expr, sig2: Expr) {
        val sanitizedSig1Name = sanitizeTypedIdentifier(sig1)
        val sanitizedSig2Name = sanitizeTypedIdentifier(sig2)
        if (sig1 == sig2) {
            return
        }
        if (singletonAnnotator.isSingleton(sig1) && singletonAnnotator.isSingleton(sig2)) {
            // inequality if both are singletons
            properties.add("$sanitizedSig1Name /= $sanitizedSig2Name")
            return
        }
        properties.add("$sanitizedSig1Name /\\ $sanitizedSig2Name = {}")
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
    fun sanitizeIdentifier(id: String): String =
            if (id == "univ" || id == "iden") id else "${id.replace("'", "_").replace("this/", "")}_"

    fun sanitizeTypedIdentifier(label: String, isSingleton: Boolean) =
            if (isSingleton) "{${sanitizeIdentifier(label)}}" else sanitizeIdentifier(label)

    fun sanitizeTypedIdentifier(id: Expr): String =
            sanitizeTypedIdentifier(id.toString(), singletonAnnotator.isSingleton(id))

    private fun isExtendingSignature(sig: Sig): Boolean {
        // TODO: there is most likely a better way to identify extending signatures
        val isSubSig = sig.isSubsig
        return sig is Sig.PrimSig && isSubSig != null && (isSubSig.x != isSubSig.x2 || isSubSig.y != isSubSig.y2)
    }

}