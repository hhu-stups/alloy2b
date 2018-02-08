package de.hhu.stups.alloy2b.translation.prolog

import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4.Pair
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil

/**
 * Convert the abstract syntax tree of an Alloy model to a Prolog term.
 */
class AlloyAstToProlog(alloyModelPath: String) {

    /**
     *
     * The root of the prolog term is alloy_model/5:
     *      alloy_model(facts(ListOfAlloyFact),assertions(ListOfAssertion),commands(ListOfCommand),
     *                  functions(ListOfFunction),signatures(ListOfSignature))
     *
     * fact/2:
     *      fact(Expr,Pos)
     *
     * check/5, run/5:
     *      functor is either check or run
     *      functor(FormulaExpr,global_scope(GlobalScope),exact_scopes(ListOfSigAndScope),bitwidth(BitWidth),Pos)
     *
     * function/2:
     *      function(Name,Body)
     *
     * predicate/2:
     *      predicate(Name,Body)
     *
     * signature/5:
     *      signature(Name,ListOfFieldDecl,ListOfFact,Options,Pos)
     *      Options is a subset of [abstract,enum,meta,lone,one,private,some,subset,subsig,top_level]
     *
     * pos/2:
     *      tuple of x and y position
     *
     * Binary and unary operators are self-explanatory, for instance, a join is represented as the term join/3
     * with left and right expression and position information.
     *
     */

    private val prologTerm: String
    private val expressionTranslator = ExpressionToProlog(this)

    init {
        val res = object {}.javaClass.getResource(alloyModelPath)
        val astRoot = CompUtil.parseEverything_fromFile(A4Reporter(), null, res.file)
        val listOfFacts = astRoot.rootModule.allFacts
                .map { toPrologTerm(it) }.filter { it != "" }.joinToString(",")
        val listOfAssertions = astRoot.rootModule.allAssertions
                .map { toPrologTerm(it) }.filter { it != "" }.joinToString(",")
        val listOfCommands = astRoot.rootModule.allCommands
                .map { toPrologTerm(it) }.filter { it != "" }.joinToString(",")
        val listOfFunctions = astRoot.rootModule.allFunc
                .map { toPrologTerm(it) }.filter { it != "" }.joinToString(",")
        val listOfSignatures = astRoot.rootModule.allSigs
                .map { toPrologTerm(it) }.filter { it != "" }.joinToString(",")
        prologTerm = "alloy_model(facts([$listOfFacts]),assertions([$listOfAssertions]),commands([$listOfCommands])," +
                "functions([$listOfFunctions]),signatures([$listOfSignatures]))"
    }

    fun getPrologTerm(): String {
        return prologTerm
    }

    private fun toPrologTerm(astNode: Pair<String, Expr>?): String =
            // fact
            "fact(${astNode?.b?.accept(expressionTranslator)},(${astNode?.b?.pos?.x},${astNode?.b?.pos?.y}))"

    private fun toPrologTerm(astNode: Command?): String {
        // command
        if (astNode == null) {
            return ""
        }
        val functor = if (astNode.check) "check" else "run"
        return "$functor(${astNode.formula.accept(expressionTranslator)},global_scope(${astNode.overall})," +
                "exact_scopes(${astNode.additionalExactScopes.map { it.label }}),bitwidth(${astNode.bitwidth})," +
                "pos(${astNode.pos.x},${astNode.pos.y})"
    }

    private fun toPrologTerm(astNode: Func?): String {
        // function or predicate
        val functor = if (astNode?.isPred != false) {
            "predicate"
        } else {
            "function"
        }
        return "$functor(${astNode?.label?.toLowerCase()},${toPrologTerm(astNode?.body)})"
    }

    private fun toPrologTerm(astNode: Expr?): String {
        // expression
        if (astNode == null) {
            return ""
        }
        return astNode.accept(expressionTranslator)
    }

    private fun toPrologTerm(astNode: Sig?): String {
        // signature
        if (astNode == null) {
            return ""
        }
        return "signature(${astNode.label.toLowerCase()}," +
                "[${astNode.fieldDecls?.joinToString(",") { toPrologTerm(it) }}]," +
                "[${astNode.facts?.joinToString(",") { toPrologTerm(it) }}]," +
                "${collectSignatureOptionsToPrologList(astNode)},pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    fun toPrologTerm(astNode: Decl?): String {
        // declaration
        if (astNode == null) {
            return ""
        }
        return astNode.expr.accept(expressionTranslator)
    }

    private fun collectSignatureOptionsToPrologList(astNode: Sig?): String {
        val lstOptions = mutableListOf<String>()
        // TODO: how to recognize extending signature? subsig seems to be set all the time
        if (astNode?.isAbstract != null) {
            lstOptions.add("abstract")
        }
        if (astNode?.isEnum != null) {
            lstOptions.add("enum")
        }
        if (astNode?.isLone != null) {
            lstOptions.add("lone")
        }
        if (astNode?.isMeta != null) {
            lstOptions.add("meta")
        }
        if (astNode?.isOne != null) {
            lstOptions.add("one")
        }
        if (astNode?.isPrivate != null) {
            lstOptions.add("private")
        }
        if (astNode?.isSome != null) {
            lstOptions.add("some")
        }
        if (astNode?.isSubset != null) {
            lstOptions.add("subset")
        }
        if (astNode?.isSubsig != null) {
            lstOptions.add("subsig")
        }
        if (astNode?.isTopLevel != null) {
            lstOptions.add("toplevel")
        }
        return lstOptions.toString()
    }
}

