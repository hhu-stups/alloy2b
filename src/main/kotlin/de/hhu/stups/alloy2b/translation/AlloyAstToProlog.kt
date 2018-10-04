package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4.Pair
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil

/**
 * Convert the abstract syntax tree of an Alloy model to a Prolog term.
 */
class AlloyAstToProlog(alloyModelPath: String) {

    /**
     *
     * The root of the prolog term is alloy/2:
     *      alloy(RootModule, ListOfModels).
     *
     * Each module model is translated to an alloy_model/5 prolog term.
     *      alloy_model(ModuleName,facts(ListOfAlloyFact),assertions(ListOfAssertion),commands(ListOfCommand),
     *                  functions(ListOfFunction),signatures(ListOfSignature),ordered_signatures(ListOfAtoms))
     *
     * fact/2:
     *      fact(Expr,Pos)
     *
     * field/3:
     *      field(Name,Expr,Options,Pos)
     *      Options are only 'disj' by now
     *
     * check/5, run/5 (functor is either check or run):
     *      functor(FormulaExpr,global_scope(GlobalScope),exact_scopes(ListOfSigAndScope),
     *      upper_bound_scopes(UpperBoundScopes),bitwidth(BitWidth),maxseq(MaxSeqSize),Pos)
     *
     * function/5, predicate/5 (functor is either function or predicate):
     *      functor(Name,Params,Decls,Body,Pos)
     *
     * signature/5:
     *      signature(Name,ListOfFieldDecl,ListOfFact,Options,Pos)
     *      Options is a subset of [abstract,enum,meta,lone,one,private,some,subset,subsig,top_level]
     *
     * pos/2:
     *      tuple of x and y position in the Alloy file
     *
     * Binary and unary operators are self-explanatory, for instance, a join is represented as the term join/4
     * with left and right expression as well as type and position information like
     * join(Lhs,Rhs,type/1,pos/2).
     *
     */

    private var prologTerm = ""

    private val orderedSignatures = mutableListOf<String>()
    private val expressionTranslator = ExpressionToProlog(this, orderedSignatures)

    init {
        val path = realPath(alloyModelPath)
        val astRoot = CompUtil.parseEverything_fromFile(A4Reporter(), null, path)

        astRoot.opens.forEach { collectPropertiesFromInclude(it) }

        val modules = astRoot.allReachableModules.joinToString(",", transform = ::translateModule)
        val rootModule = sanitizeIdentifier(astRoot.rootModule.modelName)
        prologTerm = "alloy($rootModule,[$modules])."
    }

    private fun translateModule(module: CompModule): String {
        val name = sanitizeIdentifier(module.modelName)
        val listOfFacts = module.allFacts.joinToString(",") { toPrologTerm(it) }
        val listOfAssertions = module.allAssertions.joinToString(",") { toPrologTerm(it) }
        val listOfCommands = module.allCommands.joinToString(",") { toPrologTerm(it) }
        val listOfFunctions = module.allFunc.joinToString(",") { toPrologTerm(it) }
        val listOfSignatures = module.allSigs.joinToString(",") { toPrologTerm(it) }
        return "alloy_model($name,facts([$listOfFacts]),assertions([$listOfAssertions]),commands([$listOfCommands])," +
                "functions([$listOfFunctions]),signatures([$listOfSignatures])," +
                "ordered_signatures(${orderedSignatures.map { "'$it'" }}))"
    }

    private fun realPath(alloyModelPath: String): String {
        return try {
            // either from resources
            object {}.javaClass.getResource(alloyModelPath).file
        } catch (exception: IllegalStateException) {
            // or an absolute path
            alloyModelPath
        }
    }

    private fun collectPropertiesFromInclude(it: CompModule.Open) {
        if ("util/ordering" == it.filename) {
            val prefixedSignatures = it.args.map { it.replace("this/", "") }
            orderedSignatures.addAll(prefixedSignatures)
            orderedSignatures.add(it.alias)
        }
    }

    fun getPrologTerm() = prologTerm

    private fun toPrologTerm(astNode: Pair<String, Expr>) =
            "fact(${astNode.b?.accept(expressionTranslator)},(${astNode.b?.pos?.x},${astNode.b?.pos?.y}))"

    private fun toPrologTerm(astNode: Command): String {
        val functor = if (astNode.check) "check" else "run"
        val exactScopes = astNode.scope.asSequence().filter { it.isExact }
                .map { createSigScopeTuple(it) }.toList()
        val upperBoundScopes = astNode.scope.asSequence().filter { !it.isExact }
                .map { createSigScopeTuple(it) }.toList()
        return "$functor(${astNode.formula.accept(expressionTranslator)},global_scope(${astNode.overall})," +
                "exact_scopes($exactScopes)," +
                "upper_bound_scopes($upperBoundScopes)," +
                "bitwidth(${astNode.bitwidth}),maxseq(${astNode.maxseq}),pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    private fun createSigScopeTuple(scope: CommandScope) =
            "(${sanitizeIdentifier(scope.sig.label)},${scope.startingScope})"

    private fun toPrologTerm(astNode: Func): String {
        val functor = if (astNode.isPred) "predicate" else "function"
        return "$functor(${sanitizeIdentifier(astNode.label)},${astNode.params().map { toPrologTerm(it) }}," +
                astNode.decls.map { toPrologTerm(it) } +
                ",${toPrologTerm(astNode.body)},pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    private fun toPrologTerm(astNode: Expr) =
            astNode.accept(expressionTranslator)

    private fun toPrologTerm(astNode: Sig): String {
        return "signature(${sanitizeIdentifier(astNode.label)}," +
                "[${astNode.fieldDecls.joinToString(",") { toPrologTerm(it) }}]," +
                "[${astNode.facts.joinToString(",") { toPrologTerm(it) }}]," +
                "${collectSignatureOptionsToPrologList(astNode)},pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    fun toPrologTerm(astNode: Decl): String {
        val options = if (astNode.disjoint != null || astNode.disjoint2 != null) "[disj]" else "[]"
        if (astNode.names.size > 1) {
            // a field declaration may has several names, for instance:
            // "sig State { near, far: set Object }" has one Decl with one Expr but two names near and far
            // we then have to define Expr for each name
            return astNode.names.joinToString(",") {
                "field(${sanitizeIdentifier(it.label)},${astNode.expr.accept(expressionTranslator)}," +
                        "${getType(astNode.expr.type())},$options,pos(${astNode.get().pos.x},${astNode.get().pos.y}))"
            }
        }
        return "field(${sanitizeIdentifier(astNode.get().label)},${astNode.expr.accept(expressionTranslator)}," +
                "${getType(astNode.expr.type())},$options,pos(${astNode.get().pos.x},${astNode.get().pos.y}))"
    }

    private fun collectSignatureOptionsToPrologList(astNode: Sig): String {
        val lstOptions = mutableListOf<String>()
        if (orderedSignatures.contains(astNode.label.replace("this/", ""))) {
            lstOptions.add("ordered")
        }
        if (astNode.isAbstract != null) {
            lstOptions.add("abstract")
        }
        if (astNode.isEnum != null) {
            lstOptions.add("enum")
        }
        if (astNode.isLone != null) {
            lstOptions.add("lone")
        }
        if (astNode.isMeta != null) {
            lstOptions.add("meta")
        }
        if (astNode.isOne != null) {
            lstOptions.add("one")
        }
        if (astNode.isPrivate != null) {
            lstOptions.add("private")
        }
        if (astNode.isSome != null) {
            lstOptions.add("some")
        }
        if (astNode.isSubset != null) {
            lstOptions.add("subset(${(astNode as Sig.SubsetSig).parents.map { sanitizeIdentifier(it.label) }})")
        }
        if (isExtendingSignature(astNode)) {
            lstOptions.add("subsig(${sanitizeIdentifier((astNode as Sig.PrimSig).parent.label)})")
        }
        return lstOptions.toString()
    }

    private fun isExtendingSignature(sig: Sig): Boolean {
        val isSubSig = sig.isSubsig
        return sig is Sig.PrimSig && isSubSig != null && (isSubSig.x != isSubSig.x2 || isSubSig.y != isSubSig.y2)
    }

    /**
     * Replace ticks by underscores and use single quotes for identifiers since strings with capital letter first are
     * variables in Prolog. The arity is also added to the Prolog term.
     */
    fun sanitizeIdentifier(identifier: String): String {
        if (identifier == "this") {
            // field declarations of signature may be joined with 'this' leading to a universal quantification in B
            return identifier
        }
        return identifier.replace("'", "_").replace("{", "").replace("}", "")
                .split("/").asSequence().filter { it != "this" }
                .joinToString("") { "'$it'" }
    }

    fun getType(type: Type): String {
        val tType = type.map { splitAndCleanType(it) }
        return "type(${if (tType.isEmpty()) "[untyped]" else tType.toString()},${type.arity()})"
    }

    private fun splitAndCleanType(innerType: Type.ProductType?) =
            innerType.toString().split("->").map { it ->
                sanitizeIdentifier(it.replace("{", "").replace("}", ""))
            }
}
