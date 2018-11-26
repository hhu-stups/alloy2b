package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4.ConstList
import edu.mit.csail.sdg.alloy4.Err
import edu.mit.csail.sdg.alloy4.Pair
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil

data class ParserResult(val prologTerm: String, val commandNames: HashSet<String>)

/**
 * Convert the abstract syntax tree of an Alloy model to a Prolog term.
 */
class Alloy2BParser {

    private val orderedSignatures = mutableListOf<String>()
    private val expressionTranslator = ExpressionToProlog(orderedSignatures)

    private lateinit var commands: ConstList<Command>

    private fun translateModule(module: CompModule): String {
        commands = module.allCommands
        val name = sanitizeIdentifier(module.modelName)
        val listOfFacts = module.allFacts.joinToString(",") { toPrologTerm(it) }
        val listOfAssertions = module.allAssertions.joinToString(",") { toPrologTerm(it) }
        val listOfCommands = commands.joinToString(",") { toPrologTerm(it) }
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

    @Throws(Err::class)
    fun parseFromFile(alloyModelPath: String): ParserResult {
        orderedSignatures.clear()
        val path = realPath(alloyModelPath)
        try {
            val astRoot = CompUtil.parseEverything_fromFile(A4Reporter(), null, path)
            astRoot.opens.forEach { collectPropertiesFromInclude(it) }

            val modules = astRoot.allReachableModules.joinToString(",", transform = ::translateModule)
            val rootModule = sanitizeIdentifier(astRoot.rootModule.modelName)
            val commandNames = hashSetOf<String>()
            astRoot.rootModule.allCommands
                    .forEachIndexed { i, cmd -> commandNames.add(if (cmd.check) "check$i" else "run$i") }
            return ParserResult("alloy($rootModule,[$modules]).", commandNames)
        } catch (exception: Err) {
            throw exception
        }
    }

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
                "bitwidth(${astNode.bitwidth}),maxseq(${astNode.maxseq}),index(${commands.indexOf(astNode)})," +
                "pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    private fun createSigScopeTuple(scope: CommandScope) =
            "(${sanitizeIdentifier(scope.sig.label)},${scope.startingScope})"

    private fun toPrologTerm(astNode: Func): String {
        val functor = if (astNode.isPred) "predicate" else "function"
        return "$functor(${sanitizeIdentifier(astNode.label)},${astNode.params().map { toPrologTerm(it) }}," +
                astNode.decls.map { expressionTranslator.toPrologTerm(it) } +
                ",${toPrologTerm(astNode.body)},pos(${astNode.pos.x},${astNode.pos.y}))"
    }

    private fun toPrologTerm(astNode: Expr) =
            astNode.accept(expressionTranslator)

    private fun toPrologTerm(astNode: Sig): String {
        return "signature(${sanitizeIdentifier(astNode.label)}," +
                "[${astNode.fieldDecls.joinToString(",") { expressionTranslator.toPrologTerm(it) }}]," +
                "[${astNode.facts.joinToString(",") { toPrologTerm(it) }}]," +
                "${collectSignatureOptionsToPrologList(astNode)},pos(${astNode.pos.x},${astNode.pos.y}))"
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
}
