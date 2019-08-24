package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionToProlog(private val signatures: MutableList<Sig>,
                         private val orderedSignatures: MutableList<String>) : VisitReturn<String>() {

    var usesSequences = false

    /**
     * Translate a field declaration to a Prolog term.
     */
    fun toPrologTerm(astNode: Decl): String {
        val options = if (astNode.disjoint != null || astNode.disjoint2 != null) "[disj]" else "[]"
        if (astNode.names.size > 1) {
            // a field declaration may has several names, for instance:
            // "sig State { near, far: set Object }" has one Decl with one Expr but two names near and far
            // we then have to define Expr for each name
            return astNode.names.joinToString(",") {
                "field(identifier(${sanitizeIdentifier(it.label)},${getType(it.type())}," +
                        "pos(${it.pos?.x},${it.pos?.y}))," +
                        "${astNode.expr.accept(this)},${getType(astNode.expr.type())},$options," +
                        "pos(${astNode.get().pos.x},${astNode.get().pos.y}))"
            }
        }
        val astNodeE = astNode.get()
        return "field(identifier(${sanitizeIdentifier(astNodeE.label)},${getType(astNodeE.type())}," +
                "pos(${astNodeE.pos?.x},${astNodeE.pos?.y})),${astNode.expr.accept(this)}," +
                "${getType(astNode.expr.type())},$options,pos(${astNode.get().pos.x},${astNode.get().pos.y}))"
    }

    override fun visit(p0: ExprBinary): String {
        val left = p0.left
        val tLeft = left.accept(this)
        val right = p0.right
        val tRight = right.accept(this)
        // we define ordered signatures as sets of integer, if an ordered signature interacts with an unordered one
        // both have to be defined as a set of integer
        val leftType = left.type().toString()
        val rightType = right.type().toString()
        if (orderedSignatures.contains(leftType) && !orderedSignatures.contains(rightType)) {
            orderedSignatures.add(rightType)
        } else if (orderedSignatures.contains(rightType) && !orderedSignatures.contains(leftType)) {
            orderedSignatures.add(leftType)
        }
        // TODO: check for univ type
        return "${getOperator(p0.op.toString())}($tLeft,$tRight," +
                "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"
    }

    override fun visit(p0: ExprList) =
            "${getOperator(p0.op.toString())}(${p0.args.map { it.accept(this) }},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprCall): String {
        val functor = if (p0.`fun`.isPred) "pred_call" else "fun_call"
        val name = sanitizeIdentifier(p0.`fun`.label)
        // flatten types for ordering function calls to the signature name
        val type = if (name.startsWith("'ordering_'") || name.endsWith("'first_'") || name.endsWith("'last_'"))
            "type([${sanitizeIdentifier(p0.type().toString().split("->").first())}]," +
                    "${p0.type().arity()})" else getType(p0.type())
        return "$functor($name,${p0.args?.map { it.accept(this) }}," +
                "$type,pos(${p0.pos?.x},${p0.pos?.y}))"
    }

    override fun visit(p0: ExprConstant): String {
        if (p0.type().is_int) {
            return "integer($p0,pos(${p0.pos.x},${p0.pos.y}))"
        }
        if (p0.type().is_bool) {
            return "boolean($p0,pos(${p0.pos.x},${p0.pos.y}))"
        }
        if (p0.toString().matches(Regex("\".*\""))) {
            return "string($p0,pos(${p0.pos.x},${p0.pos.y}))"
        }
        return "$p0(pos(${p0.pos.x},${p0.pos.y}))"
    }

    override fun visit(p0: ExprITE) =
            "if_then_else(${p0.cond?.accept(this)},${p0.left?.accept(this)},${p0.right?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprLet) =
            // a let with multiple variables is split into several let expressions by Alloy each having only one var
            "let(identifier(${sanitizeIdentifier(p0.`var`.label)},${getType(p0.`var`.type())}," +
                    "pos(${p0.`var`.pos?.x},${p0.`var`.pos?.y}))," +
                    "${p0.expr?.accept(this)},${p0.sub?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprQt): String {
        val params = p0.decls?.flatMap { decl -> decl.names.map { "identifier(${sanitizeIdentifier(it.label)},${getType(decl.expr.type())}," +
                "pos(${p0.pos?.x},${p0.pos?.y}))" } }
        return "${getOperator(p0.op.toString())}($params," +
                "${p0.decls?.map { toPrologTerm(it) }}," +
                "${p0.sub?.accept(this)},${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"
    }

    override fun visit(p0: ExprUnary): String =
            when (p0.op) {
                ExprUnary.Op.NOOP -> p0.sub.accept(this) as String
                else -> "${getOperator(p0.op.toString())}(${p0.sub.accept(this)}," +
                        "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"
            }

    override fun visit(p0: ExprVar) = "identifier(${sanitizeIdentifier(p0.label)}," +
            "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: Sig) = "identifier(${sanitizeIdentifier(p0.label)}," +
            "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: Sig.Field) = "identifier(${sanitizeIdentifier(p0.label)}," +
            "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    private fun getOperator(op: String): String {
        val operator = try {
            Operator.toKeyword(Operator.fromString(op))
        } catch (exception: UnsupportedOperationException) {
            System.err.println(exception)
            op.toLowerCase().replace(" ", "")
        }
        return "'$operator'"
    }

    /**
     * Transform the type of an Alloy ast node (like t1->t2->..->tn, with arity n) to a Prolog list.
     */
    private fun splitAndCleanType(type: Type): List<String> {
        if (type.size() == 0) {
            arrayListOf<String>()
        }
        return generalizeTypes(type)
    }

    private fun generalizeType(type: Type): String {
        val typeString = type.toString()
        val parents = signatures.filter { type.isSubtypeOf(it.type()) }
        if (type.toString().contains("/Ord")) {
            return "'Ordering'"
        }
        if (parents.isEmpty() || typeString == "{Int}" || typeString == "{univ}") {
            return cleanUpType(type)
        }
        return cleanUpType(getMostGeneralType(parents))
    }

    private fun generalizeTypes(type: Type): MutableList<String> {
        if (type.arity() == 0) {
            return arrayListOf(cleanUpType(type))
        }
        val typeList = arrayListOf<String>()
        val firstType = type.first()
        for (i in 0 until type.arity()) {
            val sig = firstType.get(i)
            typeList.add(generalizeType(sig.type()))
        }
        return typeList
    }

    private fun cleanUpType(type: Type) =
            sanitizeIdentifier(type.toString().replace("{", "").replace("}", ""))

    private fun getMostGeneralType(types: List<Sig>): Type {
        var mgt = types.first()
        for (i in 2 until types.size) {
            val currType = types[i]
            if (mgt.isSameOrDescendentOf(currType)) {
                mgt = currType
            }
        }
        return mgt.type()
    }

    /**
     * Transform the type of an Alloy ast node to a Prolog term type(ListOfType,Arity).
     * Additionally, log if sequences are used.
     */
    private fun getType(type: Type): String {
        val tType = splitAndCleanType(type)
        val tTypeString = tType.toString()
        val seqTypeRegex = Regex(".seq'.")
        usesSequences = usesSequences || seqTypeRegex.containsMatchIn(tTypeString)
        return "type(${if (tType.isEmpty()) "[untyped]" else tTypeString},${type.arity()})"
    }
}

