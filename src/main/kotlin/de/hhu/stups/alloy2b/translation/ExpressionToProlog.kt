package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.*
import kodkod.util.ints.Ints
import kotlin.math.max

class ExpressionToProlog(private val signatures: MutableList<Sig>,
                         private val orderedSignatures: MutableList<String>) : VisitReturn<String>() {

    private val seqTypeRegex = Regex(".seq.")

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
                "field(${sanitizeIdentifier(it.label)},${astNode.expr.accept(this)}," +
                        "${getType(astNode.expr.type())},$options,pos(${astNode.get().pos.x},${astNode.get().pos.y}))"
            }
        }
        return "field(${sanitizeIdentifier(astNode.get().label)},${astNode.expr.accept(this)}," +
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
        return "$p0(pos(${p0.pos.x},${p0.pos.y}))"
    }

    override fun visit(p0: ExprITE) =
            "if_then_else(${p0.cond?.accept(this)},${p0.left?.accept(this)},${p0.right?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprLet) =
    // a let with multiple variables are split into several let expressions by Alloy each having only one var
            "let(${sanitizeIdentifier(p0.`var`.label)}," +
                    "${p0.expr?.accept(this)},${p0.sub?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprQt): String {
        val params = p0.decls?.map { decl -> decl.names.joinToString(",") { sanitizeIdentifier(it.label) } }
        return "${getOperator(p0.op.toString())}($params," +
                "${p0.decls?.map { toPrologTerm(it) }}," +
                "${p0.sub?.accept(this)},${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"
    }

    override fun visit(p0: ExprUnary): String =
            when (p0.op) {
                ExprUnary.Op.NOOP -> p0.sub.accept(this) as String
                ExprUnary.Op.CAST2INT,
                ExprUnary.Op.CAST2SIGINT -> p0.sub.accept(this)
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
            Operator.fromString(op).toString().toLowerCase()
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
        if (typeString == "{Int}" || typeString == "{univ}") {
            return cleanUpType(type)
        }
        val parents = signatures.filter { type.isSubtypeOf(it.type()) }
        if (parents.isEmpty()) {
            return ""
        }
        return cleanUpType(getMostGeneralType(parents))
    }

    private fun generalizeTypes(type: Type): MutableList<String> {
        if (type.arity() == 0) {
            return arrayListOf(cleanUpType(type))
        }
        val typeList = arrayListOf<String>()
        var arity = 0
        for (i in 1..type.arity()) {
            val innerType = type.extract(i)
            arity = max(arity, innerType.arity())
            innerType.forEach {
                (0 until innerType.arity()).forEach { i -> typeList.add(generalizeType(it.get(i).type())) }
            }
        }
        return typeList.subList(0, arity)
    }

    private fun cleanUpType(type: Type) =
            sanitizeIdentifier(type.toString().replace("{", "").replace("}", ""))

    private fun getMostGeneralType(types: List<Sig>): Type {
        assert(types.isNotEmpty())
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
        usesSequences = usesSequences || seqTypeRegex.containsMatchIn(tTypeString)
        return "type(${if (tType.isEmpty()) "[untyped]" else tTypeString},${type.arity()})"
    }
}

