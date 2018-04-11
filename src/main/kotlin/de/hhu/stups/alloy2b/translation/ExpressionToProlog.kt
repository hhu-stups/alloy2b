package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionToProlog(private val alloyAstToProlog: AlloyAstToProlog,
                         private val orderedSignatures: MutableList<String>) : VisitReturn<String>() {

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
                "${alloyAstToProlog.getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"
    }

    override fun visit(p0: ExprList) =
            "${getOperator(p0.op.toString())}(${p0.args.map { it.accept(this) }},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprCall): String {
        val functor = if (p0.`fun`.isPred) "pred_call" else "fun_call"
        val name = alloyAstToProlog.sanitizeIdentifier(p0.`fun`.label)
        // flatten types for ordering function calls to the signature name
        val type = if (name.startsWith("'ordering_'") || name.endsWith("'first_'") || name.endsWith("'last_'"))
            "type([${alloyAstToProlog.sanitizeIdentifier(p0.type().toString().split("->").first())}]," +
                    "${p0.type().arity()})" else alloyAstToProlog.getType(p0.type())
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
        return "$p0"
    }

    override fun visit(p0: ExprITE) =
            "if_then_else(${p0.cond?.accept(this)},${p0.left?.accept(this)},${p0.right?.accept(this)}" +
                    ",${alloyAstToProlog.getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprLet) =
            // a let with multiple variables are split into several let expressions by Alloy each having only one var
            "let(${alloyAstToProlog.sanitizeIdentifier(p0.`var`.label)}," +
                    "${p0.expr?.accept(this)},${p0.sub?.accept(this)}" +
                    ",${alloyAstToProlog.getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprQt): String {

        val params = p0.decls?.map { it.names.joinToString(",") { alloyAstToProlog.sanitizeIdentifier(it.label) } }
        return "${getOperator(p0.op.toString())}($params," +
                "${p0.decls?.map { alloyAstToProlog.toPrologTerm(it) }}," +
                "${p0.sub?.accept(this)},${alloyAstToProlog.getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"
    }

    override fun visit(p0: ExprUnary): String =
            when (p0.op) {
                ExprUnary.Op.NOOP -> p0.sub.accept(this) as String
                ExprUnary.Op.CAST2INT,
                ExprUnary.Op.CAST2SIGINT -> p0.sub.accept(this)
                else -> "${getOperator(p0.op.toString())}(${p0.sub.accept(this)}," +
                        "${alloyAstToProlog.getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"
            }

    override fun visit(p0: ExprVar) = "identifier(${alloyAstToProlog.sanitizeIdentifier(p0.label)}," +
            "${alloyAstToProlog.getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: Sig) = "identifier(${alloyAstToProlog.sanitizeIdentifier(p0.label)}," +
            "${alloyAstToProlog.getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: Sig.Field) = "identifier(${alloyAstToProlog.sanitizeIdentifier(p0.label)}," +
            "${alloyAstToProlog.getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    private fun getOperator(op: String) =
            try {
                Operator.fromString(op).toString().toLowerCase()
            } catch (exception: UnsupportedOperationException) {
                op.toLowerCase().replace(" ", "")
            }
}