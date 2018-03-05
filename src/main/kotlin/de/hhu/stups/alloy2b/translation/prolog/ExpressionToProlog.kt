package de.hhu.stups.alloy2b.translation.prolog

import de.hhu.stups.alloy2b.ast.Operator
import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionToProlog(private val alloyAstToProlog: AlloyAstToProlog) : VisitReturn<String>() {

    override fun visit(p0: ExprBinary): String {
        return "${getOperator(p0.op.toString())}(${p0.left.accept(this)},${p0.right.accept(this)}," +
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
        } else if (p0.type().is_bool) {
            return "boolean($p0,pos(${p0.pos.x},${p0.pos.y}))"
        }
        return "$p0"
    }

    override fun visit(p0: ExprITE) =
            "if_then_else(${p0.cond?.accept(this)},${p0.left?.accept(this)},${p0.right?.accept(this)}" +
                    ",${alloyAstToProlog.getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y}))"

    override fun visit(p0: ExprLet) =
            "let(${p0.`var`},${p0.expr?.accept(this)},${p0.sub?.accept(this)}" +
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