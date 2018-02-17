package de.hhu.stups.alloy2b.translation.prolog

import de.hhu.stups.alloy2b.ast.Operator
import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionToProlog(private val alloyAstToProlog: AlloyAstToProlog) : VisitReturn<String>() {

    override fun visit(p0: ExprBinary): String =
            "${getOperator(p0.op.toString())}(${p0.left.accept(this)},${p0.right.accept(this)}," +
                    "${getType(p0.type())},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprList): String =
            "${getOperator(p0.op.toString())}(${p0.args.map { it.accept(this) }},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprCall): String {
        return "expr_call(${p0.`fun`},${p0.args?.map { it.accept(this) }}," +
                "${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y})"
    }

    override fun visit(p0: ExprConstant): String {
        if(p0.type().is_int) {
            return "integer($p0,pos(${p0.pos.x},${p0.pos.y}))"
        } else if (p0.type().is_bool) {
            return "boolean($p0,pos(${p0.pos.x},${p0.pos.y}))"
        }
        return "\'$p0\'"
    }

    override fun visit(p0: ExprITE): String =
            "exprITE(${p0.cond?.accept(this)},${p0.left?.accept(this)},${p0.right?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y})"

    override fun visit(p0: ExprLet): String =
            "let(${p0.expr?.accept(this)},${p0.sub?.accept(this)}" +
                    ",${getType(p0.type())},pos(${p0.pos?.x},${p0.pos?.y})"

    override fun visit(p0: ExprQt): String =
            "${getOperator(p0.op.toString())}(${p0.decls?.map { alloyAstToProlog.toPrologTerm(it) }}," +
                    "${getType(p0.type())},${p0.sub?.accept(this)},pos(${p0.pos?.x},${p0.pos?.y})"

    override fun visit(p0: ExprUnary): String {
        if (p0.op == ExprUnary.Op.NOOP) {
            return p0.sub.accept(this)
        }
        if (p0.op == ExprUnary.Op.CAST2INT || p0.op == ExprUnary.Op.CAST2SIGINT) {
            return "integer_cast(${p0.sub.accept(this)},pos(${p0.pos.x},${p0.pos.y}))"
        }
        return "${getOperator(p0.op.toString())}(${p0.sub.accept(this)},${getType(p0.type())}," +
                "pos(${p0.pos.x},${p0.pos.y}))"

    }

    override fun visit(p0: ExprVar): String = "$p0"

    override fun visit(p0: Sig): String = "$p0"

    override fun visit(p0: Sig.Field): String = "$p0"

    private fun getOperator(op: String): String {
        return try {
            Operator.fromString(op).toString().toLowerCase()
        } catch (exception: UnsupportedOperationException) {
            op.toLowerCase().replace(" ", "")
        }
    }

    private fun getType(type: Type) : String {
        return "type(${type.toString().replace("{","").replace("}","")}"
    }
}