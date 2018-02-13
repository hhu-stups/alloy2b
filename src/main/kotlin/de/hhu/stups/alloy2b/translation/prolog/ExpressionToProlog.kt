package de.hhu.stups.alloy2b.translation.prolog

import de.hhu.stups.alloy2b.ast.Operator
import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionToProlog(private val alloyAstToProlog: AlloyAstToProlog) : VisitReturn<String>() {

    override fun visit(p0: ExprBinary): String =
            "${getOperator(p0.op.toString())}(${p0.left.accept(this)},${p0.right.accept(this)}," +
                    "pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprList): String =
            "${getOperator(p0.op.toString())}(${p0.args.map { it.accept(this) }},pos(${p0.pos.x},${p0.pos.y}))"

    override fun visit(p0: ExprCall?): String {
        return "expr_call(${p0?.`fun`},${p0?.args?.map { it.accept(this) }}" +
                ",(${p0?.pos?.x},${p0?.pos?.y})"
    }

    override fun visit(p0: ExprConstant?): String {
        return p0.toString().toLowerCase()
    }

    override fun visit(p0: ExprITE?): String =
            "exprITE(${p0?.cond?.accept(this)},${p0?.left?.accept(this)},${p0?.right?.accept(this)}" +
                    ",(${p0?.pos?.x},${p0?.pos?.y})"

    override fun visit(p0: ExprLet?): String =
            "let(${p0?.expr?.accept(this)},${p0?.sub?.accept(this)}" +
                    ",(${p0?.pos?.x},${p0?.pos?.y})"

    override fun visit(p0: ExprQt?): String =
            "${getOperator(p0?.op.toString())}(${p0?.decls?.map { alloyAstToProlog.toPrologTerm(it) }}," +
                    "${p0?.sub?.accept(this)},(${p0?.pos?.x},${p0?.pos?.y})"

    override fun visit(p0: ExprUnary): String {
        if (p0.op == ExprUnary.Op.NOOP) {
            return p0.sub.accept(this)
        }
        if (p0.op == ExprUnary.Op.CAST2INT || p0.op == ExprUnary.Op.CAST2SIGINT) {
            return "integer_cast(${p0.sub.accept(this)},pos(${p0.pos.x},${p0.pos.y}))"
        }
        return "${getOperator(p0.op.toString())}(${p0.sub.accept(this)},pos(${p0.pos.x},${p0.pos.y}))"

    }

    override fun visit(p0: ExprVar): String = p0.toString().toLowerCase()

    override fun visit(p0: Sig): String = p0.label.toLowerCase()

    override fun visit(p0: Sig.Field): String = p0.label.toLowerCase()

    private fun getOperator(op: String): String {
        return try {
            Operator.fromString(op).toString().toLowerCase()
        } catch (exception: UnsupportedOperationException) {
            op.toLowerCase().replace(" ", "")
        }
    }
}