package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionTranslator(private val alloyAstTranslation: AlloyAstTranslation) : VisitReturn<String>() {

    override fun visit(p0: ExprBinary): String =
            when (p0.op) {
                ExprBinary.Op.EQUALS -> "(${p0.left.accept(this)} = ${p0.right.accept(this)})"
                ExprBinary.Op.JOIN -> translateJoin(p0.left, p0.right)
                else -> throw UnsupportedOperationException("Binary operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprList): String =
            when (p0.op) {
                ExprList.Op.AND -> p0.args.joinToString(" & ") { it.accept(this) }
                else -> throw UnsupportedOperationException("List operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprCall?): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visit(p0: ExprConstant?): String {
        if (p0?.toString()?.toIntOrNull() != null) {
            // do not sanitize integers
            return p0.toString()
        }
        return alloyAstTranslation.sanitizeIdentifier(p0.toString())
    }

    override fun visit(p0: ExprITE?): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visit(p0: ExprLet?): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visit(p0: ExprQt?): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visit(p0: ExprUnary): String =
            when (p0.op) {
                ExprUnary.Op.NOOP -> p0.sub.accept(this)
                ExprUnary.Op.CARDINALITY -> "card(${p0.sub.accept(this)})"
                ExprUnary.Op.ONEOF -> p0.sub.accept(this) // TODO do something here
                ExprUnary.Op.SETOF -> "<: ${p0.sub.accept(this)}"
                else -> throw UnsupportedOperationException("Unary operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprVar): String =
            alloyAstTranslation.sanitizeIdentifier(p0.toString())

    override fun visit(p0: Sig): String =
            alloyAstTranslation.sanitizeIdentifier(p0.label)

    override fun visit(p0: Sig.Field): String =
            alloyAstTranslation.sanitizeIdentifier(p0.label)

    private fun translateJoin(left: Expr, right: Expr): String {
        val tLeft = left.accept(this)
        val tRight = right.accept(this)
        if (left.type().arity() == 1 && right.type().arity() == 2) {
            return "($tRight[$tLeft])"
        }
        if (left.type().arity() == 2 && right.type().arity() == 1) {
            return "($tLeft~[$tRight])"
        }
        if (left.type().arity() == 2 && right.type().arity() == 2) {
            return "($tLeft ; $tRight)"
        }
        throw UnsupportedOperationException("Join translation failed.")
    }
}