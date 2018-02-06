package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionTranslator: VisitReturn<String>() {
    override fun visit(p0: ExprBinary): String =
            when(p0.op) {
                ExprBinary.Op.EQUALS -> "${p0.left.accept(this)} = ${p0.right.accept(this)}"
                ExprBinary.Op.JOIN -> translateJoin(p0.left,p0.right)
                else -> TODO("Binary operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprList): String =
            when(p0.op) {
                ExprList.Op.AND -> p0.args.map { it.accept(this) }.joinToString(" & ")
                else -> TODO("List operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprCall?): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visit(p0: ExprConstant?): String =
            p0.toString()

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
            when(p0.op) {
                ExprUnary.Op.NOOP -> p0.sub.accept(this)
                ExprUnary.Op.CARDINALITY -> "card(${p0.sub.accept(this)})"
                ExprUnary.Op.ONEOF -> p0.sub.accept(this) // TODO do something here
                else -> TODO("Unary operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprVar): String =
            p0.toString()

    override fun visit(p0: Sig): String =
            p0.label

    override fun visit(p0: Sig.Field): String =
            p0.label

    private fun translateJoin(left: Expr, right: Expr): String {
        val tLeft = left.accept(this)
        val tRight = right.accept(this)
        if(left.type().arity() == 1 && right.type().arity() == 2) {
            return "$tRight[$tLeft]"
        }
        if(left.type().arity() == 2 && right.type().arity() == 1) {
            return "$tLeft~[$tRight]"
        }
        if(left.type().arity() == 2 && right.type().arity() == 2) {
            return "($tLeft ; $tRight)"
        }
        TODO("Join Translation failed")
    }

}