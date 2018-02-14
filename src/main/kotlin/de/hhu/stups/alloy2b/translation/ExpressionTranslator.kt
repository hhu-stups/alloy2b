package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.ConstList
import edu.mit.csail.sdg.alloy4compiler.ast.*

class ExpressionTranslator(private val alloyAstTranslation: AlloyAstTranslation, private val singletonAnnotator: AlloyAstSingletonAnnotator) : VisitReturn<String>() {

    override fun visit(p0: ExprBinary): String {
        val symbol: String
        when (p0.op) {
            ExprBinary.Op.EQUALS -> symbol = "="
            ExprBinary.Op.NOT_EQUALS -> symbol = "/="
            ExprBinary.Op.JOIN -> return translateJoin(p0.left, p0.right)
            ExprBinary.Op.ARROW -> symbol = "*"
            ExprBinary.Op.INTERSECT -> symbol = "/\\"
            ExprBinary.Op.MINUS -> symbol = "-"
            ExprBinary.Op.DIV -> symbol = "/"
            ExprBinary.Op.MUL -> symbol = "*"
            ExprBinary.Op.PLUS -> symbol = "\\/"
            ExprBinary.Op.IPLUS -> symbol = "+"
            ExprBinary.Op.IMINUS -> symbol = "-"
            ExprBinary.Op.REM -> symbol = "mod"
            ExprBinary.Op.IMPLIES -> symbol = "=>"
            ExprBinary.Op.IFF -> symbol = "<=>"
            ExprBinary.Op.AND -> symbol = "&"
            ExprBinary.Op.OR -> symbol = "or"
            ExprBinary.Op.GT -> symbol = ">"
            ExprBinary.Op.NOT_GT -> symbol = "<="
            ExprBinary.Op.GTE -> symbol = ">="
            ExprBinary.Op.NOT_GTE -> symbol = "<"
            ExprBinary.Op.LT -> symbol = "<"
            ExprBinary.Op.NOT_LT -> symbol = ">="
            ExprBinary.Op.LTE -> symbol = "<="
            ExprBinary.Op.NOT_LTE -> symbol = ">"
            ExprBinary.Op.IN -> symbol = "<:"
            ExprBinary.Op.NOT_IN -> symbol = "/:"
            else -> throw UnsupportedOperationException("Binary operator not implemented: " + p0.op)
        }
        return "(${p0.left.accept(this)} $symbol ${p0.right.accept(this)})"
    }

    override fun visit(p0: ExprList): String =
            when (p0.op) {
                ExprList.Op.AND -> p0.args.joinToString(" & ") { it.accept(this) }
                else -> throw UnsupportedOperationException("List operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprCall): String =
            if (p0.args.isEmpty()) {
                alloyAstTranslation.sanitizeIdentifier(p0.`fun`.label)
            } else {
                "${alloyAstTranslation.sanitizeIdentifier(p0.`fun`.label)}(" +
                        "${p0.args.joinToString(", ") { it.accept(this) }})"
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

    override fun visit(p0: ExprQt): String =
            when (p0.op) {
                ExprQt.Op.ALL -> "!(${translateDeclsIDList(p0.decls)}).(" +
                        "${translateDeclsExprList(p0.decls)} => ${p0.sub.accept(this)})"
                else -> throw UnsupportedOperationException("Quantifier not implemented: " + p0.op)
            }

    override fun visit(p0: ExprUnary): String =
            when (p0.op) {
                ExprUnary.Op.CARDINALITY -> "card(${p0.sub.accept(this)})"
                ExprUnary.Op.ONEOF -> p0.sub.accept(this) // TODO do something here
                ExprUnary.Op.SETOF -> "<: ${p0.sub.accept(this)}"
                ExprUnary.Op.NO -> "(${p0.sub.accept(this)} = {})"
                ExprUnary.Op.SOME -> "card(${p0.sub.accept(this)}) >= 1"
                ExprUnary.Op.LONE -> "card(${p0.sub.accept(this)}) <= 1"
                ExprUnary.Op.NOT -> "not(${p0.sub.accept(this)})"
                ExprUnary.Op.NOOP,
                ExprUnary.Op.CAST2SIGINT,
                ExprUnary.Op.CAST2INT -> p0.sub.accept(this)
                else -> throw UnsupportedOperationException("Unary operator not implemented: " + p0.op)
            }

    override fun visit(p0: ExprVar): String =
            sanitizeTypedIdentifier(p0)

    override fun visit(p0: Sig): String =
            alloyAstTranslation.sanitizeIdentifier(p0.label)

    override fun visit(p0: Sig.Field): String =
            alloyAstTranslation.sanitizeIdentifier(p0.label) + alloyAstTranslation.sanitizeIdentifier(p0.sig.label)

    private fun translateJoin(left: Expr, right: Expr): String {
        val tLeft = left.accept(this)
        val tRight = right.accept(this)
        if (tLeft.toString() == "univ") {
            return "ran($tRight)"
        }
        if (tRight.toString() == "univ") {
            return "dom($tRight)"
        }
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

    private fun translateDeclsIDList(decls: ConstList<Decl>) =
            // sanitizing string instead of identifier expression avoids set expansion to {id}
            decls.joinToString(", ") {
                it.names.joinToString(", ") { alloyAstTranslation.sanitizeIdentifier(it.label) }
            }

    private fun translateDeclsExprList(decls: ConstList<Decl>): String {
        return decls.joinToString(" & ") {
            it.names.joinToString(" & ") { n ->
                "${alloyAstTranslation.sanitizeIdentifier(n.label)} <: ${it.expr.accept(this)}"
            }
        }
    }

    private fun sanitizeTypedIdentifier(id: Expr): String {
        if(singletonAnnotator.isSingleton(id)) {
            return "{${alloyAstTranslation.sanitizeIdentifier(id.toString())}}"
        } else {
            return alloyAstTranslation.sanitizeIdentifier(id.toString())
        }
    }
}
