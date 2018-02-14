package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.Env
import edu.mit.csail.sdg.alloy4.SafeList
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

class AlloyAstSingletonAnnotator(spec: CompModule) : VisitReturn<Unit>() {

    private val isSingleton = mutableMapOf<Expr, Boolean>()

    fun isSingleton(expr: Expr): Boolean = isSingleton.getOrDefault(expr,false)
    fun setSingleton(expr: Expr) = isSingleton.put(expr, true)

    private val currentSingletonIDs: Env<String,Boolean> = Env()

    init {
        collectSignatures(spec.allSigs)
        annotateSingletons(spec.allFunc)
    }

    private fun collectSignatures(allSigs: SafeList<Sig>) {
        allSigs.forEach {
            if (it.isOne != null) {
                currentSingletonIDs.put(it.label, true)
            }
        }
    }

    private fun annotateSingletons(allFuncs: SafeList<Func>) {
        allFuncs.forEach {
            it.decls.forEach {
                it.expr.accept(this)
            }
            it.body.accept(this)
        }
    }

        override fun visit(p0: ExprBinary) {
            p0.left.accept(this)
            p0.right.accept(this)
            if(isSingleton(p0.left)) { // TODO: join, etc
                setSingleton(p0)
            }
        }

        override fun visit(p0: ExprList) {
            p0.args.forEach { it.accept(this) }
        }

        override fun visit(p0: ExprCall) {
            p0.args.forEach { it.accept(this) }
        }

        override fun visit(p0: ExprConstant?) {
            TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
        }

        override fun visit(p0: ExprITE?) {
            TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
        }

        override fun visit(p0: ExprLet?) {
            TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
        }

        override fun visit(p0: ExprQt) {
            p0.decls.forEach {
                it.expr.accept(this)
                currentSingletonIDs.put(it.names.joinToString("/"),isSingleton(it.expr))
            }
            p0.sub.accept(this)
            p0.decls.forEach {
                currentSingletonIDs.remove(it.names.joinToString("/"))
            }
        }

        override fun visit(p0: ExprUnary) {
            p0.sub.accept(this)
            if(isSingleton(p0.sub)) {
                setSingleton(p0)
            }
            if(p0.op == ExprUnary.Op.ONEOF) {
                setSingleton(p0)
            }
        }

        override fun visit(p0: ExprVar) {
            if(currentSingletonIDs.has(p0.label) && currentSingletonIDs.get(p0.label)) {
                setSingleton(p0)
            }
        }

        override fun visit(p0: Sig?) {
        }

        override fun visit(p0: Sig.Field?) {
        }
}