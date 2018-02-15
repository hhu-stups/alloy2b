package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.Env
import edu.mit.csail.sdg.alloy4.SafeList
import edu.mit.csail.sdg.alloy4compiler.ast.*
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

class AlloyAstSingletonAnnotator(spec: CompModule) : VisitReturn<Unit>() {

    private val isSingleton = mutableMapOf<Expr, Boolean>()

    private val currentSingletonIDs: Env<Expr, Boolean> = Env()

    init {
        collectSignatures(spec.allSigs)
        annotateSingletons(spec.allFunc)
    }

    private fun collectSignatures(allSigs: SafeList<Sig>) {
        allSigs.forEach {
            if (it.isOne != null) {
                isSingleton[it] = true
            }
        }
    }

    private fun annotateSingletons(allFuncs: SafeList<Func>) {
        allFuncs.forEach {
            it.decls.forEach {
                it.expr.accept(this)
                setSingleton(it.get(), isSingleton(it.expr))
                currentSingletonIDs.put(it.get(), isSingleton(it.expr))
            }
            it.body.accept(this)
            it.decls.forEach {
                currentSingletonIDs.remove(it.get())
            }
        }
    }

    override fun visit(p0: ExprBinary) {
        p0.left.accept(this)
        p0.right.accept(this)
        if (isSingleton(p0.left)) { // TODO: join, etc
            setSingleton(p0)
        }
    }

    override fun visit(p0: ExprList) {
        p0.args.forEach { it.accept(this) }
    }

    override fun visit(p0: ExprCall) {
        p0.args.forEach { it.accept(this) }
    }

    override fun visit(p0: ExprConstant) {
        if (p0.type().is_int || p0.type().is_bool) {
            setSingleton(p0)
        }
    }

    override fun visit(p0: ExprITE) {
        p0.cond.accept(this)
        p0.left.accept(this)
        p0.right.accept(this)
    }

    override fun visit(p0: ExprLet) {
        p0.expr.accept(this)
        currentSingletonIDs.put(p0.`var`, isSingleton(p0.expr))
        p0.sub.accept(this)
        currentSingletonIDs.remove(p0.`var`)
    }

    override fun visit(p0: ExprQt) {
        p0.decls.forEach {
            it.expr.accept(this)
            setSingleton(it.get(),isSingleton(it.expr))
            currentSingletonIDs.put(it.get(), isSingleton(it.expr))
        }
        p0.sub.accept(this)
        p0.decls.forEach {
            currentSingletonIDs.remove(it.get())
        }
    }

    override fun visit(p0: ExprUnary) {
        p0.sub.accept(this)
        if (isSingleton(p0.sub)) {
            setSingleton(p0)
        }
        if (p0.op == ExprUnary.Op.ONEOF) {
            setSingleton(p0)
        }
    }

    override fun visit(p0: ExprVar) {
        if (currentSingletonIDs.has(p0) && currentSingletonIDs.get(p0)) {
            setSingleton(p0)
        }
    }

    override fun visit(p0: Sig?) {
    }

    override fun visit(p0: Sig.Field?) {
    }

    private fun setSingleton(expr: Expr, singleton: Boolean = true) = isSingleton.put(expr, singleton)

    fun isSingleton(expr: Expr): Boolean = isSingleton.getOrDefault(expr, false)
}