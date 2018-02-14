package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4.SafeList
import edu.mit.csail.sdg.alloy4compiler.ast.Expr
import edu.mit.csail.sdg.alloy4compiler.ast.Func
import edu.mit.csail.sdg.alloy4compiler.ast.Sig
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

class AlloyAstSingletonAnnotator(spec: CompModule) {

    private val isSingleton = mutableMapOf<Expr, Boolean>()
    private val singletonSignatures = mutableListOf<Sig>()

    fun isSingleton(expr: Expr): Boolean = isSingleton.getOrDefault(expr,false)

    init {
        collectSignatures(spec.allSigs)
        annotateSingletons(spec.allFunc)
    }

    private fun collectSignatures(allSigs: SafeList<Sig>) {
        allSigs.forEach {
            if (it.isOne != null) {
                singletonSignatures.add(it)
            }
        }
    }

    private fun annotateSingletons(allSigs: SafeList<Func>?) {}
}