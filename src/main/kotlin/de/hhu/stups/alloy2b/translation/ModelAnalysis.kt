package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.typechecker.Scalar
import de.hhu.stups.alloy2b.typechecker.Set
import de.hhu.stups.alloy2b.typechecker.Type
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule

fun modelAnalysis(orderingAndScopeMap: Map<String, Long>,
                  translationPreferences: Map<TranslationPreference, MutableMap<String, String>>,
                  spec: AlloySpecification) {
    // check if an unordered signature interacts with an ordered signature
    // if so, we have to define the unordered signature as a set of integer, too
    spec.declarations.forEach { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
}

private fun modelAnalysis(orderingAndScopeMap: Map<String, Long>,
                          translationPreferences: Map<TranslationPreference, MutableMap<String, String>>,
                          statement: Statement) {
    when (statement) {
        is FactDeclaration -> statement.expressions
                .map { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
        is FunDeclaration -> statement.expressions
                .map { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
        is PredDeclaration -> statement.expressions
                .map { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
        is AssertionStatement -> statement.expressions
                .map { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
        is RunStatement -> statement.expressions
                .map { modelAnalysis(orderingAndScopeMap, translationPreferences, it) }
    }
}

private fun modelAnalysis(orderingAndScopeMap: Map<String, Long>,
                          translationPreferences: Map<TranslationPreference, MutableMap<String, String>>,
                          expression: Expression) {
    when (expression) {
        is BinaryOperatorExpression ->
            modelAnalysisBinaryExpression(orderingAndScopeMap, translationPreferences, expression)
    }
}

private fun modelAnalysisBinaryExpression(orderingAndScopeMap: Map<String, Long>,
                                          translationPreferences: Map<TranslationPreference, MutableMap<String, String>>,
                                          expression: BinaryOperatorExpression) {
    val leftIsOrderedSignature = isOrderedSignature(orderingAndScopeMap, expression.left.type)
    val rightIsOrderedSignature = isOrderedSignature(orderingAndScopeMap, expression.right.type)
    if ((leftIsOrderedSignature.first.isOrdered() && rightIsOrderedSignature.first.isUnordered()) ||
            rightIsOrderedSignature.first.isOrdered() && leftIsOrderedSignature.first.isUnordered()) {
        // left is ordered but right is unordered signature or vice versa
        // add the pair of signature names to the translation preferences
        translationPreferences[TranslationPreference.ORDERED_UNORDERED_SIGNATURE_INTERACTION]
                ?.put(leftIsOrderedSignature.second, rightIsOrderedSignature.second)
        return
    }
    val leftSigType = getSigTypeFromSet(expression.left.type).currentType
    val rightSigType = getSigTypeFromSet(expression.right.type).currentType
    if (leftSigType is Scalar && rightSigType is Scalar &&
            leftSigType != rightSigType) {
        translationPreferences[TranslationPreference.DISTINCT_SIGNATURE_INTERACTION]
                ?.put(leftSigType.subType.currentType.toString(), rightSigType.subType.currentType.toString())
    }
}

private fun isOrderedSignature(orderingAndScopeMap: Map<String, Long>,
                               type: Type): Pair<Ordering, String> {
    // return a pair of Ordering and the signature name
    val currentType = type.currentType
    if (currentType is Set) {
        val sigType = getSigTypeFromSet(type).toString()
        if (orderingAndScopeMap.contains(sigType)) {
            return Pair(Ordering.ORDERED, sigType)
        }
        return Pair(Ordering.UNORDERED, sigType)
    }
    return Pair(Ordering.NO_SIGNATURE, "")
}

private enum class Ordering {
    ORDERED, UNORDERED, NO_SIGNATURE;

    fun isOrdered(): Boolean {
        return this == ORDERED
    }

    fun isUnordered(): Boolean {
        return this == UNORDERED
    }
}