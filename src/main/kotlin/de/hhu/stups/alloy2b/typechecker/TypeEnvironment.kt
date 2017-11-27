package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.Expression
import de.hhu.stups.alloy2b.ast.IdentifierExpression

class TypeEnvironment(val types: Map<String,Type> = mapOf()) {
    fun lookupType(id: IdentifierExpression) =
            types.get(id.name)

    fun addType(id: String, type: Type) =
            TypeEnvironment(types + Pair(id,type))
}
