package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.IdentifierExpression

class TypeEnvironment(val types: Map<String, Type> = mapOf()) {
    fun lookupType(id: IdentifierExpression) =
            types[id.name] ?: Type.UNTYPED

    fun addType(id: String, type: Type) =
            TypeEnvironment(types + Pair(id, type))
}
