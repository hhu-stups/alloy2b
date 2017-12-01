package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.IdentifierExpression

class TypeEnvironment(val types: MutableMap<String, Type> = mutableMapOf()) {
    fun lookupType(id: IdentifierExpression) =
            types[id.name] ?: Type()

    fun addType(id: String, type: Types) =
            if(types.containsKey(id)) {
                types[id]!!.setType(type)
            } else {
                types[id] = Type(type)
            }

    fun addType(id: String, type: Type) =
            addType(id,type.currentType)

    fun copy(): TypeEnvironment =
            TypeEnvironment(types.toMutableMap())
}
