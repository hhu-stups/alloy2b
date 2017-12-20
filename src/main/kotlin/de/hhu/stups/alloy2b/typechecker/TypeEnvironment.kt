package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.IdentifierExpression

class TypeEnvironment(private val types: MutableMap<String, Type> = mutableMapOf(), private val localTypes: MutableMap<String,Type> = mutableMapOf()) {
    fun lookupType(id: IdentifierExpression): Type {
        if (localTypes.containsKey(id.name)) {
            return localTypes[id.name]!!
        }

        if (types.containsKey(id.name)) {
            return types[id.name]!!
        } else {
            types[id.name] = Type(Untyped())
            return types[id.name]!!
        }
    }

    fun addType(id: String, type: ExplicitType) =
            if (types.containsKey(id)) {
                types[id]!!.setType(Type(type))
            } else {
                types[id] = Type(type)
            }

    fun addType(id: String, type: Type) =
            addType(id, type.currentType)

    fun addLocalType(id: String, type: ExplicitType) {
        localTypes[id] = Type(type)
    }


    fun addLocalType(id: String, type: Type) =
            addLocalType(id, type.currentType)
}
