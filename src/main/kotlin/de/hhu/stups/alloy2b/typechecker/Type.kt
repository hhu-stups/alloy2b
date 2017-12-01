package de.hhu.stups.alloy2b.typechecker

enum class Types {
    SCALAR, SET, RELATION, UNTYPED, INTEGER
}

class Type(type:Types = Types.UNTYPED) {
    var currentType = type

    fun setType(type: Types) {
        if(currentType == Types.UNTYPED) {
            currentType = type
        } else if(currentType != type) {
            throw UnsupportedOperationException("Type Checking failed")
        }
    }

    fun untyped(): Boolean {
        return currentType == Types.UNTYPED
    }

    override fun toString(): String {
        return currentType.toString()
    }

    override fun equals(other: Any?): Boolean {
        if(other is Type) {
            return currentType.equals(other.currentType)
        } else {
            return false
        }
    }
}