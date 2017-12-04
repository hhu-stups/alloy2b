package de.hhu.stups.alloy2b.typechecker

interface ExplicitType

class Type(type: ExplicitType = Untyped()) {
    var currentType = type

    fun setType(type: ExplicitType) {
        if (currentType is Untyped) {
            currentType = type
        } else if (currentType != type) {
            throw UnsupportedOperationException("Type Checking failed")
        }
    }

    fun untyped(): Boolean {
        return currentType is Untyped
    }

    override fun toString(): String {
        return currentType.toString()
    }

    override fun equals(other: Any?): Boolean {
        if (other is Type) {
            return currentType.equals(other.currentType)
        } else {
            return false
        }
    }

    override fun hashCode(): Int {
        return currentType.hashCode()
    }
}

data class Signature(val subType: String) : ExplicitType

data class Set(val subType: ExplicitType) : ExplicitType

data class Relation(val leftType: ExplicitType, val rightType: ExplicitType) : ExplicitType

data class Scalar(val subType: ExplicitType) : ExplicitType

class Integer : ExplicitType

class Untyped : ExplicitType