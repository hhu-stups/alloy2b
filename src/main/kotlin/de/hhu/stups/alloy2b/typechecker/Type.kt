package de.hhu.stups.alloy2b.typechecker

interface ExplicitType

class Type(type: ExplicitType = Untyped()) {
    var currentType = type

    fun setType(type: ExplicitType) {
        if(type is Untyped) {
            return
        }
        if (currentType is Untyped) {
            currentType = type
        } else if (currentType is Relation && type is Relation) {
            (currentType as Relation).rightType.setType(type.rightType.currentType)
            (currentType as Relation).leftType.setType(type.leftType.currentType)
        } else if (currentType is Scalar && type is Scalar) {
            (currentType as Scalar).subType.setType(type.subType.currentType)
            (currentType as Scalar).subType.setType(type.subType.currentType)
        } else if (currentType is Set && type is Set) {
            (currentType as Set).subType.setType(type.subType.currentType)
            (currentType as Set).subType.setType(type.subType.currentType)
        } else if (currentType is Signature && type is Signature) {
            if(!(currentType as Signature).subType.equals(type.subType)) {
                throw UnsupportedOperationException("Type Checking failed. Tried to unify $currentType and $type")
            }
        } else {
            throw UnsupportedOperationException("Type Checking failed. Tried to unify $currentType and $type")
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

data class Set(val subType: Type) : ExplicitType

data class Relation(val leftType: Type, val rightType: Type) : ExplicitType

data class Scalar(val subType: Type) : ExplicitType

class Integer : ExplicitType

class Untyped : ExplicitType {
    override fun toString(): String {
        return "UNTYPED"
    }

    override fun equals(other: Any?): Boolean {
        return other is Untyped
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}