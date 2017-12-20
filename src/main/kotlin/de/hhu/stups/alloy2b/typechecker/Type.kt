package de.hhu.stups.alloy2b.typechecker

interface ExplicitType

class Type(type: ExplicitType = Untyped()) {
    var currentType = type

    fun setType(type: Type) {
        if(type.untyped() && this.untyped()) {
            return
        }
        if(type.untyped()) {
            type.setType(this)
        }
        val ctype = type.currentType
        if (currentType is Untyped) {
            currentType = type.currentType
        } else if (currentType is Relation && ctype is Relation) {
            (currentType as Relation).rightType.setType(ctype.rightType)
            (currentType as Relation).leftType.setType(ctype.leftType)
        } else if (currentType is Scalar && ctype is Scalar) {
            (currentType as Scalar).subType.setType(ctype.subType)
        } else if (currentType is Set && ctype is Set) {
            (currentType as Set).subType.setType(ctype.subType)
        } else if (currentType is Signature && ctype is Signature) {
            if(!(currentType as Signature).subType.equals(ctype.subType)) {
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

class Integer : ExplicitType {
    override fun toString(): String {
        return "INTEGER"
    }

    override fun equals(other: Any?): Boolean {
        return other is Integer
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class Predicate : ExplicitType

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