package de.hhu.stups.alloy2b.ast

enum class Operator {
    LONE;

    companion object {
        fun fromString(op: String): Operator = when (op) {
            "lone" -> LONE
            else -> throw UnsupportedOperationException(op)
        }
    }
}