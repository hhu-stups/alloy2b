package de.hhu.stups.alloy2b.ast

enum class Operator {
    LONE, SET, ALL, DOT, EQUAL, PLUS, NO, IN, STAR, CLOSURE, ONE;

    companion object {
        fun fromString(op: String): Operator = when (op) {
            "lone" -> LONE
            "set" -> SET
            "all" -> ALL
            "." -> DOT
            "=" -> EQUAL
            "+" -> PLUS
            "no" -> NO
            "in" -> IN
            "*" -> STAR
            "^" -> CLOSURE
            "one" -> ONE
            else -> throw UnsupportedOperationException(op)
        }
    }
}