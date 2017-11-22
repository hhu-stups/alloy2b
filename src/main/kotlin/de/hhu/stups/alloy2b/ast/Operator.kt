package de.hhu.stups.alloy2b.ast

enum class Operator {
    LONE, SET, ALL, JOIN, EQUAL, UNION, NO, IN, CLOSURE, CLOSURE1, ONE, SOME;

    companion object {
        fun fromString(op: String): Operator = when (op) {
            "lone" -> LONE
            "set" -> SET
            "all" -> ALL
            "." -> JOIN
            "=" -> EQUAL
            "+" -> UNION
            "no" -> NO
            "in" -> IN
            "*" -> CLOSURE
            "^" -> CLOSURE1
            "one" -> ONE
            "some" -> SOME
            else -> throw UnsupportedOperationException(op)
        }
    }
}