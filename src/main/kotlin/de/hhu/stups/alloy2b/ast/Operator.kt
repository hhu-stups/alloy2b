package de.hhu.stups.alloy2b.ast

enum class Operator {
    LONE, SET, ALL, JOIN, EQUAL, UNION, INTERSECTION, DIFFERENCE, NO, IN, CLOSURE, CLOSURE1, ONE, SOME, AND, OR,
    IMPLIES, IFF, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, CARD, DOM_RESTR, RAN_RESTR, OVERRIDE, ABSTRACT;

    companion object {
        fun fromString(op: String): Operator = when (op) {
            "lone" -> LONE
            "set" -> SET
            "all" -> ALL
            "++" -> OVERRIDE
            "." -> JOIN
            "<:" -> DOM_RESTR
            ":>" -> RAN_RESTR
            "=" -> EQUAL
            ">" -> GREATER
            ">=" -> GREATER_EQUAL
            "<" -> LESS
            "=<" -> LESS_EQUAL
            "#" -> CARD
            "&" -> INTERSECTION
            "+" -> UNION
            "-" -> DIFFERENCE
            "no" -> NO
            "in" -> IN
            "*" -> CLOSURE
            "^" -> CLOSURE1
            "one" -> ONE
            "some" -> SOME
            "abstract" -> ABSTRACT
            else -> throw UnsupportedOperationException(op)
        }
    }
}