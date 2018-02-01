package de.hhu.stups.alloy2b.ast

enum class Operator {
    LONE, SET, ALL, JOIN, EQUAL, PLUS, INTERSECTION, MINUS, NO, IN, CLOSURE, CLOSURE1, ONE, SOME, AND, OR,
    IMPLICATION, IFF, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, CARD, DOM_RESTR, RAN_RESTR, OVERRIDE, ABSTRACT,
    CARTESIAN, TOTAL_FUNCTION, PARTIAL_FUNCTION, BIJECTIVE_FUNCTION, INVERSE, NOT, SEQ,
    INT_PLUS, INT_MINUS, INT_DIV, INT_MODULO, INT_PRODUCT, INT_SUM, INT_MAX, INT_MIN;

    companion object {
        fun fromString(op: String): Operator =
                when (op) {
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
                    "<=" -> LESS_EQUAL
                    "=<" -> LESS_EQUAL
                    "#" -> CARD
                    "&" -> INTERSECTION
                    "+" -> PLUS
                    "-" -> MINUS
                    "no" -> NO
                    "in" -> IN
                    "*" -> CLOSURE
                    "^" -> CLOSURE1
                    "~" -> INVERSE
                    "one" -> ONE
                    "some" -> SOME
                    "abstract" -> ABSTRACT
                    "=>" -> IMPLICATION
                    "implies" -> IMPLICATION
                    "iff" -> IFF
                    "<=>" -> IFF
                    "->" -> CARTESIAN
                    "->one" -> TOTAL_FUNCTION
                    "->lone" -> PARTIAL_FUNCTION
                    "one->one" -> BIJECTIVE_FUNCTION
                    "and" -> AND
                    "&&" -> AND
                    "or" -> OR
                    "||" -> OR
                    "!" -> NOT
                    "not" -> NOT
                    "seq" -> SEQ
                    "plus" -> INT_PLUS
                    "minus" -> INT_MINUS
                    "div" -> INT_DIV
                    "rem" -> INT_MODULO
                    "mul" -> INT_PRODUCT
                    "sum" -> INT_SUM
                    "max" -> INT_MAX
                    "min" -> INT_MIN
                    else -> throw UnsupportedOperationException(op)
                }
    }
}