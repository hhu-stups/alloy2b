package de.hhu.stups.alloy2b.translation

enum class Operator {
    LONE, SET, ALL, JOIN, EQUAL, NOT_EQUAL, PLUS, INTERSECTION, MINUS, NO, NOT_IN, IN, CLOSURE, CLOSURE1, ONE, SOME, AND, OR,
    IMPLICATION, IFF, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, CARD, DOM_RESTR, RAN_RESTR, OVERRIDE, ABSTRACT,
    CARTESIAN, TOTAL_FUNCTION, PARTIAL_FUNCTION, TOTAL_BIJECTION, TOTAL_INJECTION, PARTIAL_INJECTION,
    PARTIAL_SURJECTION, TOTAL_SURJECTION, PARTIAL_BIJECTION,
    TOTAL_RELATION, SURJECTION_RELATION, TOTAL_SURJECTION_RELATION,
    INJECTION_SURJECTION_RELATION, INJECTION_RELATION,
    INVERSE, NOT, SEQ,
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
                    "!=" -> NOT_EQUAL
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
                    "!in" -> NOT_IN
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
                    "->some" -> TOTAL_RELATION
                    
                    "lone->lone" -> PARTIAL_INJECTION
                    "lone->one" -> TOTAL_INJECTION
                    "lone->" -> INJECTION_RELATION // not supported
                    "lone->some" -> INJECTION_SURJECTION_RELATION // not supported
                    
                    "one->lone" -> PARTIAL_BIJECTION
                    "one->one" -> TOTAL_BIJECTION
                    // missing one->some and one->
                    
                    "some->lone" -> PARTIAL_SURJECTION
                    "some->one" -> TOTAL_SURJECTION
                    "some->some" -> TOTAL_SURJECTION_RELATION
                    "some->" -> SURJECTION_RELATION
                    

                    "and" -> AND
                    "AND" -> AND
                    "&&" -> AND
                    "or" -> OR
                    "||" -> OR
                    "OR" -> OR
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