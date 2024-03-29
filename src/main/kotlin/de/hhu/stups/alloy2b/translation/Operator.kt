package de.hhu.stups.alloy2b.translation

import java.util.*

enum class Operator {
    LONEOF, ONEOF, SETOF, EXACTLYOF, SOMEOF, IS_SEQ, COMPREHENSION,
    LONE, SET, ALL, JOIN, EQUAL, NOT_EQUAL, PLUS, INTERSECTION, MINUS, NO, NOT_IN, IN, CLOSURE, CLOSURE1, ONE, SOME, AND, OR,
    IMPLICATION, IFF, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, CARD, DOM_RESTR, RAN_RESTR, OVERRIDE, ABSTRACT,
    CARTESIAN, TOTAL_FUNCTION, PARTIAL_FUNCTION, TOTAL_BIJECTION, TOTAL_INJECTION, PARTIAL_INJECTION,
    PARTIAL_SURJECTION, TOTAL_SURJECTION, PARTIAL_BIJECTION,
    TOTAL_RELATION, SURJECTION_RELATION, TOTAL_SURJECTION_RELATION,
    INJECTION_SURJECTION_RELATION, INJECTION_RELATION, TOTAL_BIJECTION_RELATION, BIJECTION_RELATION,
    INVERSE, NOT, SEQ,
    INT_PLUS, INT_MINUS, INT_DIV, INT_MODULO, INT_PRODUCT, INT_SUM, INT_MAX, INT_MIN,CAST2INT,CAST2SIGINT,
    TOTALORDER;

    companion object {
        fun toKeyword(op: Operator) : String =
                // one can use plus[1,1] or 1+1 in Alloy; for our translation, both should point to the same keyword
                // (note that plus[1,1] is a function application called "integer'plus")
                when (op) {
                    INT_PLUS -> "integer''plus"
                    INT_MINUS -> "integer''minus"
                    INT_DIV -> "integer''div"
                    INT_MODULO -> "integer''rem"
                    INT_PRODUCT -> "integer''mul"
                    INT_SUM -> "integer''sum"
                    INT_MAX -> "integer''max"
                    INT_MIN -> "integer''min"
                    else -> op.toString().lowercase(Locale.getDefault())
                }

        fun fromString(op: String): Operator =
                when (op) {
                    "Int->int" -> CAST2INT
                    "int->Int" -> CAST2SIGINT
                    "TOTALORDER" -> TOTALORDER
                    "comprehension" -> COMPREHENSION
                    "isSeq->lone" -> IS_SEQ
                    "some of" -> SOMEOF
                    "exactly of" -> EXACTLYOF
                    "lone of" -> LONEOF
                    "one of" -> ONEOF
                    "set of" -> SETOF
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
                    "one->some" -> TOTAL_BIJECTION_RELATION // not supported, inverse is a bijection
                    "one->" -> BIJECTION_RELATION // not supported, inverse is a bijection
                    
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
                    "@+" -> INT_PLUS
                    "minus" -> INT_MINUS
                    "@-" -> INT_MINUS
                    "div" -> INT_DIV
                    "/" -> INT_DIV
                    "rem" -> INT_MODULO
                    "%" -> INT_MODULO
                    "mul" -> INT_PRODUCT
                    "sum" -> INT_SUM
                    "max" -> INT_MAX
                    "min" -> INT_MIN
                    else -> throw UnsupportedOperationException(op)
                }
    }
}