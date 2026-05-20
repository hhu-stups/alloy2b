package de.hhu.stups.alloy2b.translation;

import java.util.Locale;

public enum Operator {
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

    public static String toKeyword(Operator op) {
        // one can use plus[1,1] or 1+1 in Alloy; for our translation, both should point to the same keyword
        // (note that plus[1,1] is a function application called "integer'plus")
        switch (op) {
            case INT_PLUS: return "integer''plus";
            case INT_MINUS: return "integer''minus";
            case INT_DIV: return "integer''div";
            case INT_MODULO: return "integer''rem";
            case INT_PRODUCT: return "integer''mul";
            case INT_SUM: return "integer''sum";
            case INT_MAX: return "integer''max";
            case INT_MIN: return "integer''min";
            default: return op.toString().toLowerCase(Locale.getDefault());
        }
    }

    public static Operator fromString(String op) {
        switch (op) {
            case "Int->int": return CAST2INT;
            case "int->Int": return CAST2SIGINT;
            case "TOTALORDER": return TOTALORDER;
            case "comprehension": return COMPREHENSION;
            case "isSeq->lone": return IS_SEQ;
            case "some of": return SOMEOF;
            case "exactly of": return EXACTLYOF;
            case "lone of": return LONEOF;
            case "one of": return ONEOF;
            case "set of": return SETOF;
            case "lone": return LONE;
            case "set": return SET;
            case "all": return ALL;
            case "++": return OVERRIDE;
            case ".": return JOIN;
            case "<:": return DOM_RESTR;
            case ":>": return RAN_RESTR;
            case "=": return EQUAL;
            case "!=": return NOT_EQUAL;
            case ">": return GREATER;
            case ">=": return GREATER_EQUAL;
            case "<": return LESS;
            case "<=": return LESS_EQUAL;
            case "=<": return LESS_EQUAL;
            case "#": return CARD;
            case "&": return INTERSECTION;
            case "+": return PLUS;
            case "-": return MINUS;
            case "no": return NO;
            case "in": return IN;
            case "!in": return NOT_IN;
            case "*": return CLOSURE;
            case "^": return CLOSURE1;
            case "~": return INVERSE;
            case "one": return ONE;
            case "some": return SOME;
            case "abstract": return ABSTRACT;
            case "=>": return IMPLICATION;
            case "implies": return IMPLICATION;
            case "iff": return IFF;
            case "<=>": return IFF;
            case "->": return CARTESIAN;

            case "->one": return TOTAL_FUNCTION;
            case "->lone": return PARTIAL_FUNCTION;
            case "->some": return TOTAL_RELATION;

            case "lone->lone": return PARTIAL_INJECTION;
            case "lone->one": return TOTAL_INJECTION;
            case "lone->": return INJECTION_RELATION; // not supported
            case "lone->some": return INJECTION_SURJECTION_RELATION; // not supported

            case "one->lone": return PARTIAL_BIJECTION;
            case "one->one": return TOTAL_BIJECTION;
            case "one->some": return TOTAL_BIJECTION_RELATION; // not supported, inverse is a bijection
            case "one->": return BIJECTION_RELATION; // not supported, inverse is a bijection

            case "some->lone": return PARTIAL_SURJECTION;
            case "some->one": return TOTAL_SURJECTION;
            case "some->some": return TOTAL_SURJECTION_RELATION;
            case "some->": return SURJECTION_RELATION;

            case "and": return AND;
            case "AND": return AND;
            case "&&": return AND;
            case "or": return OR;
            case "||": return OR;
            case "OR": return OR;
            case "!": return NOT;
            case "not": return NOT;
            case "seq": return SEQ;
            case "plus": return INT_PLUS;
            case "@+": return INT_PLUS;
            case "minus": return INT_MINUS;
            case "@-": return INT_MINUS;
            case "div": return INT_DIV;
            case "/": return INT_DIV;
            case "rem": return INT_MODULO;
            case "%": return INT_MODULO;
            case "mul": return INT_PRODUCT;
            case "sum": return INT_SUM;
            case "max": return INT_MAX;
            case "min": return INT_MIN;
            default: throw new UnsupportedOperationException(op);
        }
    }
}
