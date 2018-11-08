package de.hhu.stups.alloy2b.translation

import edu.mit.csail.sdg.alloy4compiler.ast.Type

/**
 * Replace ticks by underscores and use single quotes for identifiers since strings with capital letter first are
 * variables in Prolog. The arity is also added to the Prolog term.
 */
fun sanitizeIdentifier(identifier: String): String {
    if (identifier == "this") {
        // field declarations of signature may be joined with 'this' leading to a universal quantification in B
        return identifier
    }
    return identifier.replace("'", "_").replace("{", "").replace("}", "")
            .split("/").asSequence().filter { it != "this" }
            .joinToString("") { "'$it'" }
}

/**
 * Transform the type of an Alloy ast node (like t1->t2->..) to a Prolog list.
 */
private fun splitAndCleanType(innerType: Type.ProductType?) =
        innerType.toString().split("->").map { it ->
            sanitizeIdentifier(it.replace("{", "").replace("}", ""))
        }

/**
 * Transform the type of an Alloy ast node to a Prolog term type(ListOfType,Arity).
 */
fun getType(type: Type): String {
    val tType = type.map { splitAndCleanType(it) }
    return "type(${if (tType.isEmpty()) "[untyped]" else tType.toString()},${type.arity()})"
}
