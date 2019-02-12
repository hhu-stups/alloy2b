package de.hhu.stups.alloy2b.translation

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
