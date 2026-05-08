package de.hhu.stups.alloy2b.translation;

import java.util.Arrays;
import java.util.stream.Collectors;

final class ParserUtil {
    private ParserUtil() {
        throw new AssertionError("Utility class");
    }

    /**
     * Replace ticks by underscores and use single quotes for identifiers since strings with capital letter first are
     * variables in Prolog. The arity is also added to the Prolog term.
     */
    static String sanitizeIdentifier(String identifier) {
        if ("this".equals(identifier)) {
            // field declarations of signature may be joined with 'this' leading to a universal quantification in B
            return identifier;
        }
        String[] split = identifier.replace("'", "_")
            .replace("{", "")
            .replace("}", "")
            .split("/");
        return Arrays.stream(split)
            .filter(s -> !"this".equals(s))
            .map(s -> "'" + s + "'")
            .collect(Collectors.joining());
    }
}
