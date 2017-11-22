package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.AlloySpecification

class BTranslation {
    val sets = emptyList<String>()

    constructor(spec: AlloySpecification) {

    }

    fun getTranslation() = sets.joinToString(",")
}