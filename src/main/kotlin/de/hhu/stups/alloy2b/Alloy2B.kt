package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.parsing.ParserFacade.parse
import de.hhu.stups.alloy2b.translation.BTranslation
import java.io.File

fun main(args: Array<String>) {
    if(args.size != 1) {
        println("use filename as single command line argument")
    } else {
        println("Parsing ...")
        val parseResult = parse(File(args[0]))

        if (parseResult.root != null) {
            println("Translating ...")
            println(BTranslation((parseResult.root.toAst(false))).getTranslation())
        }
    }
}