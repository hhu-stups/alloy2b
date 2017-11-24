package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.parsing.ParserFacade.parse
import de.hhu.stups.alloy2b.translation.BTranslation
import java.io.File

fun main(args: Array<String>) {
    if(args.size == 1) {
        println("Parsing ...")
        val parseResult = parse(File(args[0]))

        if (parseResult.root != null) {
            println("Translating ...")
            println(BTranslation((parseResult.root.toAst(false))).getTranslation())
        }
    } else if (args.size == 2) {
        val parseResult = parse(File(args[0]))
        if (parseResult.root != null) {
            File(args[1]).printWriter().use { out ->
                out.println(BTranslation((parseResult.root.toAst(false))).getTranslation()) }
        }
        else {
            println("Parsing or Translation failed")
        }
    } else {
        println("use alloy filename as single command line argument (translation will be printed to stdout)")
        println("or alloy filename and .mch file as two command line arguments")
    }
}