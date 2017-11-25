package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.parsing.ParserFacade.parse
import de.hhu.stups.alloy2b.translation.BTranslation
import java.io.File

fun main(args: Array<String>) {
    if (args.size == 1) {
        translateToConsole(args[0])
        return
    }
    if (args.size == 2) {
        translateToFile(args[0], args[1])
        return
    }
    println("use alloy filename as single command line argument (translation will be printed to stdout)")
    println("or alloy filename and .mch file as two command line arguments")
}

fun translateToConsole(inputFilePath: String) {
    println("Parsing ...")
    val parseResult = parse(File(inputFilePath))
    if (parseResult.root != null) {
        println("Translating ...")
        println(BTranslation((parseResult.root.toAst(false))).getTranslation())
    }
    println("Parsing failed")
}

fun translateToFile(inputFilePath: String, outputFilePath: String) {
    println("Parsing ...")
    val parseResult = parse(File(inputFilePath))
    if (parseResult.root != null) {
        println("Translating ...")
        File(outputFilePath).printWriter().use { out ->
            out.println(BTranslation((parseResult.root.toAst(false))).getTranslation())
        }
        return
    }
    println("Parsing failed")
}

