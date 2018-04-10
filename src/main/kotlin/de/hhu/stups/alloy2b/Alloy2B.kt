package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.parsing.ParserFacade.parse
import de.hhu.stups.alloy2b.translation.BTranslation
import de.hhu.stups.alloy2b.translation.prolog.AlloyAstToProlog
import java.io.File

fun main(args: Array<String>) {
    if (args.size == 2 && args[1] == "-toProlog") {
        translatePrologToConsole(args[0])
        return
    }
    if (args.size == 3 && args[1] == "-toProlog") {
        translatePrologToFile(args[0],args[2])
        return
    }
    if (args.size == 1) {
        translateBToConsole(args[0])
        return
    }
    if (args.size == 2) {
        translateBToFile(args[0], args[1])
        return
    }
    println("Use the alloy file path as a single command line argument to print the translation to stdout.")
    println("Alternatively, use the alloy file path and .mch file path as two command line arguments.")
    println("Use the flag -toProlog as the last argument to translate an Alloy model to a Prolog term.")
}

fun translatePrologToConsole(inputFilePath: String) {
    println(AlloyAstToProlog(inputFilePath).getPrologTerm())
}

fun translatePrologToFile(inputFilePath: String, outputFilePath: String) {
    File(outputFilePath).printWriter().use { out ->
            out.println(AlloyAstToProlog(inputFilePath).getPrologTerm())
    }
}

fun translateBToConsole(inputFilePath: String) {
    println("Parsing ...")
    val parseResult = parse(File(inputFilePath))
    if (parseResult.root != null && parseResult.errors.isEmpty()) {
        println("Translating ...")
        println(BTranslation((parseResult.root.toAst(false))).getTranslation())
        return
    }
    println("Parsing failed.")
}

fun translateBToFile(inputFilePath: String, outputFilePath: String) {
    println("Parsing ...")
    val parseResult = parse(File(inputFilePath))
    if (parseResult.root != null && parseResult.errors.isEmpty()) {
        println("Translating ...")
        File(outputFilePath).printWriter().use { out ->
            out.println(BTranslation((parseResult.root.toAst(false))).getTranslation())
        }
        return
    }
    println("Parsing failed.")
}

