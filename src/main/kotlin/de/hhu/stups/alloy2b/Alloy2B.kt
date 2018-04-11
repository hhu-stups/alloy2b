package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.AlloyAstToProlog
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
    println("Use the alloy file path as a single command line argument to print the translation to stdout.")
    println("Alternatively, use the alloy file path and an output file name to print the translation to a file.")
}

fun translatePrologToConsole(inputFilePath: String) {
    println(AlloyAstToProlog(inputFilePath).getPrologTerm())
}

fun translatePrologToFile(inputFilePath: String, outputFilePath: String) {
    File(outputFilePath).printWriter().use { out ->
        out.println(AlloyAstToProlog(inputFilePath).getPrologTerm())
    }
}
