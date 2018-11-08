package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.translation.Alloy2BParser
import java.io.File

fun main(args: Array<String>) {
    if (args.size == 2 && args[1] == "-toProlog") {
        translatePrologToConsole(args[0])
        return
    }
    if (args.size == 3 && args[1] == "-toProlog") {
        translatePrologToFile(args[0], args[2])
        return
    }
    println("Usage:")
    println("To print the translation to stdout:")
    println("    $ java -jar <path to alloy2b.jar> input.als -toProlog")
    println("Alternatively, to print the translation to a file:")
    println("    $ java -jar <path to alloy2b.jar> input.als -toProlog output.pl")
}

fun translatePrologToConsole(inputFilePath: String) {
    println(Alloy2BParser(inputFilePath).getPrologTerm())
}

fun translatePrologToFile(inputFilePath: String, outputFilePath: String) {
    val file = File(outputFilePath)
    file.createNewFile()
    file.printWriter().use { out -> out.println(Alloy2BParser(inputFilePath).getPrologTerm()) }
}
