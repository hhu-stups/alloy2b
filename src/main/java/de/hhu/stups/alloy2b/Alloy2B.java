package de.hhu.stups.alloy2b;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;

import de.hhu.stups.alloy2b.translation.Alloy2BParser;
import de.hhu.stups.alloy2b.translation.Alloy2BParserErr;

public final class Alloy2B {
    private Alloy2B() {
        throw new AssertionError("Main class");
    }

    private static void translatePrologToConsole(String inputFilePath) throws Alloy2BParserErr {
        System.out.println(new Alloy2BParser().parseFromFile(inputFilePath).getPrologTerm());
    }

    private static void translatePrologToFile(String inputFilePath, String outputFilePath) throws Alloy2BParserErr, IOException {
        String prologTerm = new Alloy2BParser().parseFromFile(inputFilePath).getPrologTerm();
        Files.write(Paths.get(outputFilePath), Collections.singletonList(prologTerm));
    }

    public static void main(String[] args) throws Alloy2BParserErr, IOException {
        if (args.length == 2 && "-toProlog".equals(args[1])) {
            translatePrologToConsole(args[0]);
            return;
        }

        if (args.length == 3 && "-toProlog".equals(args[1])) {
            translatePrologToFile(args[0], args[2]);
            return;
        }

        System.out.println("Usage:");
        System.out.println("To print the translation to stdout:");
        System.out.println("    $ java -jar <path to alloy2b.jar> input.als -toProlog");
        System.out.println("Alternatively, to print the translation to a file:");
        System.out.println("    $ java -jar <path to alloy2b.jar> input.als -toProlog output.pl");
    }
}
