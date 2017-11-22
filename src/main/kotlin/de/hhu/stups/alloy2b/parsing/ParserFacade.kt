package de.hhu.stups.alloy2b.parsing;

import de.hhu.stups.alloy2b.antlr.AlloyLexer
import de.hhu.stups.alloy2b.antlr.AlloyParser
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.nio.charset.Charset
import java.util.*

data class Error(val message: String)

data class ParsingResult(val root : AlloyParser.SpecificationContext?, val errors: List<Error>) {
    fun isCorrect() = errors.isEmpty() && root != null
}

fun String.toStream(charset: Charset = Charsets.UTF_8) = ByteArrayInputStream(toByteArray(charset))

object ParserFacade {

    fun parse(code: String) : ParsingResult = parse(code.toStream())

    fun parse(file: File) : ParsingResult = parse(FileInputStream(file))

    fun parse(inputStream: InputStream) : ParsingResult {
        val errors = LinkedList<Error>()
        val errorListener = object : ANTLRErrorListener {
            override fun reportAmbiguity(p0: Parser?, p1: DFA?, p2: Int, p3: Int, p4: Boolean, p5: BitSet?, p6: ATNConfigSet?) {
                // Ignored for now
            }

            override fun reportAttemptingFullContext(p0: Parser?, p1: DFA?, p2: Int, p3: Int, p4: BitSet?, p5: ATNConfigSet?) {
                // Ignored for now
            }

            override fun syntaxError(recognizer: Recognizer<*, *>?, offendingSymbol: Any?, line: Int, charPositionInline: Int, msg: String?, ex: RecognitionException?) {
                errors.add(Error("Error at L$line:$charPositionInline: $msg"))
            }

            override fun reportContextSensitivity(p0: Parser?, p1: DFA?, p2: Int, p3: Int, p4: Int, p5: ATNConfigSet?) {
                // Ignored for now
            }
        }

        val lexer = AlloyLexer(CharStreams.fromStream(inputStream))
        lexer.removeErrorListeners()
        lexer.addErrorListener(errorListener)
        val parser = AlloyParser(CommonTokenStream(lexer))
        parser.removeErrorListeners()
        parser.addErrorListener(errorListener)
        val root = parser.specification()
        return ParsingResult(root, errors)
    }

}