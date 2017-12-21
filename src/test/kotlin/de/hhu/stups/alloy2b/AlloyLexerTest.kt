package de.hhu.stups.alloy2b

import org.junit.Assert.assertEquals
import org.junit.Test as test

class AlloyLexerTest {
    @test
    fun tokenizeEmptySignatureDeclaration() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "RBRACKET", "EOF"),
                tokens(lexerForCode("sig SomeName { }")))
    }

    @test
    fun tokenizeLineCommentEmptySignatureDeclaration() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "RBRACKET", "EOF"),
                tokens(lexerForCode("// line comment\nsig SomeName { }")))
    }

    @test
    fun tokenizeBlockCommentEmptySignatureDeclaration() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "RBRACKET", "EOF"),
                tokens(lexerForCode("/* block comment */\nsig SomeName { }")))
    }

    @test
    fun tokenizeLineCommentSignatureDeclarationWithFields() {
        assertEquals(listOf("SIG", "ID", "LBRACKET", "ID", "COLON", "LONE", "ID", "RBRACKET", "EOF"),
                tokens(lexerForCode("// A file system object in the file system\nsig FSObject { parent: lone Dir }")))
    }

    @test
    fun tokenizeBoxJoin() {
        assertEquals(listOf("ID", "DOT", "ID", "LSQBRACKET", "ID", "RSQBRACKET", "EOF"),
                tokens(lexerForCode("a.b[c]")))
    }

    @test
    fun partOfSudokuModel() {
        assertEquals(listOf("ALL", "ID", "COLON", "ID", "DASH", "ID", "LSQBRACKET", "ID", "RSQBRACKET", "LSQBRACKET", "ID", "RSQBRACKET", "EQUAL", "ID", "EOF"),
                tokens(lexerForCode("all r : Row		| array[r][Column]	= Number")))
    }

    @test
    fun tokenizeArrorOp() {
        assertEquals(listOf("ID","COLON","ID","ARROW","LONE","ID","EOF"),
                tokens(lexerForCode("mother : Person -> lone women")))
    }

    @test
    fun tokenizeQuantifiedDeclListExpression() {
        assertEquals(listOf("PRED", "ID", "LBRACKET", "NO", "DISJ", "ID", "COMMA", "ID", "COLON", "ID", "DASH", "ID", "LSQBRACKET", "ID", "COMMA", "ID", "RSQBRACKET", "EOF"),
                tokens(lexerForCode("pred Show {\n no disj q1,q2:Queen| aligned[q1,q2]")))
    }
}