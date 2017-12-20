package de.hhu.stups.alloy2b

import de.hhu.stups.alloy2b.ast.SignatureDeclarations
import de.hhu.stups.alloy2b.ast.toAst
import de.hhu.stups.alloy2b.parsing.ParserFacade
import de.hhu.stups.alloy2b.typechecker.Signature
import de.hhu.stups.alloy2b.typechecker.Type
import de.hhu.stups.alloy2b.typechecker.TypeChecker
import junit.framework.TestCase.assertEquals
import de.hhu.stups.alloy2b.typechecker.Set
import org.junit.Assert
import org.junit.Test

class TypeCheckerTest {
    @Test
    fun typeSubSignatures() {
        val code = "sig test {}\nsig test2 extends test {}"

        val result = ParserFacade.parse(code)
        val ast = result.root!!.toAst()
        val errors = result.errors

        Assert.assertTrue(errors.isEmpty())

        TypeChecker(ast)

        var sigdecls = ast.declarations.get(0) as SignatureDeclarations
        assertEquals(1,sigdecls.signatures.size)
        assertEquals(Type(Set(Type(Signature("test")))),sigdecls.signatures.first().name.type)


        sigdecls = ast.declarations.get(1) as SignatureDeclarations
        assertEquals(1,sigdecls.signatures.size)
        assertEquals(Type(Set(Type(Signature("test")))),sigdecls.signatures.first().name.type)

    }
}