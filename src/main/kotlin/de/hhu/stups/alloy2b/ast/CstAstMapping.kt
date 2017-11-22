package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.antlr.AlloyParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token


interface ParseTreeToAstMapper<in CstNode : ParserRuleContext, out AstNode : Node> {
    fun map(parseTreeNode: CstNode) : AstNode
}

fun SpecificationContext.toAst(considerPosition: Boolean = false) : AlloySpecification = AlloySpecification(this.paragraph().map { it.toAst(considerPosition) }, toPosition(considerPosition))

fun Token.startPoint() = Point(line, charPositionInLine)

fun Token.endPoint() = Point(line, charPositionInLine + text.length)

fun ParserRuleContext.toPosition(considerPosition: Boolean) : Position? {
    return if (considerPosition) Position(start.startPoint(), stop.endPoint()) else null
}

fun ParagraphContext.toAst(considerPosition: Boolean = false) : Statement {
    val child = this.getChild(0)
    when(child) {
        is FactDeclContext -> return FactDeclaration(child.name().text, child.block().expr().map { it.toAst(considerPosition) }, toPosition(considerPosition))
        is SigDeclContext -> return SignatureDeclaration(child.name().text, child.declList()?.decls?.filterIsInstance<DeclContext>()?.map { it.toAst(considerPosition) } ?: emptyList(), child.block()?.expr()?.map { it.toAst(considerPosition) } ?: emptyList(), toPosition(considerPosition))
        else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
    }
}

fun ExprContext.toAst(considerPosition: Boolean = false) : Expression = when(this) {
    is IdExprContext -> IdentifierExpression(this.name().text, toPosition(considerPosition))
    is UnOpExprContext -> UnaryOperatorExpression(Operator.fromString(this.unOp().text), expr().toAst(considerPosition), toPosition(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun DeclContext.toAst(considerPosition: Boolean = false) : Decl = Decl(this.name().text, this.expr().toAst(considerPosition))


//fun StatementContext.toAst(considerPosition: Boolean = false) : Statement = when (this) {
//    is VarDeclarationStatementContext -> VarDeclaration(varDeclaration().assignment().ID().text, varDeclaration().assignment().expression().toAst(considerPosition), toPosition(considerPosition))
//    is AssignmentStatementContext -> Assignment(assignment().ID().text, assignment().expression().toAst(considerPosition), toPosition(considerPosition))
//    is PrintStatementContext -> Print(print().expression().toAst(considerPosition), toPosition(considerPosition))
//    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
//}

class AlloyParseTreeToAstMapper : ParseTreeToAstMapper<SpecificationContext, AlloySpecification> {
    override fun map(parseTreeNode: SpecificationContext): AlloySpecification = parseTreeNode.toAst()
}