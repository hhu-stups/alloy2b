package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.antlr.AlloyParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import java.util.Arrays.asList


interface ParseTreeToAstMapper<in CstNode : ParserRuleContext, out AstNode : Node> {
    fun map(parseTreeNode: CstNode) : AstNode
}

fun SpecificationContext.toAst(considerPosition: Boolean = false) : AlloySpecification = AlloySpecification(this.paragraph().map { it.toAst(considerPosition) }, toPosition(considerPosition))

fun Token.startPoint() = Point(line, charPositionInLine)

fun Token.endPoint() = Point(line, charPositionInLine + text.length)

fun ParserRuleContext.toPosition(considerPosition: Boolean) : Position? {
    return if (considerPosition) Position(start.startPoint(), stop.endPoint()) else null
}

fun NameContext.toAst(considerPosition: Boolean = false) : Expression =
        IdentifierExpression(this.text, toPosition(considerPosition))

fun ParagraphContext.toAst(considerPosition: Boolean = false) : Statement {
    val child = this.getChild(0)
    when(child) {
        is FactDeclContext -> return FactDeclaration(child.name()?.text ?: "", child.block().expr().map { it.toAst(considerPosition) }, toPosition(considerPosition))
        is SigDeclContext -> return SignatureDeclaration(child.name()?.text ?: "", child.sigExt()?.toAst(considerPosition), child.declList()?.decls?.filterIsInstance<DeclContext>()?.map { it.toAst(considerPosition) } ?: emptyList(), child.block()?.expr()?.map { it.toAst(considerPosition) } ?: emptyList(), toPosition(considerPosition))
        is AssertDeclContext -> return AssertionStatement(child.name()?.text ?: "", child.block().toAst(considerPosition), toPosition(considerPosition))
        is CmdDeclContext -> return CheckStatement(child.cmdname?.text ?: "", child.block()?.toAst(considerPosition) ?: asList(child.name(0).toAst(considerPosition)), toPosition(considerPosition))
        else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
    }
}

fun SigExtContext.toAst(considerPosition: Boolean = false) : SignatureExtension = when(this) {
    is ExtendsExtensionContext -> NameSignatureExtension(this.ref().text, toPosition(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun ExprContext.toAst(considerPosition: Boolean = false) : Expression = when(this) {
    is IdExprContext -> IdentifierExpression(this.name().text, toPosition(considerPosition))
    is UnOpExprContext -> UnaryOperatorExpression(Operator.fromString(this.unOp().text), expr().toAst(considerPosition), toPosition(considerPosition))
    is BinOpExprContext -> BinaryOperatorExpression(Operator.fromString(this.binOp().text), expr().map { it.toAst(considerPosition) }, toPosition(considerPosition))
    is QuantExprContext -> QuantifiedExpression(Operator.fromString(this.quant().text), declList()?.decls?.filterIsInstance<DeclContext>()?.map { it.toAst(considerPosition) } ?: emptyList(), blockOrBar().toAst(considerPosition), toPosition(considerPosition))
    is CompareExprContext -> BinaryOperatorExpression(Operator.fromString(this.compareOp().text), expr().map { it.toAst(considerPosition) }, toPosition(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun BlockContext.toAst(considerPosition: Boolean = false) : List<Expression> =
        expr().map { it.toAst(considerPosition) }

fun BlockOrBarContext.toAst(considerPosition: Boolean = false) : List<Expression> = when(this) {
    is ExprInBlockOrBarContext -> asList(expr().toAst(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun DeclContext.toAst(considerPosition: Boolean = false) : Decl = Decl(this.name().text, this.expr().toAst(considerPosition))

class AlloyParseTreeToAstMapper : ParseTreeToAstMapper<SpecificationContext, AlloySpecification> {
    override fun map(parseTreeNode: SpecificationContext): AlloySpecification = parseTreeNode.toAst()
}