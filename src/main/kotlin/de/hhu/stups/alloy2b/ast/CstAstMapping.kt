package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.antlr.AlloyParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import java.util.Arrays.asList

fun SpecificationContext.toAst(considerPosition: Boolean = false): AlloySpecification =
        AlloySpecification(this.paragraph().map { it.toAst(considerPosition) }, toPosition(considerPosition))

fun Token.startPoint() = Point(line, charPositionInLine)

fun Token.endPoint() = Point(line, charPositionInLine + text.length)

fun ParserRuleContext.toPosition(considerPosition: Boolean): Position? {
    return if (considerPosition) Position(start.startPoint(), stop.endPoint()) else null
}

fun NameContext.toAst(considerPosition: Boolean = false): Expression =
        IdentifierExpression(this.text, toPosition(considerPosition))

fun ParagraphContext.toAst(considerPosition: Boolean = false): Statement {
    val child = this.getChild(0)
    when (child) {
        is FactDeclContext -> return FactDeclaration(child.name()?.text ?: "",
                child.block().expr().map { it.toAst(considerPosition) },
                toPosition(considerPosition))
        is SigDeclContext -> return SignatureDeclaration(child.sigQual().map { Operator.fromString(it.text) },
                child.name()?.map({ it.text }) ?: emptyList(), child.sigExt()?.toAst(considerPosition),
                child.declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(),
                child.block()?.expr()?.map { it.toAst(considerPosition) } ?: emptyList(), toPosition(considerPosition))
        is AssertDeclContext -> return AssertionStatement(child.name()?.text ?: "",
                child.block().toAst(considerPosition),
                toPosition(considerPosition))
        is CmdDeclContext -> return CheckStatement(child.cmdname?.text ?: "",
                child.block()?.toAst(considerPosition) ?: asList(child.name(0).toAst(considerPosition)),
                toPosition(considerPosition))
        is FunDeclContext -> return FunDeclaration(child.name()?.text ?: "",
                child.declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(),
                child.block().expr().map { it.toAst(considerPosition) },
                toPosition(considerPosition))
        is PredDeclContext -> return PredDeclaration(child.name()?.text ?: "",
                child.declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(),
                child.block().expr().map { it.toAst(considerPosition) },
                toPosition(considerPosition))
        else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
    }
}

fun SigExtContext.toAst(considerPosition: Boolean = false): SignatureExtension = when (this) {
    is ExtendsExtensionContext -> NameSignatureExtension(this.ref().text, toPosition(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun ExprContext.toAst(considerPosition: Boolean = false): Expression =
        when (this) {
            is IdExprContext -> IdentifierExpression(this.name().text, toPosition(considerPosition))
            is UnOpExprContext -> UnaryOperatorExpression(Operator.fromString(this.unOp().text),
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            is QuantifiedExprContext -> UnaryOperatorExpression(Operator.fromString(this.exprQuantifier().text),
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            is BinOpExprContext -> BinaryOperatorExpression(Operator.fromString(this.binOp().text),
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is ImpliesExprContext -> BinaryOperatorExpression(Operator.fromString(this.IMPLIES().text),
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is DotJoinExprContext -> BinaryOperatorExpression(Operator.fromString(this.DOT().text),
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is BoxJoinExprContext -> BoxJoinExpression(expr().toAst(considerPosition),
                    exprList().expr().map { it.toAst(considerPosition) },
                    toPosition(considerPosition))
            is QuantExprContext -> QuantifiedExpression(Operator.fromString(this.quant().text),
                    declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(),
                    blockOrBar().toAst(considerPosition),
                    toPosition(considerPosition))
            is CompareExprContext -> BinaryOperatorExpression(Operator.fromString(this.compareOp().text),
                    left.toAst(considerPosition), right.toAst(considerPosition),
                    toPosition(considerPosition))
            is ArrowOpExprContext -> BinaryOperatorExpression(Operator.fromString(this.arrowOp().text),
                    left.toAst(considerPosition), right.toAst(considerPosition),
                    toPosition(considerPosition))
            is LetExprContext -> LetExpression(this.letDecl().map { it.toAst(considerPosition) },
                    this.blockOrBar().toAst(considerPosition),
                    toPosition(considerPosition))
            is ParenExprContext -> this.expr().toAst(considerPosition)
            else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
        }

fun BlockContext.toAst(considerPosition: Boolean = false): List<Expression> =
        expr().map { it.toAst(considerPosition) }

fun BlockOrBarContext.toAst(considerPosition: Boolean = false): List<Expression> = when (this) {
    is ExprInBlockOrBarContext -> asList(expr().toAst(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun DeclContext.toAst(considerPosition: Boolean = false): Decl = Decl(this.name().map { it.text }, this.expr().toAst(considerPosition))
fun LetDeclContext.toAst(considerPosition: Boolean = false): LetDecl = LetDecl(this.name().text, this.expr().toAst(considerPosition))
