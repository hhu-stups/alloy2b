package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.antlr.AlloyParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import java.util.Arrays.asList

fun SpecificationContext.toAst(considerPosition: Boolean = false): AlloySpecification =
        AlloySpecification(this.open().map {it.toAst(considerPosition)} + this.paragraph().map { it.toAst(considerPosition) }, toPosition(considerPosition))

fun Token.startPoint() = Point(line, charPositionInLine)

fun Token.endPoint() = Point(line, charPositionInLine + text.length)

fun ParserRuleContext.toPosition(considerPosition: Boolean): Position? {
    return if (considerPosition) Position(start.startPoint(), stop.endPoint()) else null
}

fun OpenContext.toAst(considerPosition: Boolean = false) : OpenStatement =
        OpenStatement(this.name().map { it.toAst(considerPosition) }, this.ref().map { it.toAst(considerPosition) })

fun NameContext.toAst(considerPosition: Boolean = false): IdentifierExpression =
        IdentifierExpression(this.text, toPosition(considerPosition))

fun ParagraphContext.toAst(considerPosition: Boolean = false): Statement {
    val child = this.getChild(0)
    when (child) {
        is FactDeclContext -> return FactDeclaration(child.name()?.text ?: "",
                child.block().expr().map { it.toAst(considerPosition) },
                toPosition(considerPosition))
        is SigDeclContext -> return this.sigDecl().toAst(considerPosition)
        is AssertDeclContext -> return AssertionStatement(child.name()?.text ?: "",
                child.block().toAst(considerPosition),
                toPosition(considerPosition))
        is CheckStatementContext -> return CheckStatement(child.cmdname?.text ?: "",
                child.block()?.toAst(considerPosition) ?: asList(child.name(0).toAst(considerPosition)),
                toPosition(considerPosition))
        is RunStatementContext -> return RunStatement(child.cmdname?.text ?: "",
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
        is EnumDeclContext -> return EnumDeclaration(child.name()?.get(0)?.text ?: "",
                child.name().subList(1, child.name().size).map { it.text }, toPosition(considerPosition))
        else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
    }
}

fun SigDeclContext.toAst(considerPosition: Boolean = false): SignatureDeclarations {
    val qualifiers = sigQual().map { Operator.fromString(it.text) }
    val signatureExtensions = sigExt()?.toAst(considerPosition)
    val decls = declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList()

    return SignatureDeclarations(name().map {
        SignatureDeclaration(qualifiers,
                it.toAst(considerPosition), signatureExtensions,
                decls,
                quantifiedBlock(it.toAst(considerPosition), block()?.toAst(considerPosition)), toPosition(considerPosition))
    })
}

fun quantifiedBlock(signatureName: IdentifierExpression, block: List<Expression>?): QuantifierExpression? {
    if (block == null) {
        return null
    }
    val thisDecl = Decl(asList(IdentifierExpression("this")), QuantifiedExpression(Operator.ONE, signatureName))
    return QuantifierExpression(Operator.ALL, asList(thisDecl), block)
}

fun SigExtContext.toAst(considerPosition: Boolean = false): SignatureExtension = when (this) {
    is ExtendsExtensionContext -> ExtendsSignatureExtension(this.ref().toAst(considerPosition), toPosition(considerPosition))
    is InExtensionContext -> InSignatureExtension(this.ref().map { it.toAst(considerPosition) }, toPosition(considerPosition))
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun RefContext.toAst(considerPosition: Boolean = false): IdentifierExpression =
        IdentifierExpression(this.text, toPosition(considerPosition))

fun ExprContext.toAst(considerPosition: Boolean = false): Expression =
        when (this) {
            is IdExprContext -> IdentifierExpression(this.name().text, toPosition(considerPosition))
            is UnOpExprContext -> UnaryOperatorExpression(Operator.fromString(this.unOp().text),
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            is QuantifiedExprContext -> QuantifiedExpression(Operator.fromString(this.exprQuantifier().text),
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            is BinOpExprContext -> BinaryOperatorExpression(Operator.fromString(this.binOp().text),
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is ImpliesExprContext -> if (elseExpr != null) {
                IfElseExpression(ifExpr.toAst(considerPosition), thenExpr.toAst(considerPosition), elseExpr.toAst(considerPosition), toPosition(considerPosition))
            } else {
                IfExpression(ifExpr.toAst(considerPosition), thenExpr.toAst(considerPosition), toPosition(considerPosition))
            }
            is DotJoinExprContext -> BinaryOperatorExpression(Operator.fromString(this.DOT().text),
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is BoxJoinExprContext -> BoxJoinExpression(expr().toAst(considerPosition),
                    exprList().expr().map { it.toAst(considerPosition) },
                    toPosition(considerPosition))
            is QuantifierExprContext -> QuantifierExpression(Operator.fromString(this.quant().text),
                    declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(),
                    blockOrBar().toAst(considerPosition),
                    toPosition(considerPosition))
            is CompareExprContext -> if (NOT() != null) {UnaryOperatorExpression(Operator.NOT, BinaryOperatorExpression(Operator.fromString(this.compareOp().text),
                    left.toAst(considerPosition), right.toAst(considerPosition),
                    toPosition(considerPosition)))} else {BinaryOperatorExpression(Operator.fromString(this.compareOp().text),
                    left.toAst(considerPosition), right.toAst(considerPosition),
                    toPosition(considerPosition))}
            is ArrowOpExprContext -> BinaryOperatorExpression(Operator.fromString(this.arrowOp().text),
                    left.toAst(considerPosition), right.toAst(considerPosition),
                    toPosition(considerPosition))
            is LetExprContext -> LetExpression(this.letDecl().map { it.toAst(considerPosition) },
                    this.blockOrBar().toAst(considerPosition),
                    toPosition(considerPosition))
            is ParenExprContext -> this.expr().toAst(considerPosition)
            is BlockExprContext -> BlockExpression(this.block().expr().map { it.toAst(considerPosition) })
            is DeclListExprContext -> DeclListExpression(declList()?.decl()?.map { it.toAst(considerPosition) } ?: emptyList(), blockOrBar().toAst(considerPosition), toPosition(considerPosition))
            is CapIntExprContext -> IntegerSetExpression(toPosition(considerPosition))
            is NumberExprContext -> IntegerExpression(NUMBER().text.toLong(), toPosition(considerPosition))
            is IntCastExprContext -> IntegerCastExpression(expr().toAst(considerPosition), toPosition(considerPosition))
            is IdenExprContext -> IdentityExpression(toPosition(considerPosition))
            is UnivExprContext -> UnivExpression(toPosition(considerPosition))
            is CardExprContext -> UnaryOperatorExpression(Operator.CARD,
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            is ConjunctionExprContext -> BinaryOperatorExpression(Operator.AND,
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is DisjunctionExprContext -> BinaryOperatorExpression(Operator.OR,
                    left.toAst(considerPosition),
                    right.toAst(considerPosition),
                    toPosition(considerPosition))
            is IntersectionExprContext -> BinaryOperatorExpression(Operator.INTERSECTION,
                    left.toAst(considerPosition),
                    right.toAst(considerPosition))
            is RestrictionOpExprContext -> BinaryOperatorExpression(Operator.DOM_RESTR,
                    left.toAst(considerPosition),
                    right.toAst(considerPosition))
            is OverrideExprContext -> BinaryOperatorExpression(Operator.OVERRIDE,
                    left.toAst(considerPosition),
                    right.toAst(considerPosition))
            is NegatedExprContext -> UnaryOperatorExpression(Operator.NOT,
                    expr().toAst(considerPosition),
                    toPosition(considerPosition))
            else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
        }

fun BlockContext.toAst(considerPosition: Boolean = false): List<Expression> =
        expr().map { it.toAst(considerPosition) }

fun BlockOrBarContext.toAst(considerPosition: Boolean = false): List<Expression> = when (this) {
    is ExprInBlockOrBarContext -> asList(expr().toAst(considerPosition))
    is BlockInBlockOrBarContext -> this.block().toAst(considerPosition)
    else -> throw UnsupportedOperationException(this.javaClass.canonicalName)
}

fun DeclContext.toAst(considerPosition: Boolean = false): Decl {
    var expr = expr().toAst(considerPosition)
    if (!(expr is QuantifiedExpression)) {
        expr = QuantifiedExpression(Operator.ONE, expr, expr.position)
    }
    return Decl(this.name().map { IdentifierExpression(it.text, it.toPosition(considerPosition)) }, expr)
}

fun LetDeclContext.toAst(considerPosition: Boolean = false): LetDecl = LetDecl(IdentifierExpression(this.name().text, this.toPosition(considerPosition)), this.expr().toAst(considerPosition))
