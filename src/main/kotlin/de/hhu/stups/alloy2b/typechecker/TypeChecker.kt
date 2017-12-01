package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.typechecker.Types.*

class TypeChecker(spec: AlloySpecification) {

    init {
        typeCheck(spec)
    }

    private fun typeCheck(spec: AlloySpecification) {
        val te = TypeEnvironment()
        spec.declarations.forEach { dec -> typeCheck(te, dec) }
    }

    private fun typeCheck(te: TypeEnvironment, stmt: Statement) =
            when (stmt) {
                is CheckStatement -> typeCheck(te, stmt)
                is AssertionStatement -> typeCheck(te, stmt)
                is SignatureDeclaration -> typeCheck(te, stmt)
                is FactDeclaration -> typeCheck(te, stmt)
                is FunDeclaration -> typeCheck(te, stmt)
                is PredDeclaration -> typeCheck(te, stmt)
                else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
            }

    private fun typeCheck(teIn: TypeEnvironment, stmt: CheckStatement) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }


    private fun typeCheck(teIn: TypeEnvironment, stmt: AssertionStatement) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }

    private fun typeCheck(te: TypeEnvironment, stmt: SignatureDeclaration) {
        stmt.names.forEach { name -> te.addType(name, SET) }
        stmt.decls.forEach { decl -> decl.names.forEach { name -> name.type.setType(RELATION); te.addType(name.name, RELATION) }}
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: FactDeclaration) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }


    private fun typeCheck(te: TypeEnvironment, stmt: FunDeclaration) {
        val localTe = te.copy()
        stmt.decls.forEach { decl -> decl.names.forEach { name -> name.type.setType(SET); localTe.addType(name.name, SET); typeCheckExpr(localTe, decl.expression) }}
        stmt.expressions.forEach { expr -> typeCheckExpr(localTe, expr) }
    }

    private fun typeCheck(te: TypeEnvironment, stmt: PredDeclaration) {
        val localTe = te.copy()
        stmt.decls.forEach { decl -> decl.names.forEach { name -> name.type.setType(SET); localTe.addType(name.name, SET); typeCheckExpr(localTe, decl.expression) }}
        stmt.expressions.forEach { expr -> typeCheckExpr(localTe, expr) }
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: Expression) =
            when (expr) {
                is QuantifierExpression -> typeCheckExpr(te, expr)
                is IdentifierExpression -> typeCheckExpr(te, expr)
                is BinaryOperatorExpression -> typeCheckExpr(te, expr)
                is UnaryOperatorExpression -> typeCheckExpr(te, expr)
                is LetExpression -> typeCheckExpr(te, expr)
                is BoxJoinExpression -> typeCheckExpr(te, expr)
                is BlockExpression -> typeCheckExpr(te, expr)
                is IfExpression -> typeCheckExpr(te, expr)
                is DeclListExpression -> typeCheckExpr(te, expr)
                is IntegerCastExpression -> typeCheckExpr(te, expr)
                is IntegerExpression -> typeCheckExpr(te, expr)
                is IntegerSetExpression -> typeCheckExpr(te, expr)
                is QuantifiedExpression -> typeCheckExpr(te, expr)
                else -> throw UnsupportedOperationException(expr.javaClass.canonicalName)
            }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifiedExpression) {
        typeCheckExpr(teIn, expr.expression)
        if (expr.operator == Operator.ONE) {
            expr.type = Type(SCALAR)
        } else {
            expr.type = expr.expression.type
        }
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerCastExpression) {
        expr.type = Type(INTEGER)
        typeCheckExpr(teIn, expr.expr)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerSetExpression) {
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerExpression) {
        expr.type = Type(INTEGER)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfExpression) {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
        typeCheckExpr(teIn, expr.elseExpr)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: BlockExpression) {
        expr.expressions.forEach {subExpr -> typeCheckExpr(teIn, subExpr) }
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifierExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: DeclListExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: IdentifierExpression) {
        expr.type = te.lookupType(expr)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BinaryOperatorExpression) {
        typeCheckExpr(te, expr.left)
        typeCheckExpr(te, expr.right)
        if (expr.left.type.currentType == RELATION && expr.right.type.currentType == RELATION) {
            expr.type.setType(RELATION)
        } else {
            expr.type.setType(SET)
        }
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: UnaryOperatorExpression) {
        typeCheckExpr(te, expr.expression)
        expr.type = expr.expression.type
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: LetExpression) {
        expr.letDecls.forEach { decl ->
            typeCheckExpr(te, decl.expression); te.addType(decl.name.name, decl.expression.type)
        }
        expr.expressions.forEach { typeCheckExpr(te, it) }
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BoxJoinExpression) {
        typeCheckExpr(te, expr.left)
        expr.parameters.map { typeCheckExpr(te, it) }
        if (expr.left.type.currentType == RELATION && expr.parameters.any { it.type.currentType == RELATION }) {
            expr.type.setType(RELATION)
        } else {
            expr.type.setType(SET)
        }
    }

    private fun checkDeclsAndExpressions(te: TypeEnvironment, decls: List<Decl>, expressions: List<Expression>) {
        val localTe = te.copy()
        decls.forEach { decl -> decl.names.forEach { name -> typeCheckExpr(localTe, decl.expression); name.type = decl.expression.type; localTe.addType(name.name, decl.expression.type) }}
        expressions.forEach { subExpr -> typeCheckExpr(localTe, subExpr) }
    }
}