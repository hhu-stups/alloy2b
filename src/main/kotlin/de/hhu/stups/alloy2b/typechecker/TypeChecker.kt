package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.typechecker.Type.*

class TypeChecker(spec: AlloySpecification) {

    init {
        typeCheck(spec)
    }

    private fun typeCheck(spec: AlloySpecification) {
        spec.declarations.fold(TypeEnvironment(),{te, dec -> typeCheck(te, dec)})
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

    private fun typeCheck(te: TypeEnvironment, stmt: CheckStatement) =
            stmt.expressions.fold(te, {te, expr -> typeCheckExpr(te, expr)})

    private fun typeCheck(te: TypeEnvironment, stmt: AssertionStatement) = te // TODO

    private fun typeCheck(te: TypeEnvironment, stmt: SignatureDeclaration): TypeEnvironment {
        val tEnv = stmt.names.fold(te, { te, name -> te.addType(name, UNARY) })
        return stmt.decls.fold(tEnv, {te, decl -> te.addType(decl.name, BINARY)})
    }

    private fun typeCheck(te: TypeEnvironment, stmt: FactDeclaration) = te // TODO

    private fun typeCheck(te: TypeEnvironment, stmt: FunDeclaration) = te // TODO

    private fun typeCheck(te: TypeEnvironment, stmt: PredDeclaration) = te // TODO

    private fun typeCheckExpr(te: TypeEnvironment, expr: Expression) =
            when (expr) {
                is QuantifiedExpression -> typeCheckExpr(te, expr)
                is IdentifierExpression -> typeCheckExpr(te, expr)
                is BinaryOperatorExpression -> typeCheckExpr(te, expr)
                is UnaryOperatorExpression -> typeCheckExpr(te, expr)
                is LetExpression -> typeCheckExpr(te, expr)
                is BoxJoinExpression -> typeCheckExpr(te, expr)
                else -> throw UnsupportedOperationException(expr.javaClass.canonicalName)
            }

    private fun typeCheckExpr(te: TypeEnvironment, expr: QuantifiedExpression) = te // TODO
    private fun typeCheckExpr(te: TypeEnvironment, expr: IdentifierExpression) = te // TODO
    private fun typeCheckExpr(te: TypeEnvironment, expr: BinaryOperatorExpression) = te // TODO
    private fun typeCheckExpr(te: TypeEnvironment, expr: UnaryOperatorExpression) = te // TODO
    private fun typeCheckExpr(te: TypeEnvironment, expr: LetExpression) = te // TODO
    private fun typeCheckExpr(te: TypeEnvironment, expr: BoxJoinExpression) = te // TODO
}