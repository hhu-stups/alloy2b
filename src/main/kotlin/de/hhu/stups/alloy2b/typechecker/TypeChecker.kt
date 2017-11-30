package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.typechecker.Type.*

class TypeChecker(spec: AlloySpecification) {

    init {
        typeCheck(spec)
    }

    private fun typeCheck(spec: AlloySpecification) {
        spec.declarations.fold(TypeEnvironment(), { te, dec -> typeCheck(te, dec) })
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

    private fun typeCheck(teIn: TypeEnvironment, stmt: CheckStatement) =
            stmt.expressions.fold(teIn, { te, expr -> typeCheckExpr(te, expr) })

    private fun typeCheck(teIn: TypeEnvironment, stmt: AssertionStatement): TypeEnvironment =
            stmt.expressions.fold(teIn, { te, expr -> typeCheckExpr(te, expr) })

    private fun typeCheck(teIn: TypeEnvironment, stmt: SignatureDeclaration): TypeEnvironment {
        val tEnv = stmt.names.fold(teIn, { te, name -> te.addType(name, SET) })
        return stmt.decls.fold(tEnv, { te, decl -> decl.names.fold(te, { te2, name -> te2.addType(name, RELATION) }) })
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: FactDeclaration): TypeEnvironment =
            stmt.expressions.fold(teIn, { te, expr -> typeCheckExpr(te, expr) })


    private fun typeCheck(teIn: TypeEnvironment, stmt: FunDeclaration): TypeEnvironment {
        val tEnv = stmt.decls.fold(teIn, { te, decl -> decl.names.fold(typeCheckExpr(te, decl.expression), { te2, name -> te2.addType(name, decl.expression.type ?: UNTYPED) }) })
        return stmt.expressions.fold(tEnv, { te, expr -> typeCheckExpr(te, expr) })
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: PredDeclaration): TypeEnvironment {
        val tEnv = stmt.decls.fold(teIn, { te, decl -> decl.names.fold(typeCheckExpr(te, decl.expression), { te2, name -> te2.addType(name, decl.expression.type ?: UNTYPED) }) })
        return stmt.expressions.fold(tEnv, { te, expr -> typeCheckExpr(te, expr) })
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

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifiedExpression): TypeEnvironment {
        val nte = typeCheckExpr(teIn, expr.expression)
        expr.type = expr.expression.type
        return nte
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerCastExpression): TypeEnvironment {
        expr.type = INTEGER
        return typeCheckExpr(teIn, expr.expr)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerSetExpression): TypeEnvironment {
        return teIn
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerExpression): TypeEnvironment {
        expr.type = INTEGER
        return teIn
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfExpression): TypeEnvironment {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
        typeCheckExpr(teIn, expr.elseExpr)
        return teIn
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: BlockExpression): TypeEnvironment {
        return expr.expressions.fold(teIn, { te, subExpr -> typeCheckExpr(te, subExpr) })
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifierExpression): TypeEnvironment {
        return checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: DeclListExpression): TypeEnvironment {
        return checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: IdentifierExpression): TypeEnvironment {
        expr.type = te.lookupType(expr)
        return te
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BinaryOperatorExpression): TypeEnvironment {
        typeCheckExpr(te, expr.left)
        typeCheckExpr(te, expr.right)
        if (expr.left.type == RELATION && expr.right.type == RELATION) {
            expr.type = RELATION
        } else {
            expr.type = SET
        }
        return te
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: UnaryOperatorExpression): TypeEnvironment {
        typeCheckExpr(te, expr.expression)
        expr.type = expr.expression.type
        return te
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: LetExpression): TypeEnvironment {
        val tEnv = expr.letDecls.fold(teIn, { te, decl ->
            typeCheckExpr(te, decl.expression).addType(decl.name, decl.expression.type ?: UNTYPED) })
        return expr.expressions.fold(tEnv, { te, e -> typeCheckExpr(te, e) })
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BoxJoinExpression): TypeEnvironment {
        typeCheckExpr(te, expr.left)
        expr.parameters.map { typeCheckExpr(te, it) }
        if (expr.left.type == RELATION && expr.parameters.any { it.type == RELATION }) {
            expr.type = RELATION
        } else {
            expr.type = SET
        }
        return te
    }

    private fun checkDeclsAndExpressions(teIn: TypeEnvironment, decls: List<Decl>, expressions: List<Expression>): TypeEnvironment {
        val tempTe = decls.fold(teIn, { te, decl -> decl.names.fold(typeCheckExpr(te, decl.expression), { te2, name -> te2.addType(name, decl.expression.type ?: UNTYPED) }) })
        return expressions.fold(tempTe, { te, subExpr -> typeCheckExpr(te, subExpr) })
    }
}