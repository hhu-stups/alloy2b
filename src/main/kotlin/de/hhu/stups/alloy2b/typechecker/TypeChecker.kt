package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*

class TypeChecker(spec: AlloySpecification) {

    init {
        typeCheck(spec)
    }

    private fun typeCheck(spec: AlloySpecification) {
        val te = TypeEnvironment()
        // first pass: collect signatures
        spec.declarations.filter { it is SignatureDeclarations }.forEach{collectSignatureType(te, it as SignatureDeclarations)}
        spec.declarations.forEach { dec -> typeCheck(te, dec) }
    }

    private fun collectSignatureType(te: TypeEnvironment, sdecs: SignatureDeclarations) {
        sdecs.signatures.forEach({
            if (it.qualifiers.contains(Operator.ONE)) {
                it.name.type.setType(Scalar(Type(Signature(it.name.name))))
                te.addType(it.name.name, Scalar(Type(Signature(it.name.name))))
            } else {
                it.name.type.setType(Set(Type(Signature(it.name.name))))
                te.addType(it.name.name, Set(Type(Signature(it.name.name))))
            }
        it.decls.forEach({d -> d.names.forEach({name -> te.addType(name.name,Relation(Type(Untyped()),Type(Untyped())))})})})
    }


    private fun typeCheck(te: TypeEnvironment, stmt: Statement) =
            when (stmt) {
                is CheckStatement -> typeCheck(te, stmt)
                is AssertionStatement -> typeCheck(te, stmt)
                is SignatureDeclarations -> typeCheck(te, stmt)
                is FactDeclaration -> typeCheck(te, stmt)
                is FunDeclaration -> typeCheck(te, stmt)
                is PredDeclaration -> typeCheck(te, stmt)
                is EnumDeclaration -> typeCheck(te, stmt)
                else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
            }

    @Suppress("UNUSED_PARAMETER")
    private fun typeCheck(teIn: TypeEnvironment, stmt: EnumDeclaration) {
        //
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: CheckStatement) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: AssertionStatement) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: SignatureDeclarations) {
        stmt.signatures.forEach({ declaration -> typeCheck(teIn, declaration) })
    }

    private fun typeCheck(te: TypeEnvironment, stmt: SignatureDeclaration) {
        // signatures themselves are typed in a first pass
        // only decls and expressions have to be typed here

        stmt.decls.forEach { decl ->
            decl.names.forEach { idExpr ->
                run {
                    typeCheckExpr(te, decl.expression)
                    val type = Relation(Type(Set(Type(Signature(stmt.name.name)))), decl.expression.expression.type)
                    idExpr.type.setType(type)
                    te.addType(idExpr.name, type)
                }
            }
        }

        if (stmt.expression != null) {
            typeCheckExpr(te, stmt.expression)
        }
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: FactDeclaration) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }

    private fun typeCheck(te: TypeEnvironment, stmt: FunDeclaration) {
        checkDeclsAndExpressions(te, stmt.decls, stmt.expressions)
        te.addType(stmt.name,stmt.expressions.last().type)
    }

    private fun typeCheck(te: TypeEnvironment, stmt: PredDeclaration) {
        checkDeclsAndExpressions(te, stmt.decls, stmt.expressions)
        te.addType(stmt.name,stmt.expressions.last().type)
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
                is IfElseExpression -> typeCheckExpr(te, expr)
                is DeclListExpression -> typeCheckExpr(te, expr)
                is IntegerCastExpression -> typeCheckExpr(te, expr)
                is IntegerExpression -> typeCheckExpr(te, expr)
                is IntegerSetExpression -> typeCheckExpr(te, expr)
                is QuantifiedExpression -> typeCheckExpr(te, expr)
                is IdentityExpression -> typeCheckExpr(te, expr)
                is UnivExpression -> typeCheckExpr(te, expr)
                else -> throw UnsupportedOperationException(expr.javaClass.canonicalName)
            }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifiedExpression) {
        typeCheckExpr(teIn, expr.expression)
        if (expr.operator == ONE) {
            expr.type = Type(Scalar(expr.expression.type))
        } else {
            expr.type = expr.expression.type
        }
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerCastExpression) {
        expr.type = Type(Integer())
        typeCheckExpr(teIn, expr.expr)
    }

    private fun typeCheckExpr(@Suppress("UNUSED_PARAMETER") teIn: TypeEnvironment, expr: IntegerSetExpression) {
        expr.type = Type(Set(Type(Integer())))
    }

    private fun typeCheckExpr(@Suppress("UNUSED_PARAMETER") teIn: TypeEnvironment, expr: IntegerExpression) {
        expr.type = Type(Integer())
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfExpression) {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfElseExpression) {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
        typeCheckExpr(teIn, expr.elseExpr)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: BlockExpression) {
        expr.expressions.forEach { subExpr -> typeCheckExpr(teIn, subExpr) }
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifierExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: DeclListExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
        expr.type.setType(expr.expressions.last().type.currentType)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: IdentifierExpression) {
        expr.type = te.lookupType(expr)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BinaryOperatorExpression) {
        typeCheckExpr(te, expr.left)
        typeCheckExpr(te, expr.right)
        if(expr.operator == JOIN) {
            typeCheckJoinExpr(te, expr)
            return
        }
        if (expr.right is IdentityExpression) run {
            expr.right.type.setType(expr.left.type.currentType)
            return
        }
        val exprRightType = expr.right.type.currentType
        if (expr.left is UnivExpression && exprRightType is Relation) {
            expr.type.setType(Set(exprRightType.leftType))
            return
        }
        val exprLeftType = expr.left.type.currentType
        if (expr.right is UnivExpression && exprLeftType is Relation) {
            expr.type.setType(Set(exprLeftType.rightType))
            return
        }
        if (expr.left.type.currentType is Relation && expr.right.type.currentType is Relation) {
            expr.type.setType(expr.left.type.currentType)
            return
        }
        expr.type.setType(Set(expr.left.type))
    }

    private fun typeCheckJoinExpr(te: TypeEnvironment, je: BinaryOperatorExpression) {
        val jeRightType = je.right.type.currentType
        val jeLeftType = je.left.type.currentType
        if (je.left is UnivExpression && jeRightType is Relation) {
            je.type.setType(jeRightType.rightType.currentType)
        } else if (je.right is UnivExpression && jeLeftType is Relation) {
            je.type.setType(jeLeftType.leftType.currentType)
        } else if (jeLeftType is Relation && jeRightType is Relation) {
            je.type.setType(Relation(jeLeftType.leftType,jeRightType.rightType))
        } else if (jeLeftType is Relation && (jeRightType is Set || jeRightType is Scalar)) {
            je.type.setType(Set(jeLeftType.leftType))
        } else if ((jeLeftType is Set || jeLeftType is Scalar) && jeRightType is Relation) {
            je.type.setType(Set(jeRightType.rightType))
        } else {
            throw UnsupportedOperationException("join typechecking failed: ${je.left} . ${je.right}")
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
        if (expr.left.type.currentType is Relation && expr.parameters.any { it.type.currentType is Relation }) {
            expr.type.setType(Relation(expr.left.type, expr.left.type))
            return
        }
        expr.type.setType(Set(expr.left.type))
    }

    private fun checkDeclsAndExpressions(te: TypeEnvironment, decls: List<Decl>, expressions: List<Expression>) {
        decls.forEach { decl -> decl.names.forEach { name -> typeCheckExpr(te, decl.expression); name.type = decl.expression.type; te.addLocalType(name.name, decl.expression.type) } }
        expressions.forEach { subExpr -> typeCheckExpr(te, subExpr) }
    }

    @Suppress("UNUSED_PARAMETER")
    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IdentityExpression) {
        //
    }

    @Suppress("UNUSED_PARAMETER")
    private fun typeCheckExpr(teIn: TypeEnvironment, expr: UnivExpression) {
        //
    }
}