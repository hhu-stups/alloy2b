package de.hhu.stups.alloy2b.typechecker

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*
import javax.naming.OperationNotSupportedException

class TypeChecker(spec: AlloySpecification) {

    init {
        typeCheck(spec)
    }

    private fun typeCheck(spec: AlloySpecification) {
        val te = TypeEnvironment()
        // first pass: collect signatures
        spec.declarations.filter { it is SignatureDeclarations }.forEach { collectSignatureType(te, it as SignatureDeclarations) }
        spec.declarations.forEach { dec -> typeCheck(te, dec) }
    }

    private fun collectSignatureType(te: TypeEnvironment, sdecs: SignatureDeclarations) {
        sdecs.signatures.forEach({
            var signatureType = if (it.signatureExtension == null || it.signatureExtension is InSignatureExtension) Type(Signature(it.name.name)) else Type(te.lookupType((it.signatureExtension as ExtendsSignatureExtension).name).currentType)
            if (signatureType.currentType is Set) {
                signatureType = (signatureType.currentType as Set).subType
            }
            if (signatureType.currentType is Scalar) {
                signatureType = (signatureType.currentType as Scalar).subType
            }
            if (it.qualifiers.contains(Operator.ONE)) {
                it.name.type.setType(Type(Scalar(signatureType)))
                te.addType(it.name.name, Type(Scalar(signatureType)))
            } else {
                it.name.type.setType(Type(Set(signatureType)))
                te.addType(it.name.name, Set(signatureType))
            }
            it.decls.forEach({ d -> d.names.forEach({ name -> te.addType(name.name, Relation(Type(Untyped()), Type(Untyped()))) }) })
        })
    }


    private fun typeCheck(te: TypeEnvironment, stmt: Statement) =
            when (stmt) {
                is CheckStatement -> typeCheck(te, stmt)
                is RunStatement -> typeCheck(te, stmt)
                is AssertionStatement -> typeCheck(te, stmt)
                is SignatureDeclarations -> typeCheck(te, stmt)
                is FactDeclaration -> typeCheck(te, stmt)
                is FunDeclaration -> typeCheck(te, stmt)
                is PredDeclaration -> typeCheck(te, stmt)
                is EnumDeclaration -> typeCheck(te, stmt)
                is OpenStatement -> {
                } // nothing to typecheck here
                else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
            }

    @Suppress("UNUSED_PARAMETER")
    private fun typeCheck(teIn: TypeEnvironment, stmt: EnumDeclaration) {
        //
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: CheckStatement) {
        stmt.expressions.forEach { expr -> typeCheckExpr(teIn, expr) }
    }

    private fun typeCheck(teIn: TypeEnvironment, stmt: RunStatement) {
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
                    val type = Relation(te.lookupType(stmt.name), decl.expression.expression.type)
                    idExpr.type.setType(Type(type))
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
        if (stmt.expressions.isNotEmpty()) {
            te.addType(stmt.name, stmt.expressions.last().type)
        }
    }

    private fun typeCheck(te: TypeEnvironment, stmt: PredDeclaration) {
        checkDeclsAndExpressions(te, stmt.decls, stmt.expressions)
        if (stmt.expressions.isNotEmpty()) {
            te.addType(stmt.name, stmt.expressions.last().type)
        }
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
            expr.type.setType(Type(Scalar(expr.expression.type)))
        } else {
            expr.type.setType(expr.expression.type)
        }
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IntegerCastExpression) {
        typeCheckExpr(teIn, expr.expr)
        expr.type.setType(Type(Integer()))
    }

    private fun typeCheckExpr(@Suppress("UNUSED_PARAMETER") teIn: TypeEnvironment, expr: IntegerSetExpression) {
        expr.type.setType(Type(Set(Type(Integer()))))
    }

    private fun typeCheckExpr(@Suppress("UNUSED_PARAMETER") teIn: TypeEnvironment, expr: IntegerExpression) {
        expr.type.setType(Type(Integer()))
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfExpression) {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
        expr.type.setType(expr.thenExpr.type)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: IfElseExpression) {
        typeCheckExpr(teIn, expr.ifExpr)
        typeCheckExpr(teIn, expr.thenExpr)
        typeCheckExpr(teIn, expr.elseExpr)
        expr.type.setType(expr.thenExpr.type)
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: BlockExpression) {
        expr.expressions.forEach { subExpr -> typeCheckExpr(teIn, subExpr) }
        expr.type.setType(Type(Predicate()))
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: QuantifierExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
        expr.type.setType(Type(Predicate()))
    }

    private fun typeCheckExpr(teIn: TypeEnvironment, expr: DeclListExpression) {
        checkDeclsAndExpressions(teIn, expr.decls, expr.expressions)
        expr.type.setType(expr.expressions.last().type)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: IdentifierExpression) {
        val type = te.lookupType(expr)
        expr.type.setType(type)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BinaryOperatorExpression) {
        typeCheckExpr(te, expr.left)
        typeCheckExpr(te, expr.right)
        if (expr.operator == JOIN) {
            typeCheckJoinExpr(te, expr)
            return
        }
        if (expr.right is IdentityExpression) run {
            expr.right.type.setType(expr.left.type)
        }
        val exprRightType = expr.right.type.currentType
        if (expr.left is UnivExpression && exprRightType is Relation) {
            expr.type.setType(Type(Set(exprRightType.leftType)))
            return
        }
        val exprLeftType = expr.left.type.currentType
        if (expr.right is UnivExpression && exprLeftType is Relation) {
            expr.type.setType(Type(Set(exprLeftType.rightType)))
            return
        }
        if (expr.left.type.currentType is Relation && expr.right.type.currentType is Relation) {
            expr.type.setType(expr.left.type)
            return
        }
        expr.type.setType(Type(Set(expr.left.type)))
    }

    @Suppress("UNUSED_PARAMETER")
    private fun typeCheckJoinExpr(te: TypeEnvironment, je: BinaryOperatorExpression) {
        val jeRightType = je.right.type.currentType
        val jeLeftType = je.left.type.currentType
        if (je.left is UnivExpression && jeRightType is Relation) {
            je.type.setType(jeRightType.rightType)
        } else if (je.right is UnivExpression && jeLeftType is Relation) {
            je.type.setType(jeLeftType.leftType)
        } else if (jeLeftType is Relation && jeRightType is Relation) {
            je.left.type.setType(Type(Relation(jeRightType.leftType, Type(Untyped()))))
            je.right.type.setType(Type(Relation(Type(Untyped()), jeLeftType.rightType)))
            je.type.setType(Type(Relation(jeLeftType.leftType, jeRightType.rightType)))
        } else if (jeLeftType is Relation && (jeRightType is Set || jeRightType is Scalar)) {
            je.type.setType(Type(Set(jeLeftType.leftType)))
        } else if (jeLeftType is Set && jeRightType is Relation) {
            //je.left.type.setType(Set(jeRightType.leftType))
            je.type.setType(Type(Set(jeRightType.rightType)))
        } else if (jeLeftType is Scalar && jeRightType is Untyped) {
            // next, nexts, prev, prevs
            je.right.type.setType(je.left.type)
            je.type.setType(Type(jeLeftType))
        } else if (jeRightType is Scalar && jeLeftType is Untyped) {
            // first, last
            je.left.type.setType(je.right.type)
            je.type.setType(Type(jeRightType))
        } else if (jeRightType is Relation && jeLeftType is Untyped) {
            // first, last for signature fields
            val rightType = je.right.type.currentType as Relation
            val setType = rightType.leftType.currentType as Set
            je.left.type.setType(Type(Scalar(setType.subType)))
            je.type.setType(Type(jeRightType))
        } else if (jeLeftType is Scalar && jeRightType is Relation) {
            //je.left.type.setType(Scalar(jeRightType.leftType))
            je.type.setType(Type(Set(jeRightType.rightType)))
        } else {
            throw UnsupportedOperationException("join typechecking failed: ${je.left} . ${je.right}")
        }
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: UnaryOperatorExpression) {
        typeCheckExpr(te, expr.expression)
        expr.type.setType(expr.expression.type)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: LetExpression) {
        expr.letDecls.forEach { decl ->
            typeCheckExpr(te, decl.expression); te.addType(decl.name.name, decl.expression.type)
        }
        expr.expressions.forEach { typeCheckExpr(te, it) }
        expr.type.setType(expr.expressions.last().type)
    }

    private fun typeCheckExpr(te: TypeEnvironment, expr: BoxJoinExpression) {
        typeCheckExpr(te, expr.left)
        expr.parameters.map { typeCheckExpr(te, it) }
        val leftType = expr.left.type.currentType
        if (leftType is Relation && expr.parameters.any { it.type.currentType is Relation }) {
            expr.type.setType(Type(Relation(expr.left.type, expr.left.type)))
            return
        }
        if (leftType is Untyped && expr.parameters.size > 0) {
            val orderedParam = expr.parameters.get(0)
            // next, nexts, prev, prevs can also be used by a box join like next[s]
            expr.left.type.setType(Type(getSigTypeFromSet(orderedParam.type)))
        }
        expr.type.setType(Type(Set(expr.left.type)))
    }

    private fun getSigTypeFromSet(type: Type): Scalar {
        val currentType = type.currentType
        if (currentType is Scalar) {
            return currentType
        }
        if (currentType is Set) {
            return getSigTypeFromSet(currentType.subType)
        }
        throw OperationNotSupportedException("Only nested sets allowed.")
    }

    private fun checkDeclsAndExpressions(te: TypeEnvironment, decls: List<Decl>, expressions: List<Expression>) {
        decls.forEach { decl -> decl.names.forEach { name -> typeCheckExpr(te, decl.expression); name.type.setType(decl.expression.type); te.addLocalType(name.name, decl.expression.type) } }
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