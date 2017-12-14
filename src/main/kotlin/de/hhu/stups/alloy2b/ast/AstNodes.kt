package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.typechecker.Type
import de.hhu.stups.alloy2b.typechecker.Untyped

interface Node {
    val position: Position?
}

data class Point(val line: Int, val column: Int)

data class Position(val start: Point, val end: Point)

data class AlloySpecification(val declarations: List<Statement>,
                              override val position: Position? = null) : Node

interface Statement : Node
interface Expression : Node {
    var type: Type
}


// decls
data class Decl(val names: List<IdentifierExpression>, val expression: QuantifiedExpression,
                override val position: Position? = null) : Node

data class LetDecl(val name: IdentifierExpression, val expression: Expression,
                   override val position: Position? = null) : Node

// signature extensions
interface SignatureExtension : Node

data class ExtendsSignatureExtension(val name: IdentifierExpression,
                                     override val position: Position? = null) : SignatureExtension
data class InSignatureExtension(val names: List<IdentifierExpression>,
                                override val position: Position? = null) : SignatureExtension

// expressions
data class UnaryOperatorExpression(val operator: Operator,
                                   val expression: Expression,
                                   override val position: Position? = null,
                                   override var type: Type = Type(Untyped())) : Expression

data class BinaryOperatorExpression(val operator: Operator,
                                    val left: Expression,
                                    val right: Expression,
                                    override val position: Position? = null,
                                    override var type: Type = Type(Untyped())) : Expression

data class BoxJoinExpression(val left: Expression,
                             val parameters: List<Expression>,
                             override val position: Position? = null,
                             override var type: Type = Type(Untyped())) : Expression

data class IdentifierExpression(val name: String,
                                override val position: Position? = null,
                                override var type: Type = Type(Untyped())) : Expression {
    override fun equals(other: Any?): Boolean {
        if (other is IdentifierExpression) {
            return name.equals(other.name)
        }
        return false
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}

data class LetExpression(val letDecls: List<LetDecl>,
                         val expressions: List<Expression>,
                         override val position: Position? = null,
                         override var type: Type = Type(Untyped())) : Expression

data class QuantifiedExpression(val operator: Operator,
                                val expression: Expression,
                                override val position: Position? = null,
                                override var type: Type = Type(Untyped())) : Expression

data class QuantifierExpression(val operator: Operator,
                                val decls: List<Decl>,
                                val expressions: List<Expression>,
                                override val position: Position? = null,
                                override var type: Type = Type(Untyped())) : Expression

data class BlockExpression(val expressions: List<Expression>,
                           override val position: Position? = null,
                           override var type: Type = Type(Untyped())) : Expression

data class IfExpression(val ifExpr: Expression, val thenExpr: Expression,
                        override val position: Position? = null,
                        override var type: Type = Type(Untyped())) : Expression

data class IfElseExpression(val ifExpr: Expression, val thenExpr: Expression, val elseExpr: Expression,
                            override val position: Position? = null,
                            override var type: Type = Type(Untyped())) : Expression

data class DeclListExpression(val decls: List<Decl>,
                              val expressions: List<Expression>,
                              override val position: Position? = null,
                              override var type: Type = Type(Untyped())) : Expression

data class IntegerSetExpression(override val position: Position? = null,
                                override var type: Type = Type(Untyped())) : Expression

data class IntegerCastExpression(val expr: Expression,
                                 override val position: Position? = null,
                                 override var type: Type = Type(Untyped())) : Expression

data class IntegerExpression(val int: Long,
                             override val position: Position? = null,
                             override var type: Type = Type(Untyped())) : Expression

data class IdentityExpression(override val position: Position? = null,
                              override var type: Type = Type(Untyped())) : Expression

data class UnivExpression(override val position: Position? = null,
                          override var type: Type = Type(Untyped())) : Expression

// statements
data class OpenStatement(val modules: List<IdentifierExpression>,
                         val refs: List<IdentifierExpression>,
                         override val position: Position? = null) : Statement

data class EnumDeclaration(val name: String,
                           val elements: List<String>,
                           override val position: Position? = null) : Statement

data class FactDeclaration(val name: String,
                           val expressions: List<Expression>,
                           override val position: Position? = null) : Statement

data class FunDeclaration(val name: String,
                          val decls: List<Decl> = emptyList(),
                          val expressions: List<Expression>,
                          override val position: Position? = null) : Statement

data class PredDeclaration(val name: String,
                           val decls: List<Decl> = emptyList(),
                           val expressions: List<Expression>,
                           override val position: Position? = null) : Statement

data class SignatureDeclarations(val signatures: List<SignatureDeclaration>,
                                 override val position: Position? = null) : Statement

data class AssertionStatement(val name: String,
                              val expressions: List<Expression>,
                              override val position: Position? = null) : Statement


data class CheckStatement(val name: String, val expressions: List<Expression>,
                          override val position: Position? = null) : Statement

// signature declarations
data class SignatureDeclaration(val qualifiers: List<Operator> = emptyList(),
                                val name: IdentifierExpression,
                                val signatureExtension: SignatureExtension? = null,
                                val decls: List<Decl> = emptyList(),
                                val expression: Expression? = null,
                                override val position: Position? = null) : Node