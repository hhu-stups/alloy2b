package de.hhu.stups.alloy2b.ast

import de.hhu.stups.alloy2b.typechecker.Type
import de.hhu.stups.alloy2b.typechecker.Type.UNTYPED

interface Node {
    val position: Position?
}

data class Point(val line: Int, val column: Int)

data class Position(val start: Point, val end: Point)

data class AlloySpecification(val declarations: List<Statement>,
                              override val position: Position? = null) : Node

interface Statement : Node
interface Expression : Node {
    var type: Type?
}


// decls
data class Decl(val names: List<String>, val expression: QuantifiedExpression,
                override val position: Position? = null) : Node

data class LetDecl(val name: String, val expression: Expression,
                   override val position: Position? = null) : Node

// signature extensions
interface SignatureExtension : Node

data class NameSignatureExtension(val name: String,
                                  override val position: Position? = null) : SignatureExtension

// expressions
data class UnaryOperatorExpression(val operator: Operator,
                                   val expression: Expression,
                                   override val position: Position? = null,
                                   override var type: Type? = UNTYPED) : Expression

data class BinaryOperatorExpression(val operator: Operator,
                                    val left: Expression,
                                    val right: Expression,
                                    override val position: Position? = null,
                                    override var type: Type? = UNTYPED) : Expression

data class BoxJoinExpression(val left: Expression,
                             val parameters: List<Expression>,
                             override val position: Position? = null,
                             override var type: Type? = UNTYPED) : Expression

data class IdentifierExpression(val name: String,
                                override val position: Position? = null,
                                override var type: Type? = UNTYPED) : Expression

data class LetExpression(val letDecls: List<LetDecl>,
                         val expressions: List<Expression>,
                         override val position: Position? = null,
                         override var type: Type? = UNTYPED) : Expression

data class QuantifiedExpression(val operator: Operator,
                                val expression: Expression,
                                override val position: Position? = null,
                                override var type: Type? = UNTYPED) : Expression

data class QuantifierExpression(val operator: Operator,
                                val decls: List<Decl>,
                                val expressions: List<Expression>,
                                override val position: Position? = null,
                                override var type: Type? = UNTYPED) : Expression

data class BlockExpression(val expressions: List<Expression>,
                           override val position: Position? = null,
                           override var type: Type? = UNTYPED) : Expression

data class IfExpression(val ifExpr: Expression, val thenExpr: Expression, val elseExpr: Expression,
                        override val position: Position? = null,
                        override var type: Type? = UNTYPED) : Expression

data class DeclListExpression(val decls: List<Decl>,
                              val expressions: List<Expression>,
                              override val position: Position? = null,
                              override var type: Type? = UNTYPED) : Expression

data class IntegerSetExpression(override val position: Position? = null,
                                override var type: Type? = UNTYPED) : Expression

data class IntegerCastExpression(val expr: Expression,
                                 override val position: Position? = null,
                                 override var type: Type? = UNTYPED) : Expression

data class IntegerExpression(val int: Long,
                             override val position: Position? = null,
                             override var type: Type? = UNTYPED) : Expression

// statements
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

data class SignatureDeclaration(val qualifiers: List<Operator> = emptyList(),
                                val names: List<String>, // a list of strings for definitions like 'sig File, Dir [...]'
                                val signatureExtension: SignatureExtension? = null,
                                val decls: List<Decl> = emptyList(),
                                val expressions: List<Expression> = emptyList(),
                                override val position: Position? = null) : Statement

data class AssertionStatement(val name: String,
                              val expressions: List<Expression>,
                              override val position: Position? = null) : Statement


data class CheckStatement(val name: String, val expressions: List<Expression>,
                          override val position: Position? = null) : Statement
