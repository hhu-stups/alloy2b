package de.hhu.stups.alloy2b.ast

interface Node {
    val position: Position?
}

data class Point(val line: Int, val column: Int)

data class Position(val start: Point, val end: Point)

fun pos(startLine:Int, startCol:Int, endLine:Int, endCol:Int) = Position(Point(startLine,startCol),Point(endLine,endCol))

data class AlloySpecification(val declarations : List<Statement>, override val position: Position? = null) : Node

interface Statement : Node { }
interface Expression : Node { }


// decls
data class Decl(val name: String, val expression: Expression, override val position: Position? = null) : Node

// expressions
data class UnaryOperatorExpression(val operator: Operator, val expression: Expression, override val position: Position? = null) : Expression
data class IdentifierExpression(val name: String, override val position: Position? = null) : Expression

// statements
data class FactDeclaration(val name: String, val expressions: List<Expression>, override val position: Position? = null) : Statement
data class SignatureDeclaration(val name: String, val decls: List<Decl> = emptyList(), val expressions: List<Expression> = emptyList(), override val position: Position? = null) : Statement

