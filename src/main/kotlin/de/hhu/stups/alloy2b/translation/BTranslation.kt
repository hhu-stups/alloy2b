package de.hhu.stups.alloy2b.translation

import de.hhu.stups.alloy2b.ast.*
import de.hhu.stups.alloy2b.ast.Operator.*
import de.hhu.stups.alloy2b.typechecker.*
import de.hhu.stups.alloy2b.typechecker.Set

class BTranslation(spec: AlloySpecification) {
    private val sets = mutableListOf<String>()
    private val constants = mutableListOf<String>()
    private val definitions = mutableListOf<String>()
    private val properties = mutableListOf<String>()
    private val assertions = mutableListOf<String>()
    private val operations = mutableListOf<String>()

    private var runCounter = 1
    private var checkCounter = 1

    private val signatures = mutableListOf<IdentifierExpression>()
    private val signatureDeclarations = mutableMapOf<IdentifierExpression, SignatureDeclaration>()
    private val abstractSignatures = mutableListOf<IdentifierExpression>()
    private val extendingSignatures = mutableMapOf<IdentifierExpression, List<IdentifierExpression>>()
    private val parentSignature = mutableMapOf<IdentifierExpression, IdentifierExpression>()

    private val fields = mutableListOf<String>()

    private val alloyAssertions = mutableMapOf<String, String>()

    init {
        TypeChecker(spec)
        spec.declarations.forEach({ stmt -> translate(stmt) })
        addSignatureExtensionProperties()
        addSignatureFieldsExtensionProperties()
    }

    fun getTranslation(): String {
        val builder = StringBuilder()

        builder.appendln("/*@ generated */")
        builder.appendln("MACHINE alloytranslation")

        appendIfNotEmpty(builder, sets, "; ", "SETS")
        appendIfNotEmpty(builder, constants, ", ", "CONSTANTS")
        appendIfNotEmpty(builder, definitions, " ;\n    ", "DEFINITIONS")
        appendIfNotEmpty(builder, properties, " &\n    ", "PROPERTIES")
        appendIfNotEmpty(builder, assertions, " &\n    ", "ASSERTIONS")
        appendIfNotEmpty(builder, operations, ";\n    ", "OPERATIONS")

        builder.appendln("END")

        return builder.toString()
    }

    private fun appendIfNotEmpty(builder: StringBuilder, list: MutableList<String>, delimiter: String, sectionName: String) {
        if (list.isEmpty()) {
            return
        }
        builder.appendln(sectionName)
        builder.appendln("    " + list.joinToString(delimiter))
    }

    private fun addSignatureExtensionProperties() {
        // two signatures extending the same base signature are distinct
        extendingSignatures.values.forEach({ signatures ->
            for (i1 in 0 until signatures.size) {
                for (i2 in i1 until signatures.size) {
                    addSignatureExtensionIfNotEqual(signatures[i1], signatures[i2])
                }
            }
        })
        // abstract signatures are exhaustively divided into their sub signatures
        abstractSignatures.forEach({ absSigName ->
            properties.add("${extendingSignatures[absSigName].orEmpty().joinToString(" \\/ ") { sanitizeIdentifier(it) }} = ${sanitizeIdentifier(absSigName)}")
        })
    }

    private fun addSignatureExtensionIfNotEqual(sig1: IdentifierExpression, sig2: IdentifierExpression) {
        if (sig1.name == sig2.name) {
            return
        }
        if (sig1.type.currentType is Scalar && sig2.type.currentType is Scalar) {
            // inequality if both are singletons
            properties.add("${sanitizeIdentifier(sig1.name)} /= ${sanitizeIdentifier(sig2.name)}")
            return
        }
        properties.add("${sanitizeIdentifier(sig1)} /\\ ${sanitizeIdentifier(sig2)} = {}")
    }

    private fun addSignatureFieldsExtensionProperties() {
        signatureDeclarations.values.forEach({
            if (it.expression != null) {
                val fieldNames = getFieldNamesOfSignatureHierarchy(it)
                val nexpr = replaceFieldIdentifiers(it.name.name, fieldNames, it.expression)
                properties.add("/* from signature declaration */ ${translateExpression(nexpr)}")
            }
        })

        // field declarations are mapped to constants and properties
        signatureDeclarations.values.forEach({
            val fieldNames = getFieldNamesOfSignatureHierarchy(it)
            it.decls.forEach({ d ->
                d.names.map { constants.add(sanitizeIdentifier(it)) }
                translateFieldDeclarations(d, it.name.name, fieldNames)
            })
        })
    }

    private fun translateFieldDeclarations(decl: Decl, name: String, fieldNames: List<IdentifierExpression>) {
        decl.names.map {
            fields.add(it.name) // used to decide how to translate dot join
            val nexpr = replaceFieldIdentifiers(name,fieldNames,decl.expression.expression)

            val symbol: String =
            when (decl.expression.operator) {
                LONE -> "+->" // one-to-one mapping, i.e. function, LONE = 0 or 1 target
                ONE -> "-->" // one-to-one mapping, i.e. function, ONE = exactly 1 target
                else -> "<->"
            }

            if(nexpr == decl.expression.expression){
                // no this was introduced, avoid quantifier
                properties.add("${sanitizeIdentifier(it)} : ${sanitizeIdentifier(name)} $symbol ${translateExpression(nexpr)}")
            } else {
                // this_ needed, add quantifier
                properties.add("!(this_).({this_} <: ${sanitizeIdentifier(name)} => ${sanitizeIdentifier(it)} : ${sanitizeIdentifier(name)} $symbol ${translateExpression(nexpr)})")
            }

        }
    }

    private fun getFieldNamesOfSignatureHierarchy(sig: SignatureDeclaration): List<IdentifierExpression> {
        val fields = sig.decls.map { it.names }.flatten()
        if (parentSignature.containsKey(sig.name)) {
            val parentName = parentSignature[sig.name]!! // should exist!
            val parentSignature = signatureDeclarations[parentName]!!
            return fields + getFieldNamesOfSignatureHierarchy(parentSignature)
        }
        return fields
    }

    private fun replaceFieldIdentifiers(signatureName: String, fieldNames: List<IdentifierExpression>, expr: Expression): Expression = when (expr) {
        is IdentifierExpression -> if (!expr.name.startsWith("@") && fieldNames.contains(expr)) castExpressionIfInteger(BinaryOperatorExpression(JOIN, IdentifierExpression("this", expr.position, Type(Scalar(Type(Signature(signatureName))))), expr, expr.position, expr.type)) else expr
        is IntegerExpression -> expr
        is QuantifiedExpression -> QuantifiedExpression(expr.operator, replaceFieldIdentifiers(signatureName, fieldNames, expr.expression), expr.position, expr.type)
        is QuantifierExpression -> QuantifierExpression(expr.operator, expr.decls, expr.expressions.map { replaceFieldIdentifiers(signatureName, fieldNames, it) }, expr.position, expr.type)
        is BinaryOperatorExpression -> BinaryOperatorExpression(expr.operator, replaceFieldIdentifiers(signatureName, fieldNames, expr.left), replaceFieldIdentifiers(signatureName, fieldNames, expr.right), expr.position, expr.type)
        is UnaryOperatorExpression -> UnaryOperatorExpression(expr.operator, replaceFieldIdentifiers(signatureName, fieldNames, expr.expression), expr.position, expr.type)
        is BoxJoinExpression -> BoxJoinExpression(replaceFieldIdentifiers(signatureName, fieldNames, expr.left), expr.parameters.map { replaceFieldIdentifiers(signatureName, fieldNames, it) }, expr.position, expr.type)
        is IfExpression -> IfExpression(replaceFieldIdentifiers(signatureName, fieldNames, expr.ifExpr), replaceFieldIdentifiers(signatureName, fieldNames, expr.thenExpr), expr.position, expr.type)
        is IfElseExpression -> IfElseExpression(replaceFieldIdentifiers(signatureName, fieldNames, expr.ifExpr), replaceFieldIdentifiers(signatureName, fieldNames, expr.thenExpr), replaceFieldIdentifiers(signatureName, fieldNames, expr.elseExpr), expr.position, expr.type)
        is IntegerSetExpression -> expr
        else -> throw UnsupportedOperationException("Missing case in replaceFieldIdentifiers: $expr")
    }

    /**
     * Variables that are defined within a signature are joined with 'this' in the signature's facts. If the variable is
     * an integer we additionally cast the expression.
     */
    private fun castExpressionIfInteger(expr: BinaryOperatorExpression): Expression {
        val type = expr.right.type.currentType
        if (type is Relation) {
            val relRightType = type.rightType.currentType
            if (relRightType is Set && relRightType.subType.currentType is Integer) {
                return IntegerCastExpression(expr)
            }
        }
        return expr
    }

    /**
     * Translate {@link Statement statements}.
     */
    private fun translate(stmt: Statement) {
        when (stmt) {
            is CheckStatement -> translate(stmt)
            is RunStatement -> translate(stmt)
            is AssertionStatement -> translate(stmt)
            is SignatureDeclarations -> translate(stmt)
            is FactDeclaration -> translate(stmt)
            is FunDeclaration -> translate(stmt)
            is PredDeclaration -> translate(stmt)
            is EnumDeclaration -> translate(stmt)
            is OpenStatement -> translate(stmt)
            else -> throw UnsupportedOperationException(stmt.javaClass.canonicalName)
        }
    }

    private fun translate(stmt: OpenStatement) {
        stmt.modules.forEach({
            if (it.name.equals("util/ordering")) {
                stmt.refs.forEach({
                    constants.add("ordering_${sanitizeIdentifier(it)}")
                    properties.add("ordering_${sanitizeIdentifier(it)} : SEQ(${sanitizeIdentifier(it)})")
                })
            } else if (it.name.equals("util/integer")) {
              // implemented by default
            } else {
                throw UnsupportedOperationException("unknown module ${it.name}")
            }
        })
    }

    private fun translate(stmt: EnumDeclaration) {
        sets.add("${sanitizeIdentifier(stmt.name)} = {${stmt.elements.joinToString(", ") { sanitizeIdentifier(it) }}}")
    }

    private fun translate(stmt: AssertionStatement) {
        alloyAssertions[stmt.name] = "/* ${stmt.name} */ " + stmt.expressions.joinToString(" & ") { e -> translateExpression(e) }
    }

    private fun translate(fdec: FactDeclaration) {
        properties.add(fdec.expressions.joinToString(" & ") { e -> translateExpression(e) })
    }

    private fun translate(fdec: FunDeclaration) {
        val builder = StringBuilder()
        if (fdec.decls.isEmpty()) {
            builder.append(sanitizeIdentifier(fdec.name))
        } else {
            builder.append("${sanitizeIdentifier(fdec.name)}(${fdec.decls.joinToString(", ") { it.names.joinToString(", ") { name -> sanitizeIdentifier(name.name) } }}) == ")
        }
        val decls = translateDeclsExprList(fdec.decls)
        val parameterExpressions = fdec.expressions.map { translateExpression(it) }.joinToString(" & ") { "temp : $it" }
        // represent functions as set comprehensions
        builder.append("{ temp | $decls & $parameterExpressions}")
        definitions.add(builder.toString())
    }

    private fun translate(pdec: PredDeclaration) {
        val builder = StringBuilder()
        val predCall: String
        val alloyAssertion: String
        if (pdec.decls.isEmpty()) {
            predCall = sanitizeIdentifier(pdec.name)
            alloyAssertion = predCall
        } else {
            val params = pdec.decls.joinToString(", ") { it.names.joinToString(",") { sanitizeIdentifier(it.name) } }
            predCall = "${sanitizeIdentifier(pdec.name)}($params)"
            alloyAssertion = "#($params).($predCall)"
        }
        val decls = translateDeclsExprList(pdec.decls)
        builder.append("$predCall == $decls")
        val blocks = pdec.expressions.joinToString(" & ") { translateExpression(it) }
        if (blocks.isNotEmpty()) {
            builder.append(" ${if (decls.isEmpty()) "" else " & "} $blocks")
        } else {
            builder.append(" ${if (decls.isEmpty()) "" else " & "} 1=1")
        }
        definitions.add(builder.toString())
        // add existantially quantified predicate to be used for CheckStatement
        alloyAssertions[pdec.name] = alloyAssertion
    }

    private fun translate(stmt: CheckStatement) {
        stmt.expressions.forEach({ e ->
            when (e) {
                is IdentifierExpression -> if (alloyAssertions.containsKey(e.name)) operations.add("check_${e.name} = PRE not(${alloyAssertions[e.name].orEmpty()}) THEN skip END")
                else -> operations.add("check_${++checkCounter} = PRE not(${translateExpression(e)}) THEN skip END")
            }
        })
    }

    private fun translate(stmt: RunStatement) {
        stmt.expressions.forEach({ e ->
            when (e) {
                is IdentifierExpression -> if (alloyAssertions.containsKey(e.name)) operations.add("run_${e.name} = PRE ${alloyAssertions[e.name].orEmpty()} THEN skip END")
                else -> operations.add("run_${++runCounter} = PRE ${translateExpression(e)} THEN skip END")
            }
        })
    }

    private fun translate(sdec: SignatureDeclarations) {
        sdec.signatures.forEach { translate(it) }
    }

    private fun translate(sdec: SignatureDeclaration) {
        signatures.add(sdec.name) // used to decide how to translate dot join
        signatureDeclarations[sdec.name] = sdec

        if (sdec.qualifiers.contains(ABSTRACT)) {
            abstractSignatures.add(sdec.name)
        }

        handleQuantifiersByCardinality(sdec)

        if (sdec.signatureExtension == null) {
            // basic signature -> B set
            sets.add(sanitizeIdentifier(sdec.name))
            return
        }

        // not a basic signature -> ensure subset relation with extended / extending signatures
        constants.add(sanitizeIdentifier(sdec.name.name)) // use sdec.name.name instead of sdec.name to avoid adding { .. }
        when (sdec.signatureExtension) {
            is ExtendsSignatureExtension -> {
                properties.add("${sanitizeIdentifier(sdec.name)} <: ${sanitizeIdentifier(sdec.signatureExtension.name)}")
                extendingSignatures[sdec.signatureExtension.name] =
                        extendingSignatures.getOrDefault(sdec.signatureExtension.name, emptyList()) + sdec.name
                parentSignature[sdec.name] = sdec.signatureExtension.name
            }
            is InSignatureExtension -> {
                sdec.signatureExtension.names.forEach({ extensionName ->
                    properties.add("${sanitizeIdentifier(extensionName)} <: ${sanitizeIdentifier(sdec.name)}")
                    parentSignature[extensionName] = sdec.name
                })

            }
            else -> throw UnsupportedOperationException(sdec.signatureExtension.javaClass.canonicalName)
        }
    }

    private fun handleQuantifiersByCardinality(sdec: SignatureDeclaration) {
        // case ONE is handled by introducing a constant for the singleton element
        // thus, we do not need a cardinality constraint
        if (NO in sdec.qualifiers) {
            properties.add("card(${sanitizeIdentifier(sdec.name.name)}) = 0")
        }
        if (LONE in sdec.qualifiers) {
            properties.add("card(${sanitizeIdentifier(sdec.name.name)}) <= 1")
        }
        if (SOME in sdec.qualifiers) {
            properties.add("card(${sanitizeIdentifier(sdec.name.name)}) >= 1")
        }
    }

    private fun translateExpression(e: Expression): String {
        if (e.type.untyped()) {
            throw UnsupportedOperationException("untyped expression: $e")
        }
        return when (e) {
            is QuantifierExpression -> translateExpression(e)
            is IdentifierExpression -> translateExpression(e)
            is BinaryOperatorExpression -> translateExpression(e)
            is UnaryOperatorExpression -> translateExpression(e)
            is LetExpression -> translateExpression(e)
            is BoxJoinExpression -> translateExpression(e)
            is IntegerSetExpression -> "INTEGER"
            is IntegerCastExpression -> translateExpression(e)
            is IntegerExpression -> translateExpression(e)
            is QuantifiedExpression -> translateExpression(e)
            is IdentityExpression -> translateExpression(e)
            is UnivExpression -> translateExpression(e)
            is IfExpression -> translateExpression(e)
            is BlockExpression -> translateExpression(e)
            is IfElseExpression -> translateExpression(e)
            else -> throw UnsupportedOperationException(e.javaClass.canonicalName)
        }
    }

    private fun translateExpression(ue: UnivExpression): String {
        // TODO
        return ""
    }

    private fun translateExpression(ie: IdentifierExpression): String {
        // special cases for identifiers used in ordering
        if("first".equals(ie.name)) {
            val subtype = ie.type.currentType as Scalar
            return "first(ordering_${subtype.subType}_)"
        }
        if("last".equals(ie.name)) {
            val subtype = ie.type.currentType as Scalar
            return "first(ordering_${subtype.subType}_)"
        }
        return sanitizeIdentifier(ie)
    }

    private fun translateExpression(ite: IfElseExpression): String {
        return "(${translateExpression(ite.ifExpr)} => ${translateExpression(ite.thenExpr)}) & (not(${translateExpression(ite.ifExpr)}) => ${translateExpression(ite.elseExpr)})"
    }

    private fun translateExpression(be: BlockExpression): String {
        return be.expressions.joinToString(" & ") { translateExpression(it) }
    }

    private fun translateExpression(ie: IfExpression): String {
        return "${translateExpression(ie.ifExpr)} => ${translateExpression(ie.thenExpr)}"
    }

    private fun translateExpression(ie: IdentityExpression): String {
        if (ie.type.untyped()) {
            throw UnsupportedOperationException("Identity is untyped.")
        }
        val ieType = ie.type.currentType
        if (ieType is Relation) {
            return "id(${translateType(ieType.leftType)})"
        }
        throw UnsupportedOperationException("Identity is wrongly typed.")
    }

    private fun translateExpression(ie: IntegerExpression): String =
            ie.int.toString()

    private fun translateExpression(ice: IntegerCastExpression): String {
        val expr = ice.expr
        if (expr is BinaryOperatorExpression && expr.operator == JOIN) {
            val rightExprType = expr.right.type.currentType
            if (rightExprType is Relation) {
                // function call if the right type of the relation is Integer
                val rightRelationType = rightExprType.rightType.currentType
                if (rightRelationType is Set && rightRelationType.subType.currentType is Integer) {
                    // replace curly brackets for function call
                    return "${translateExpression(expr.right)}(${translateExpression(expr.left).replace("{", "").replace("}", "")})"
                }
            }
            throw UnsupportedOperationException("Integer cast of a join not supported this way.")
        }
        return translateExpression(ice.expr)
    }

    private fun translateExpression(qe: QuantifierExpression): String =
            when (qe.operator) {
                ALL -> "!(${translateDeclsIDList(qe.decls)}).(${translateDeclsExprList(qe.decls)} => ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }})"
                NO -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) = 0"
                ONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) = 1"
                LONE -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) <= 1"
                SOME -> "card({${translateDeclsIDList(qe.decls)} | ${translateDeclsExprList(qe.decls)} & ${qe.expressions.joinToString(" & ") { e -> translateExpression(e) }}}) > 0"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateExpression(le: LetExpression): String {
        val builder = StringBuilder()
        builder.append("LET ")
        builder.append(le.letDecls.joinToString(", ") { it -> sanitizeIdentifier(it.name) })
        builder.append(" BE ")
        builder.append(le.letDecls.joinToString(" & ") { it -> "${sanitizeIdentifier(it.name)} = ${translateExpression(it.expression)}" })
        builder.append(" IN ")
        builder.append(le.expressions.joinToString(" & ") { it -> translateExpression(it) })
        builder.append(" END")
        return builder.toString()
    }

    private fun translateExpression(bje: BoxJoinExpression): String {
        val parameters = bje.parameters.map { if (it is IdentifierExpression) sanitizeIdentifier(it.name) else translateExpression(it) }
        return "${translateExpression(bje.left)}(${parameters.joinToString(", ")})"
    }

    private fun translateExpression(qe: BinaryOperatorExpression): String {
        val symbol: String
        when (qe.operator) {
            JOIN -> return translateJoin(qe)
            OVERRIDE -> symbol = "<+"
            DOM_RESTR -> symbol = "<|"
            RAN_RESTR -> symbol = "|>"
            IN -> symbol = "<:"
            EQUAL -> symbol = "="
            INTERSECTION -> symbol = "/\\"
            PLUS -> symbol = if (qe.left.type.currentType is Integer || qe.right.type.currentType is Integer) "+" else "\\/"
            MINUS -> symbol = "-"
            GREATER -> symbol = ">"
            GREATER_EQUAL -> symbol = ">="
            LESS -> symbol = "<"
            LESS_EQUAL -> symbol = "<="
            AND -> symbol = "&"
            OR -> symbol = "or"
            IMPLICATION -> symbol = "=>"
            IFF -> symbol = "<=>"
            INVERSE -> symbol = "~"
            CARTESIAN -> symbol = "*"
            TOTAL_FUNCTION -> symbol = "-->"
            PARTIAL_FUNCTION -> symbol = "+->"
            BIJECTIVE_FUNCTION -> symbol = ">->>"
            else -> throw UnsupportedOperationException(qe.operator.name)
        }
        return "${translateExpression(qe.left)} $symbol ${translateExpression(qe.right)}"
    }

    private fun translateExpression(qe: UnaryOperatorExpression): String =
            when (qe.operator) {
                CLOSURE -> "closure(${translateExpression(qe.expression)})"
                CLOSURE1 -> "closure1(${translateExpression(qe.expression)})"
                CARD -> "card(${translateExpression(qe.expression)})"
                INVERSE -> "${translateExpression(qe.expression)}~"
                NOT -> "not(${translateExpression(qe.expression)})"
                SET -> translateExpression(qe.expression) // TODO: should this be POW?
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateExpression(qe: QuantifiedExpression): String =
            when (qe.operator) {
                NO -> "${translateExpression(qe.expression)} = {}"
                ONE -> "card(${translateExpression(qe.expression)}) = 1"
                LONE -> "card(${translateExpression(qe.expression)}) <= 1"
                SOME -> "card(${translateExpression(qe.expression)}) >= 1"
                else -> throw UnsupportedOperationException(qe.operator.name)
            }

    private fun translateJoin(je: BinaryOperatorExpression): String {
        val jeRightType = je.right.type.currentType
        val jeLeftType = je.left.type.currentType
        if (je.left is UnivExpression) {
            return "dom(${translateExpression(je.right)})"
        }
        if (je.right is UnivExpression) {
            return "ran(${translateExpression(je.left)})"
        }
        if (je.left.type.untyped() || je.right.type.untyped()) {
            throw UnsupportedOperationException("missing types in join translation: ${je.left} . ${je.right}")
        }
        if (jeLeftType is Relation && jeRightType is Relation) {
            return "(${translateExpression(je.left)} ; ${translateExpression(je.right)})"
        }
        if (jeLeftType is Relation && (jeRightType is Set || jeRightType is Scalar)) {
            return "${translateExpression(je.left)}~[${translateExpression(je.right)}]"
        }
        if ((jeLeftType is Set || jeLeftType is Scalar) && jeRightType is Relation) {
            return "${translateExpression(je.right)}[${translateExpression(je.left)}]"
        }
        throw UnsupportedOperationException("join not supported this way: ${je.left} . ${je.right}")
    }

    private fun translateDeclsIDList(decls: List<Decl>) =
            decls.joinToString(", ") { it.names.joinToString(", ") { sanitizeIdentifier(it.name) } } // sanitizing string instead of identifier expression avoids set expansion to {id}

    private fun translateDeclsExprList(decls: List<Decl>): String {
        return decls.joinToString(" & ") { it.names.joinToString(" & ") { n -> "${sanitizeIdentifier(n)} <: ${translateDeclExpression(it.expression)}" } }
    }

    private fun translateDeclExpression(expr: QuantifiedExpression): String =
            when (expr.operator) {
                ONE -> translateExpression(expr.expression)
                SET -> translateExpression(expr.expression)
                else -> throw UnsupportedOperationException(expr.operator.name)
            }

    /**
     * Concatenate underscore to all variables in order to prevent collision with B keywords and replace all ' with _.
     */
    private fun sanitizeIdentifier(id: String) =
            "${id.replace("'", "_")}_"

    private fun sanitizeIdentifier(id: IdentifierExpression): String =
            if (id.type.currentType is Scalar) {
                "{${id.name.replace("'", "_")}_}"
            } else {
                sanitizeIdentifier(id.name)
            }

    private fun translateType(type: Type): String {
        val eType = type.currentType
        return when (eType) {
            is Signature -> sanitizeIdentifier(eType.subType)
            is Relation -> "${translateType(eType.leftType)} <-> ${translateType(eType.rightType)}"
            is Set -> translateType(eType.subType)
            else -> throw UnsupportedOperationException("Cannot translate type to B set: $eType.")
        }
    }
}


