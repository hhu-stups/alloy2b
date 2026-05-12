package de.hhu.stups.alloy2b.translation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprHasName;
import edu.mit.csail.sdg.ast.ExprITE;
import edu.mit.csail.sdg.ast.ExprLet;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.ast.VisitReturn;

import static de.hhu.stups.alloy2b.translation.ParserUtil.sanitizeIdentifier;

public final class ExpressionToProlog extends VisitReturn<String> {
    private final List<Sig> signatures;
    private final List<String> orderedSignatures;
    private final Set<Set<String>> setsOfParents;

    /**
     * True if an Alloy model uses sequences.
     */
    private boolean usesSequences;

    /**
     * True if an Alloy model only accesses successors or predecessors of ordered signatures.
     */
    private boolean orderingsUseSuccessorsOnly;

    public ExpressionToProlog(List<Sig> signatures, List<String> orderedSignatures, Set<Set<String>> setsOfParents) {
        this.signatures = signatures;
        this.orderedSignatures = orderedSignatures;
        this.setsOfParents = setsOfParents;

        this.usesSequences = false;
        this.orderingsUseSuccessorsOnly = true;
    }

    public boolean getUsesSequences() {
        return this.usesSequences;
    }

    public void setUsesSequences(boolean usesSequences) {
        this.usesSequences = usesSequences;
    }

    public boolean getOrderingsUseSuccessorsOnly() {
        return this.orderingsUseSuccessorsOnly;
    }

    public void setOrderingsUseSuccessorsOnly(boolean orderingsUseSuccessorsOnly) {
        this.orderingsUseSuccessorsOnly = orderingsUseSuccessorsOnly;
    }

    /**
     * Translate a field declaration to a Prolog term.
     */
    public String toPrologTerm(Decl astNode) {
        String options = astNode.disjoint != null || astNode.disjoint2 != null ? "[disj]" : "[]";
        if (astNode.names.size() > 1) {
            // a field declaration may has several names, for instance:
            // "sig State { near, far: set Object }" has one Decl with one Expr but two names near and far
            // we then have to define Expr for each name
            return astNode.names.stream()
                .map(it ->
                    "field("
                    + "identifier(" + sanitizeIdentifier(it.label) + "," + this.getType(it.type()) + ",pos(" + it.pos.x + "," + it.pos.y + ")),"
                    + astNode.expr.accept(this) + ","
                    + this.getType(astNode.expr.type()) + ","
                    + options + ","
                    + "pos(" + astNode.get().pos.x + "," + astNode.get().pos.y + "))"
                )
                .collect(Collectors.joining(","));
        }
        ExprHasName astNodeE = astNode.get();
        return "field("
            + "identifier(" + sanitizeIdentifier(astNodeE.label) + "," + this.getType(astNodeE.type()) + ",pos(" + astNodeE.pos.x + "," + astNodeE.pos.y + ")),"
            + astNode.expr.accept(this) + ","
            + this.getType(astNode.expr.type()) + ","
            + options + ","
            + "pos(" + astNode.get().pos.x + "," + astNode.get().pos.y + "))";
    }

    @Override
    public String visit(ExprBinary p0) {
        Expr left = p0.left;
        String tLeft = left.accept(this);
        Expr right = p0.right;
        String tRight = right.accept(this);

        List<String> leftTypeClean = this.splitAndCleanType(left.type());
        List<String> rightTypeClean = this.splitAndCleanType(right.type());
        int arityl = leftTypeClean.size();
        int arityr = rightTypeClean.size();
        List<String> leftTypeGen;
        List<String> rightTypeGen;

        // special case for dot join and range restriction possibly reversing the order of type lists
        if (".".equals(p0.op.toString()) || ":>".equals(p0.op.toString())) {
            if (arityr == 1) {
                leftTypeGen = new ArrayList<>(leftTypeClean);
                Collections.reverse(leftTypeGen);
                rightTypeGen = rightTypeClean;
            } else {
                leftTypeGen = leftTypeClean;
                if (arityl < right.type().arity()) {
                    rightTypeGen = this.splitAndCleanType(right.type()).subList(0, left.type().arity());
                } else {
                    rightTypeGen = this.splitAndCleanType(right.type());
                }
                Collections.reverse(rightTypeGen);
            }
        } else {
            leftTypeGen = leftTypeClean;
            rightTypeGen = rightTypeClean;
        }

        Set<Pair<String, String>> typeDifferences = getTypeDifferences(leftTypeGen, rightTypeGen);
        this.addToSetsOfParentsPre(p0.op.toString(), typeDifferences, leftTypeGen, rightTypeGen);
        return getOperator(p0.op.toString()) + "("
            + tLeft + ","
            + tRight + ","
            + this.getType(p0.type()) + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    private void addToSetsOfParentsPre(
        String opString,
        Set<Pair<String, String>> typeDifferences,
        List<String> leftTypeGen,
        List<String> rightTypeGen
    ) {
        if (!opString.contains("->")) { // exclude type definitions like 'date: known -> Date'
            Set<Pair<String, String>> typeDifferencesNoOrds = new LinkedHashSet<>();
            // filter pairs of different types where at least one type is an ordered signature (ordered signatures
            // already have a parent type POW(INTEGER) as we translate ordered signatures as sets of integers in B)
            for (Pair<String, String> pair : typeDifferences) {
                // if an ordered signature interacts with an unordered one both have to be defined as a set of integer
                boolean c1 = this.orderedSignatures.contains(pair.a);
                boolean c2 = this.orderedSignatures.contains(pair.b);
                if (c1 && !c2) {
                    this.orderedSignatures.add(pair.b);
                } else if (!c1 && c2) {
                    this.orderedSignatures.add(pair.a);
                } else {
                    typeDifferencesNoOrds.add(pair);
                }
            }
            // we cannot introduce a parent type in B if one type is Int
            Set<Pair<String, String>> typeDifferencesNoOrdsNoInts = typeDifferencesNoOrds.stream()
                .filter(it -> !"'Int'".equals(it.a) && !"'Int'".equals(it.b))
                .collect(Collectors.toSet());
            // for the remaining pairs of conflicting types we have to introduce a parent type in the translated B model
            // (binary interaction between different types without a parent type in Alloy but univ)
            if (!leftTypeGen.toString().contains("univ") && !rightTypeGen.toString().contains("univ")
                    && !typeDifferencesNoOrdsNoInts.isEmpty()) {
                addToSetsOfParents(this.setsOfParents, typeDifferencesNoOrdsNoInts);
            }
        }
    }

    /**
     * Insert the set of pairs of types into the sets of parents.
     * For instance, insert [(T1,P3),(T3,P4)] in [[T1,P1,P2],[T2,P3]].
     * The set of sets is merged afterwards, e.g., resulting in [[T1,P1,P2,T2,P3],[T3,P4]].
     * Note: Assuming that strings are types generated by this#splitAndCleanType().
     */
    private static void addToSetsOfParents(
        Set<Set<String>> setsOfParents,
        Set<Pair<String, String>> typeDifferences
    ) {
        for (Pair<String, String> pair : typeDifferences) {
            boolean added = false;
            for (Set<String> set : setsOfParents) {
                if (set.contains(pair.a) || set.contains(pair.b)) {
                    set.add(pair.a);
                    set.add(pair.b);
                    added = true;
                }
            }
            if (!added) {
                setsOfParents.add(new LinkedHashSet<>(Arrays.asList(pair.a, pair.b)));
            }
        }

        // merge sets of types
        Set<Set<String>> newSetsOfParents = new LinkedHashSet<>();
        for (Set<String> set : setsOfParents) {
            boolean added = false;
            for (Set<String> newSet : newSetsOfParents) {
                if (newSet.stream().anyMatch(set::contains)) {
                    newSet.addAll(set);
                    added = true;
                }
            }
            if (!added) {
                newSetsOfParents.add(set);
            }
        }

        setsOfParents.clear();
        setsOfParents.addAll(newSetsOfParents);
    }

    /**
     * Given two lists of types like ['T1','T2'], ['T1','P2']. Search for different pairs of types at the same index
     * position. For instance, above result would be [('T2','P2')].
     * Note: Assuming that strings are types generated by this#splitAndCleanType().
     */
    private static Set<Pair<String, String>> getTypeDifferences(List<String> type1, List<String> type2) {
        int size1 = type1.size();
        int size2 = type2.size();
        List<String> ntype1;
        List<String> ntype2;
        // join has different arities
        if (size2 < size1) {
            ntype1 = type2;
            ntype2 = type1;
        } else {
            ntype1 = type1;
            ntype2 = type2;
        }
        Set<Pair<String, String>> differentTypeSets = new LinkedHashSet<>();
        for (int i = 0; i < ntype1.size(); i++) {
            String v1 = ntype1.get(i);
            String v2 = ntype2.get(i);
            // we have real booleans in B so this is not a type difference
            List<String> boolException = Arrays.asList("'boolean''True'", "'boolean''False'", "'boolean''Bool'");
            if (!v1.equals(v2) && !(boolException.contains(v1) && boolException.contains(v2))) {
                differentTypeSets.add(new Pair<>(v1, v2));
            }
        }
        return differentTypeSets;
    }

    @Override
    public String visit(ExprList p0) {
        return getOperator(p0.op.toString()) + "("
            + p0.args.stream().map(it -> it.accept(this)).collect(Collectors.toList()) + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprCall p0) {
        String functor = p0.fun.isPred ? "pred_call" : "fun_call";
        String name = sanitizeIdentifier(p0.fun.label);
        String[] orderedSplit = p0.toString().split("/");
        String suffix = orderedSplit[orderedSplit.length - 1];
        if (this.orderedSignatures.contains(orderedSplit[0]) && ("nexts".equals(suffix) || "prevs".equals(suffix))) {
            this.orderingsUseSuccessorsOnly = false;
        }
        String type = this.getType(p0.type());
        return functor + "("
            + name + ","
            + p0.args.stream().map(it -> it.accept(this)).collect(Collectors.toList()) + ","
            + type + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprConstant p0) {
        if (p0.type().is_int()) {
            return "integer(" + p0 + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
        }
        if (p0.type().is_bool) {
            return "boolean(" + p0 + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
        }
        if (p0.toString().matches("\".*\"")) {
            return "string(" + p0 + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
        }
        return p0 + "(pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprITE p0) {
        return "if_then_else("
            + p0.cond.accept(this) + ","
            + p0.left.accept(this) + ","
            + p0.right.accept(this) + ","
            + this.getType(p0.type()) + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprLet p0) {
        // a let with multiple variables is split into several let expressions by Alloy each having only one var
        return "let(" +
            "identifier(" + sanitizeIdentifier(p0.var.label) + "," + this.getType(p0.var.type()) + ",pos(" + p0.var.pos.x + "," + p0.var.pos.y + ")),"
            + p0.expr.accept(this) + ","
            + p0.sub.accept(this) + ","
            + this.getType(p0.type()) + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprQt p0) {
        List<String> params = p0.decls.stream()
            .flatMap(decl ->
                decl.names.stream().map(it ->
                    "identifier(" + sanitizeIdentifier(it.label) + "," + this.getType(decl.expr.type()) + ",pos(" + p0.pos.x + "," + p0.pos.y + "))"
                )
            )
            .collect(Collectors.toList());

        return getOperator(p0.op.toString()) + "("
            + params + ","
            + p0.decls.stream().map(this::toPrologTerm).collect(Collectors.toList()) + ","
            + p0.sub.accept(this) + ","
            + this.getType(p0.type()) + ","
            + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(ExprUnary p0) {
        if (p0.op == ExprUnary.Op.NOOP) {
            return p0.sub.accept(this);
        } else {
            return getOperator(p0.op.toString()) + "("
                + p0.sub.accept(this) + ","
                + this.getType(p0.type()) + ","
                + "pos(" + p0.pos.x + "," + p0.pos.y + "))";
        }
    }

    @Override
    public String visit(ExprVar p0) {
        return "identifier(" + sanitizeIdentifier(p0.label) + "," + this.getType(p0.type()) + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(Sig p0) {
        return "identifier(" + sanitizeIdentifier(p0.label) + "," + this.getType(p0.type()) + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    @Override
    public String visit(Sig.Field p0) {
        return "identifier(" + sanitizeIdentifier(p0.label) + "," + this.getType(p0.type()) + ",pos(" + p0.pos.x + "," + p0.pos.y + "))";
    }

    private static String getOperator(String op) {
        String operator;
        try {
            operator = Operator.toKeyword(Operator.fromString(op));
        } catch (UnsupportedOperationException exception) {
            System.err.println(exception);
            operator = op.toLowerCase(Locale.getDefault()).replace(" ", "");
        }
        return "'" + operator + "'";
    }

    private String generalizeType(Type type) {
        String typeString = type.toString();
        List<Sig> parents = this.signatures.stream()
            .filter(it -> type.isSubtypeOf(it.type()))
            .collect(Collectors.toList());
        if (type.toString().contains("/Ord")) {
            return "'Ordering'";
        }
        if (parents.isEmpty() || "{Int}".equals(typeString) || "{univ}".equals(typeString)) {
            return cleanUpType(type);
        }
        return cleanUpType(getMostGeneralType(parents));
    }

    /**
     * Transform the type of an Alloy ast node (like t1->t2->..->tn, with arity n) to a Prolog list.
     * Types are generalized to top level signatures.
     */
    private List<String> splitAndCleanType(Type type) {
        if (type.arity() == 0) {
            return new ArrayList<>(Collections.singletonList(cleanUpType(type)));
        }

        List<String> typeList = new ArrayList<>();
        Type.ProductType firstType = type.iterator().next();
        for (int i = 0; i < type.arity(); i++) {
            Sig.PrimSig sig = firstType.get(i);
            typeList.add(this.generalizeType(sig.type()));
        }
        return typeList;
    }

    private static String cleanUpType(Type type) {
        return sanitizeIdentifier(type.toString().replace("{", "").replace("}", ""));
    }

    private static Type getMostGeneralType(List<Sig> types) {
        Sig mgt = types.get(0);
        for (int i = 1; i < types.size(); i++) {
            Sig currType = types.get(i);
            if (mgt.isSameOrDescendentOf(currType)) {
                mgt = currType;
            }
        }
        return mgt.type();
    }

    /**
     * Transform the type of an Alloy ast node to a Prolog term type(ListOfType,Arity).
     * Additionally, log if sequences are used.
     */
    private String getType(Type type) {
        List<String> tType = this.splitAndCleanType(type);
        String tTypeString = tType.toString();
        Pattern seqTypeRegex = Pattern.compile(".seq'.");
        this.usesSequences = this.usesSequences || seqTypeRegex.matcher(tTypeString).find();
        return "type(" + (tType.isEmpty() ? "[untyped]" : tTypeString) + "," + type.arity() + ")";
    }
}
