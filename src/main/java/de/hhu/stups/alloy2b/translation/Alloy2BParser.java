package de.hhu.stups.alloy2b.translation;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.CommandScope;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;

import static de.hhu.stups.alloy2b.translation.ParserUtil.sanitizeIdentifier;

/**
 * Convert the abstract syntax tree of an Alloy model to a Prolog term.
 */
public final class Alloy2BParser {
    private final List<Sig> signatures;
    private final List<String> orderedSignatures;
    private final Set<Set<String>> setsOfParents;
    private final List<Sig> enums;
    private final ExpressionToProlog expressionTranslator;

    private ConstList<Command> commands;

    public Alloy2BParser() {
        this.signatures = new ArrayList<>();
        this.orderedSignatures = new ArrayList<>();
        this.setsOfParents = new HashSet<>();
        this.enums = new ArrayList<>();
        this.expressionTranslator = new ExpressionToProlog(this.signatures, this.orderedSignatures, this.setsOfParents);

        this.commands = null; // initialized in translateModule
    }

    private static <T> Stream<T> safeListStream(SafeList<T> safeList) {
        return safeList.makeConstList().stream();
    }

    private String translateModule(CompModule module) {
        this.commands = module.getAllCommands();

        // if the module is named by the user using 'as', module.path is set
        String sModelName = sanitizeIdentifier(module.getModelName());
        String name = module.path.isEmpty()
            ? "(" + sModelName + ",alloy2b_none)"
            : "(" + sModelName + "," + sanitizeIdentifier(module.path) + ")";

        String listOfFacts = safeListStream(module.getAllFacts())
            .map(this::toPrologTerm)
            .collect(Collectors.joining(","));

        String listOfAssertions = module.getAllAssertions().stream()
            .map(this::toPrologTerm)
            .collect(Collectors.joining(","));

        String listOfCommands = this.commands.stream()
            .map(this::toPrologTerm)
            .collect(Collectors.joining(","));

        String listOfFunctions = safeListStream(module.getAllFunc())
            .map(this::toPrologTerm)
            .collect(Collectors.joining(","));

        String listOfSignatures = safeListStream(module.getAllSigs())
            .map(this::toPrologTerm)
            .collect(Collectors.joining(","));

        String parentTypes = "parent_types:" + this.setsOfParents;
        this.setsOfParents.clear();

        return "alloy_model(" + name + ","
            + "facts([" + listOfFacts + "]),"
            + "assertions([" + listOfAssertions + "]),"
            + "commands([" + listOfCommands + "]),"
            + "functions([" + listOfFunctions + "]),"
            + "signatures([" + listOfSignatures + "]),"
            + "ordered_signatures(" + this.orderedSignatures.stream().map(s -> "'" + s + "'").collect(Collectors.toList()) + "),"
            + "[sequences:" + this.expressionTranslator.getUsesSequences() + "," + parentTypes + ","
            + "ordering_successors_only:" + this.expressionTranslator.getOrderingsUseSuccessorsOnly() + "])";
    }

    private static String realPath(String alloyModelPath) {
        try {
            // either from resources
            URL resourceUrl = Alloy2BParser.class.getResource(alloyModelPath);
            if (resourceUrl == null) {
                return alloyModelPath;
            } else {
                try {
                    return new File(resourceUrl.toURI()).toString();
                } catch (URISyntaxException exc) {
                    throw new IllegalArgumentException(exc);
                }
            }
        } catch (IllegalStateException exception) {
            // or an absolute path
            return alloyModelPath;
        }
    }

    private void collectPropertiesFromInclude(CompModule.Open it) {
        if ("util/ordering".equals(it.filename)) {
            List<String> prefixedSignatures = it.args.stream().map(s -> s.replace("this/", "")).collect(Collectors.toList());
            this.orderedSignatures.addAll(prefixedSignatures);
            this.orderedSignatures.add(it.alias);
        }
    }

    public ParserResult parseFromFile(String alloyModelPath) throws Alloy2BParserErr {
        this.orderedSignatures.clear();
        this.expressionTranslator.setUsesSequences(false);
        String path = realPath(alloyModelPath);
        try {
            CompModule astRoot = CompUtil.parseEverything_fromFile(new A4Reporter(), null, path);
            astRoot.getOpens().forEach(this::collectPropertiesFromInclude);
            this.signatures.addAll(astRoot.getRootModule().getAllSigs().makeConstList()); // primitive types are not considered here

            String modules = safeListStream(astRoot.getAllReachableModules()).map(this::translateModule).collect(Collectors.joining(","));
            String rootModule = sanitizeIdentifier(astRoot.getRootModule().getModelName());

            List<String> commandNames = new ArrayList<>();
            for (int i = 0; i < astRoot.getRootModule().getAllCommands().size(); i++) {
                Command cmd = astRoot.getRootModule().getAllCommands().get(i);
                if (cmd.check) {
                    commandNames.add("check" + i);
                } else {
                    commandNames.add("run" + i);
                }
            }

            return new ParserResult("alloy(" + rootModule + ",[" + modules + "]).", ConstList.make(commandNames));
        } catch (Err exception) {
            throw new Alloy2BParserErr(exception);
        }
    }

    private String toPrologTerm(Pair<String, Expr> astNode) {
        return "fact(" + astNode.b.accept(this.expressionTranslator) + ",(" + astNode.b.pos.x + "," + astNode.b.pos.y + "))";
    }

    private String toPrologTerm(Command astNode) {
        String functor = astNode.check ? "check" : "run";

        List<String> exactScopes = astNode.scope.stream()
            .filter(it -> it.isExact)
            .map(Alloy2BParser::createSigScopeTuple)
            .collect(Collectors.toList());

        List<String> upperBoundScopes = astNode.scope.stream()
            .filter(it -> !it.isExact)
            .map(Alloy2BParser::createSigScopeTuple)
            .collect(Collectors.toList());

        return functor + "("
            + astNode.formula.accept(this.expressionTranslator) + ","
            + "global_scope(" + astNode.overall + "),"
            + "exact_scopes(" + exactScopes + "),"
            + "upper_bound_scopes(" + upperBoundScopes + "),"
            + "bitwidth(" + astNode.bitwidth + "),"
            + "maxseq(" + astNode.maxseq + "),"
            + "index(" + this.commands.indexOf(astNode) + "),"
            + "pos(" + astNode.pos.x + "," + astNode.pos.y + "))";
    }

    private static String createSigScopeTuple(CommandScope scope) {
        return "(" + sanitizeIdentifier(scope.sig.label) + "," + scope.startingScope + ")";
    }

    private String toPrologTerm(Func astNode) {
        String functor = astNode.isPred ? "predicate" : "function";
        return functor + "("
            + sanitizeIdentifier(astNode.label) + ","
            + astNode.params().stream().map(this::toPrologTerm).collect(Collectors.toList()) + ","
            + astNode.decls.stream().map(this.expressionTranslator::toPrologTerm).collect(Collectors.toList()) + ","
            + this.toPrologTerm(astNode.getBody()) + ","
            + "pos(" + astNode.pos.x + "," + astNode.pos.y + "))";
    }

    private String toPrologTerm(Expr astNode) {
        return astNode.accept(this.expressionTranslator);
    }

    private String toPrologTerm(Sig astNode) {
        // enums are syntactical sugar: elements are 'one sigs' which extend the base sig
        List<Sig> parentSig = this.enums.stream()
            .filter(it -> astNode.isSameOrDescendentOf(it) && !astNode.isSame(it))
            .collect(Collectors.toList());

        boolean isSubsig = !parentSig.isEmpty();
        String options = isSubsig
            ? "[one,subsig(" + sanitizeIdentifier(((Sig.PrimSig)astNode).parent.label) + ")]"
            : collectSignatureOptionsToPrologList(astNode);

        if (astNode.isEnum != null) {
            this.enums.add(astNode);
        }

        if (!isSubsig) {
            this.signatures.add(astNode);
        }

        return "signature("
            + sanitizeIdentifier(astNode.label) + ","
            + "[" + safeListStream(astNode.getFieldDecls()).map(this.expressionTranslator::toPrologTerm).collect(Collectors.joining(",")) + "],"
            + "[" + safeListStream(astNode.getFacts()).map(this::toPrologTerm).collect(Collectors.joining(",")) + "],"
            + options + ","
            + "pos(" + astNode.pos.x + "," + astNode.pos.y + "))";
    }

    private String collectSignatureOptionsToPrologList(Sig astNode) {
        List<String> lstOptions = new ArrayList<>();
        String sigName = astNode.label.replace("this/", "");
        if (astNode.isEnum != null) {
            // enums are treated as orderings in Alloy
            lstOptions.add("enum");
            this.orderedSignatures.remove(sigName);
        }
        if (this.orderedSignatures.contains(sigName)) {
            lstOptions.add("ordered");
        }
        if (astNode.isAbstract != null) {
            lstOptions.add("abstract");
        }
        if (astNode.isLone != null) {
            lstOptions.add("lone");
        }
        if (astNode.isMeta != null) {
            lstOptions.add("meta");
        }
        if (astNode.isOne != null) {
            lstOptions.add("one");
        }
        if (astNode.isPrivate != null) {
            lstOptions.add("private");
        }
        if (astNode.isSome != null) {
            lstOptions.add("some");
        }
        if (astNode.isSubset != null) {
            lstOptions.add("subset(" + ((Sig.SubsetSig)astNode).parents.stream().map(it -> sanitizeIdentifier(it.label)).collect(Collectors.toList()) + ")");
        }
        if (isExtendingSignature(astNode)) {
            lstOptions.add("subsig(" + sanitizeIdentifier(((Sig.PrimSig)astNode).parent.label) + ")");
        }
        return lstOptions.toString();
    }

    private boolean isExtendingSignature(Sig sig) {
        Pos isSubSig = sig.isSubsig;
        return sig instanceof Sig.PrimSig && isSubSig != null && (isSubSig.x != isSubSig.x2 || isSubSig.y != isSubSig.y2);
    }
}
