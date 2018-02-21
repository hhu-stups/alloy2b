:- module(alloy2b,[translate_model/2]).

:- use_module(library(lists)).

build_machine_ast(b_machine(ListOfMachineParts),machine(generated(none,AbstractMachine))) :- 
    AbstractMachine =.. [abstract_machine,none,machine(none),machine_header(none,alloytranslation,[])|ListOfMachineParts].

empty_machine_acc(b_machine([sets(none,[]),constants(none,[]),definitions(none,[]),properties(none,[]),assertions(none,[]),operations(none,[])])).

extend_machine_acc(Functor,b_machine(MachineParts),New,b_machine([NewMachinePart|RestMachineParts])) :- 
    MachinePart =.. [Functor,none,List] , 
    member(MachinePart,MachineParts) , 
    delete(MachineParts,MachinePart,RestMachineParts) , 
    NewMachinePart =.. [Functor,none,[New|List]].

translate_model(alloy_model(facts(Facts),assertions(Assertions),commands(Commands),functions(Functions),signatures(Signatures)),BAst) :- 
    retractall(signature(_,_,_,_,_)) , 
    empty_machine_acc(MAcc) , 
    map_translate(signature,MAcc,Signatures,MAcc1) , 
    map_translate(assertion,MAcc1,Assertions,MAcc2) , 
    map_translate(command,MAcc2,Commands,MAcc3) , 
    map_translate(function,MAcc3,Functions,MAcc4) , 
    map_translate(fact,MAcc4,Facts,MAcc5) , 
    build_machine_ast(MAcc5,BAst).

% Map the translation over a list and accumulate the results. Type is one of signature, assertion, command, function or fact.
map_translate(_,MAcc,[],MAcc).
map_translate(Type,MAcc,[Part|T],Res) :- 
    atom_concat(translate_,Type,Functor) , 
    TranslationCall =.. [Functor,MAcc,Part,NewMAcc] , 
    call(TranslationCall) , 
    map_translate(Type,NewMAcc,T,Res).

% signature
translate_signature(MAcc,signature(Name,Fields,Facts,Options,_Pos),NewMAcc) :- 
    % assert signatures for singleton checks
    asserta(signature(Name,Fields,Facts,Options,_Pos)) , 
    define_sig_as_set_or_constant(MAcc,Name,Options,_Pos,MAcc1) ,
    map_translate(field,MAcc1,Fields,MAcc2) , 
    map_translate(fact,MAcc2,Facts,NewMAcc).

% field
translate_field(MAcc,Field,NewMAcc) :- 
    translate_field_aux(Field,TField) , 
    extend_machine_acc(properties,MAcc,TField,NewMAcc).
translate_field_aux(field(Name,Expr,_Pos),TField) :- 
    % TODO: we may need a quantification over this_ (see Kotlin code)
    translate_e_p(Name,TID) , 
    decl_special_cases(Expr,TID,TField) , !.

% fact
translate_fact(MAcc,fact(Expr,_Pos),NewMAcc) :- 
    translate_e_p(Expr,TExpr) , 
    extend_machine_acc(properties,MAcc,TExpr,NewMAcc).

% function or predicate
translate_function(MAcc,FunctionOrPredicate,NewMAcc) :- 
    FunctionOrPredicate =.. [Functor,Name,Params,Decls,Body,_Pos] , 
    ((Functor = function , UFunctor = expression_definition) ; (Functor = predicate , UFunctor = predicate_definition)) , 
    maplist(translate_e_p,Params,TParams) , 
    maplist(translate_field_aux,Decls,TDecls) , 
    translate_e_p(Body,TBody) , 
    append(TDecls,[TBody],BehaviorList) , 
    conjunct_predicates(BehaviorList,Behavior) , 
    UAst =.. [UFunctor,none,Name,TParams,Behavior] , 
    extend_machine_acc(definitions,MAcc,UAst,NewMAcc).

% subset
define_sig_as_set_or_constant(MAcc,Name,Options,_Pos,NewMAcc) :- 
    memberchk(subset(Parents),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,MAcc1) , 
    % TODO: consider several parents -> we need the universe type
    Parents = [Parent|_] , 
    extend_machine_acc(properties,MAcc1,
        member(none,identifier(none,Name),identifier(none,Parent)),NewMAcc).
% extends
define_sig_as_set_or_constant(MAcc,Name,Options,_Pos,NewMAcc) :- 
    member(subsig(Parent),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,MAcc1) ,
    extend_machine_acc(properties,MAcc1,
        member(identifier(none,Name),identifier(none,Parent)),NewMAcc).
% default signature
define_sig_as_set_or_constant(MAcc,Name,Options,_Pos,NewMAcc) :- 
    define_sig_as_set_or_constant_aux(sets,MAcc,Name,Options,_Pos,NewMAcc).

define_sig_as_set_or_constant_aux(SetsOrConstants,MAcc,Name,_Options,_Pos,NewMAcc) :- 
    extend_machine_acc(SetsOrConstants,MAcc,identifier(none,Name),NewMAcc).

translate_e_p(A,TA) :- 
    translate_unary_e_p(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_binary_e_p(A,TA) , !.
translate_e_p(A,_) :- 
    format("Translation failed for ~w.~n",[A]).

% unary expressions and predicates
translate_unary_e_p(ID,identifier(none,ID)) :- atom(ID) , !.
translate_unary_e_p('this'/ID,identifier(none,ID)) :- !.
translate_unary_e_p(integer(A,_Pos),integer(none,A)) :- !.
translate_unary_e_p(boolean(A,_Pos),TA) :- ! , 
    alloy_to_b_operator(A,TA).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    UnaryP =.. [Op,Arg,_Type,_Pos] , 
    translate_e_p(Arg,TArg) , 
    alloy_to_b_operator(Op,BOp) , 
    TUnaryP =.. [BOp,none,TArg].

% binary expressions and predicates
translate_binary_e_p(BinaryP,TBinaryP) :- 
    % and/or possibly defines a list of ast nodes
    BinaryP =.. [Op,ArgList|_] , 
    memberchk(Op,[and,or]) , 
    is_list(ArgList) , ! , 
    maplist(translate_e_p,ArgList,TArgList) , 
    conjunct_predicates(TArgList,TBinaryP).
translate_binary_e_p(Binary,TBinary) :- 
    Binary =.. [Op,Arg1,Arg2|_] , 
    translate_e_p(Arg1,TArg1) , 
    translate_e_p(Arg2,TArg2) , 
    alloy_to_b_operator(Op,BOp) , 
    TBinary =.. [BOp,none,TArg1,TArg2].

decl_special_cases(DeclTerm,TFieldID,TField) :- 
    DeclTerm =.. [_,SetID|_] , 
    translate_e_p(SetID,TSetID) , 
    decl_special_cases_aux(DeclTerm,TSetID,TFieldID,TField).

decl_special_cases_aux(setof(_,_,_),TSetID,TFieldID,member(none,TFieldID,TSetID)).
decl_special_cases_aux(oneof(_,_,_),TSetID,TFieldID,
    conjunct(none,member(none,TFieldID,TSetID),equal(none,card(none,TFieldID),integer(none,1)))).
decl_special_cases_aux(someof(_,_,_),TSetID,TFieldID,
    conjunct(none,member(none,TFieldID,TSetID),greater(none,card(none,TFieldID),integer(none,0)))).
decl_special_cases_aux(loneof(_,_,_),TSetID,TFieldID,
    conjunct(none,member(none,TFieldID,TSetID),less_equal(none,card(none,TFieldID),integer(none,1)))).

% most of the operators can be translated straightforwardly from Alloy to B
alloy_to_b_operator(in,subset) :- !.
alloy_to_b_operator(plus,union) :- !.
alloy_to_b_operator(minus,set_subtraction) :- !.
alloy_to_b_operator(true,boolean_true(none)) :- !.
alloy_to_b_operator(false,boolean_false(none)) :- !.
alloy_to_b_operator(Op,Op).

is_identifier(this/ID,ID) :- signature(this/ID,_,_,_,_).
is_identifier(ID,ID) :- is_field(_,ID,_).
 
is_field(Set,Name,Type) :- signature(Set,Fields,_,_,_),
     memberchk(field(Name,Type,_Pos),Fields).

singleton_set(S) :- 
    signature(this/S,_,_,Options,_) , 
    memberchk(one,Options).

conjunct_predicates([],b(truth,pred,[])).
conjunct_predicates([H|T],Conjoined) :- 
    conjunct_predicates(T,H,Conjoined).

conjunct_predicates([],Acc,Acc).
conjunct_predicates([H|T],Acc,Conjoined) :- 
    conjunct_predicates(T,conjunct(none,H,Acc),Conjoined).