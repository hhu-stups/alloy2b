:- module(alloy2b,[translate_model/2]).

:- use_module(library(lists),[maplist/3,is_list/1,sublist/5,select/3]).

% An automated translation from Alloy to classical B.
% The Alloy abstract syntax tree is translated to an untyped B AST as supported by ProB.
% Afterwards, the untyped AST can be typechecked and loaded by ProB.

translate_model(alloy_model(facts(Facts),assertions(Assertions),commands(Commands),functions(Functions),signatures(Signatures)),BAst) :- 
    % signatures are asserted at runtime for singleton or field checks
    % accumulate all translations, afterwards we build the untyped machine ast
    empty_machine_acc(MAcc) , 
    map_translate(signature,MAcc,Signatures,MAcc1) , 
    map_translate(assertion,MAcc1,Assertions,MAcc2) , 
    map_translate(command,MAcc2,Commands,MAcc3) , 
    map_translate(function,MAcc3,Functions,MAcc4) , 
    map_translate(fact,MAcc4,Facts,MAcc5) , ! , 
    build_machine_ast(MAcc5,BAst) , 
    retract_state(MAcc5).

% Map the translation over a list and accumulate the results. 
% Type is one of signature, assertion, command, function, field or fact.
map_translate(_,MAcc,[],MAcc).
map_translate(Type,MAcc,[Part|T],Res) :- 
    atom_concat_safe(translate_,Type,Functor) , 
    TranslationCall =.. [Functor,MAcc,Part,NewMAcc] , 
    call(TranslationCall) , 
    map_translate(Type,NewMAcc,T,Res).

% signature
translate_signature(MAcc,signature(Name,Fields,Facts,Options,Pos),NewMAcc) :- 
    % assert signatures for singleton checks
    assert_signature_term(signature(Name,Fields,Facts,Options,Pos)) , 
    extend_machine_acc(signatures,MAcc,[Name],MAcc1) ,  
    define_sig_as_set_or_constant(MAcc1,Name,Options,_Pos,MAcc2) ,
    translate_fields(MAcc2,Name,Fields,MAcc3) , 
    map_translate(fact,MAcc3,Facts,NewMAcc).

% list of fields from a specific signature
translate_fields(MAcc,SignatureName,Fields,NewMAcc) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    translate_fields_aux(MAcc,TSignatureName,Fields,NewMAcc).

translate_fields_aux(MAcc,_,[],MAcc).
translate_fields_aux(MAcc,TSignatureName,[Field|T],NewMAcc) :- 
    translate_field(TSignatureName,MAcc,Field,MAcc1) , 
    translate_fields_aux(MAcc1,TSignatureName,T,NewMAcc).

% field
translate_field(TSignatureName,MAcc,Field,NewMAcc) :- 
    translate_field_aux(TSignatureName,MAcc,Field,TField,MAcc1) , 
    extend_machine_acc(properties,MAcc1,TField,NewMAcc).

translate_field_aux(TSignatureName,MAcc,field(Name,Expr,type(_Type,_Arity),_Pos),TField,NewMAcc) :- 
    % TODO: we may need a quantification over this_ (see Kotlin code)
    translate_e_p(Name,TName) , 
    extend_machine_acc(constants,MAcc,TName,NewMAcc) , 
    field_decl_special_cases(TSignatureName,Expr,TName,TField) , !.

translate_function_field(field(Name,Expr,type(_Type,_Arity),_Pos),TField) :- 
    translate_e_p(Name,TID) , 
    fun_or_pred_decl_special_cases(Expr,TID,TField) , !.

% fact
translate_fact(MAcc,Fact,NewMAcc) :- 
    % either the term fact/2 or an expression (for instance, if defined within a signature)
    (Fact = fact(Expr,_Pos) ; Expr = Fact) , 
    translate_e_p(Expr,TExpr) , 
    extend_machine_acc(properties,MAcc,TExpr,NewMAcc).

% function or predicate
translate_function(MAcc,FunctionOrPredicate,NewMAcc) :- 
    FunctionOrPredicate =.. [Functor,Name,Params,Decls,Body,_Pos] , 
    alloy_to_b_operator(Functor,BFunctor) , 
    maplist(translate_e_p,Params,TParams) , 
    maplist(translate_function_field,Decls,TDecls) , 
    translate_e_p(Body,TBody) , 
    append(TDecls,[TBody],BehaviorList) , 
    join_predicates_aux(conjunct,BehaviorList,Behavior) , 
    UAst =.. [BFunctor,none,Name,TParams,Behavior] , 
    extend_machine_acc(definitions,MAcc,UAst,NewMAcc).

% assertion
translate_assertion(MAcc,Assertion,NewMAcc) :- 
    translate_fact(MAcc,Assertion,NewMAcc).

% check and run command
translate_command(MAcc,Command,NewMAcc) :- 
    Command =.. [Functor,Body,GlobalScope,ExactScopes,BitWidth,_Pos] , 
    (Functor = check ; Functor = run) , 
    translate_e_p(Body,TBody) , 
    % we need all signature names to define the global scope
    get_signature_names_from_machine_acc(MAcc,SignatureNames) , 
    translate_scopes(SignatureNames,GlobalScope,ExactScopes,BitWidth,TScopesPred) , 
    Precondition = conjunct(none,TScopesPred,TBody) , 
    % TODO: generate unique operation name
    Operation = operation(none,identifier(none,Functor),[],[],precondition(none,Precondition,skip(none))) , 
    extend_machine_acc(operations,MAcc,Operation,NewMAcc).

% global scope, exact scopes and bitwidth
% We do not need to set the bitwidth since we have real integers in B. 
% Note: If only one command is defined in the Alloy model we could set min and max int of ProB in the definitions.
translate_scopes(SignatureNames,global_scope(GlobalScope),exact_scopes(ExactScopes),_BitWidth,conjunct(none,TExactScopes,TGlobalScopes)) :- 
    % translate exact scopes first
    translate_exact_scopes(ExactScopes,ExactSignatureNames,TExactScopes) ,  
    % if present, define the global scope for the remaining signatures
    translate_global_scope(GlobalScope,SignatureNames,ExactSignatureNames,TGlobalScopes).

translate_exact_scopes([],_,truth(none)) :- !. % no exact scopes defined
translate_exact_scopes(ExactScopes,ExactSignatureNames,TExactScopes) :- 
    translate_exact_scopes_aux(ExactScopes,[],ExactSignatureNames,[],TempTExactScopes) , 
    join_predicates(conjunct,TempTExactScopes,TExactScopes).

translate_exact_scopes_aux([],NamesAcc,NamesAcc,TScopeAcc,TScopeAcc).
translate_exact_scopes_aux([(SignatureName,Scope)|T],NameAcc,ExactSignatureNames,TScopeAcc,TempTExactScopes) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    TScope = equal(none,card(none,TSignatureName),integer(none,Scope)) , 
    translate_exact_scopes_aux(T,[SignatureName|NameAcc],ExactSignatureNames,[TScope|TScopeAcc],TempTExactScopes).

translate_global_scope(-1,_,_,truth(none)) :- !. % no global scope defined
translate_global_scope(GlobalScope,SignatureNames,ExactSignatureNames,TGlobalScopes) :- 
    findall(SignatureName,(member(SignatureName,SignatureNames) , \+member(SignatureName,ExactSignatureNames)),RestSignatureNames) , 
    translate_global_scope_aux(RestSignatureNames,GlobalScope,[],TempTGlobalScopes) , 
    join_predicates(conjunct,TempTGlobalScopes,TGlobalScopes).

translate_global_scope_aux([],_,Acc,Acc).
translate_global_scope_aux([SignatureName|T],GlobalScope,Acc,TGlobalScopes) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    TScope = less_equal(none,card(none,TSignatureName),integer(none,GlobalScope)) , 
    translate_global_scope_aux(T,GlobalScope,[TScope|Acc],TGlobalScopes).

% signature in (subset)
define_sig_as_set_or_constant(MAcc,Name,Options,Pos,NewMAcc) :- 
    memberchk(subset(Parents),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,Pos,MAcc1) , 
    % TODO: consider several parents -> we need the universe type
    Parents = [Parent|_] , 
    translate_e_p(Name,TName) , 
    translate_e_p(Parent,TParent) , 
    extend_machine_acc(properties,MAcc1,member(none,TName,TParent),NewMAcc).
% signature extends
define_sig_as_set_or_constant(MAcc,Name,Options,Pos,NewMAcc) :- 
    member(subsig(Parent),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,Pos,MAcc1) ,
    translate_e_p(Name,TName) , 
    translate_e_p(Parent,TParent) , 
    extend_machine_acc(properties,MAcc1,member(none,TName,TParent),NewMAcc).
% default signature
define_sig_as_set_or_constant(MAcc,Name,Options,Pos,NewMAcc) :- 
    define_sig_as_set_or_constant_aux(sets,MAcc,Name,Options,Pos,NewMAcc).

define_sig_as_set_or_constant_aux(sets,MAcc,Name,_Options,_Pos,NewMAcc) :- 
    % use deferred_set/2 instead of identifier/2 like for constants
    extend_machine_acc(sets,MAcc,deferred_set(none,Name),NewMAcc).
define_sig_as_set_or_constant_aux(constants,MAcc,Name,_Options,_Pos,NewMAcc) :- 
    translate_e_p(Name,TName) , 
    extend_machine_acc(constants,MAcc,TName,NewMAcc).

% translate expression or predicate
translate_e_p(A,TA) :- 
    % check quantifiers first
    translate_quantifier_e(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_unary_e_p(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_binary_e_p(A,TA) , !.
translate_e_p(A,_) :- 
    format("Translation failed for ~w.~n",[A]).

% quantifiers
translate_quantifier_e(Quantifier,TQuantifier) :- 
    Quantifier =.. [Functor,Params,Fields,Body,_Type,_Pos] , 
    maplist(translate_e_p,Params,TParams) , 
    maplist(translate_function_field,Fields,TFieldsList) , 
    join_predicates_aux(conjunct,TFieldsList,TFields) , 
    translate_e_p(Body,TBody) , 
    translate_quantifier_e_aux(Functor,TParams,TFields,TBody,TQuantifier).

translate_quantifier_e_aux(all,TParams,TFields,TBody,forall(none,TParams,implication(none,TFields,TBody))).
translate_quantifier_e_aux(no,TParams,TFields,TBody,negation(none,exists(none,TParams,conjunct(none,TFields,TBody)))).
translate_quantifier_e_aux(some,TParams,TFields,TBody,exists(none,TParams,conjunct(none,TFields,TBody))).
translate_quantifier_e_aux(one,TParams,TFields,TBody,equal(none,card(none,comprehension_set(none,TParams,conjunct(none,TFields,TBody))),integer(none,1))).
translate_quantifier_e_aux(lone,TParams,TFields,TBody,less_equal(none,card(none,comprehension_set(none,TParams,conjunct(none,TFields,TBody))),integer(none,1))).

% unary expressions and predicates
translate_unary_e_p(Int,integer(none,Int)) :- integer(Int) , !.
translate_unary_e_p(identifier(ID,_Type,_Pos),identifier(none,ID)) :- !.
translate_unary_e_p(ID,identifier(none,ID)) :- atom(ID) , !.
translate_unary_e_p(integer(A,_Pos),integer(none,A)) :- !.
translate_unary_e_p(boolean(A,_Pos),TA) :- ! , 
    alloy_to_b_operator(A,TA).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    % and/or defines a list of ast nodes
    UnaryP =.. [Op,ArgList|_] , 
    memberchk(Op,[and,or]) , 
    is_list(ArgList) , ! , 
    maplist(translate_e_p,ArgList,TArgList) , 
    join_predicates(Op,TArgList,TUnaryP).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    UnaryP =.. [Op,Arg|_] , 
    member(Op,[no,one,some,lone]) , ! ,
    translate_e_p(Arg,TArg) ,  
    translate_quantified_e(Op,TArg,TUnaryP).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    UnaryP =.. [Op,Arg,_Type,_Pos] , 
    translate_e_p(Arg,TArg) , 
    alloy_to_b_operator(Op,BOp) , 
    TUnaryP =.. [BOp,none,TArg].

% binary expressions and predicates
translate_binary_e_p(join(Arg1,Arg2,_Type,_Pos),TBinaryJoin) :- ! , 
    % special case: join
    translate_join(Arg1,Arg2,TBinaryJoin).
translate_binary_e_p(Call,TCall) :- 
    Call =.. [Functor,Name,Params,Type,Pos] , 
    (Functor = pred_call ; Functor = fun_call ) , ! , 
    % special case: predicate and function call
    % TODO: fun_call may be from a utility module like ordering\first
    maplist(translate_e_p,Params,TParams) , 
    TCall =.. [definition,Name,TParams,Type,Pos].
translate_binary_e_p(Binary,TBinary) :- 
    Binary =.. [Op,Arg1,Arg2|_] , 
    translate_e_p(Arg1,TArg1) , 
    translate_e_p(Arg2,TArg2) , 
    alloy_to_b_operator(Op,BOp) , 
    TBinary =.. [BOp,none,TArg1,TArg2].

% Translation of the dot join operator has several special cases depending on the arity of the arguments.
translate_join(Arg1,Arg2,TBinaryJoin) :- 
    translate_e_p(Arg1,TArg1) , 
    translate_e_p(Arg2,TArg2) , 
    translate_join_aux(Arg1,Arg2,TArg1,TArg2,TBinaryJoin).

% univ._
translate_join_aux(identifier('univ_',type(['univ_'],1),_),_Arg2,_TArg1,TArg2,ran(none,TArg2)).
% _.univ
translate_join_aux(_Arg1,identifier('univ_',type(['univ_'],1),_),TArg1,_TArg2,dom(none,TArg1)).
% unary._
translate_join_aux(Arg1,_Arg2,TArg1,TArg2,image(none,TArg2,TArg1)) :- 
    is_unary_relation(Arg1).
% binary.unary
translate_join_aux(Arg1,Arg2,TArg1,TArg2,image(none,reverse(none,TArg1),TArg2)) :- 
    is_binary_relation(Arg1) , is_unary_relation(Arg2).
% binary.binary
translate_join_aux(Arg1,Arg2,TArg1,TArg2,parallel_product(none,TArg1,TArg2)) :-
    is_binary_relation(Arg1) , is_binary_relation(Arg2).
translate_join_aux(Arg1,Arg2,_TArg1,_TArg2,empty_set(none)) :- 
    format("Join not supported this way:~nLeft: ~w~nRight: ~w~n",[Arg1,Arg2]).

% Unary quantified expressions: no, one, some, lone
translate_quantified_e(no,TArg,equal(none,TArg,empty_set(none))).
translate_quantified_e(one,TArg,equal(none,card(none,TArg),integer(none,1))).
translate_quantified_e(some,TArg,greater(none,card(none,TArg),integer(none,0))).
translate_quantified_e(lone,TArg,less_equal(none,card(none,TArg),integer(none,1))).

% Field declarations have several special cases depending on the keywords set, one, some or lone.
field_decl_special_cases(TSignatureName,DeclTerm,TFieldID,TField) :- 
    DeclTerm =.. [_,SetID|_] , 
    translate_e_p(SetID,TSetID) , 
    field_decl_special_cases_aux(DeclTerm,TSignatureName,TSetID,TFieldID,TField).

field_decl_special_cases_aux(setof(_,_,_),TSignatureName,TSetID,TFieldID,member(none,TFieldID,relations(none,TSignatureName,TSetID))).
field_decl_special_cases_aux(oneof(_,_,_),TSignatureName,TSetID,TFieldID,member(none,TFieldID,total_function(none,TSignatureName,TSetID))).
field_decl_special_cases_aux(loneof(_,_,_),TSignatureName,TSetID,TFieldID,member(none,TFieldID,partial_function(none,TSignatureName,TSetID))).
field_decl_special_cases_aux(Term,_,_,_,_) :- 
    format("Field declaration not implemented: ~w",[Term]).

% In function or predicate definitions one can use either set or one.
fun_or_pred_decl_special_cases(setof(SetID,_,_),TFieldID,subset(none,TFieldID,TSetID)) :- 
    translate_e_p(SetID,TSetID).
fun_or_pred_decl_special_cases(oneof(SetID,_,_),TFieldID,subset(none,set_extension(none,[TFieldID]),TSetID)) :- 
    translate_e_p(SetID,TSetID).
fun_or_pred_decl_special_cases(Term,_,_) :- 
    format("Field declaration for function or predicate not implemented: ~w",[Term]).

% Most of the operators can be translated straightforwardly from Alloy to B.
alloy_to_b_operator(Op,BOp) :- 
    alloy_to_b_operator_aux(Op,BOp) , !.

alloy_to_b_operator_aux(in,subset).
alloy_to_b_operator_aux(plus,union).
alloy_to_b_operator_aux(not,negation).
alloy_to_b_operator_aux(or,disjunct).
alloy_to_b_operator_aux(and,conjunct).
alloy_to_b_operator_aux(minus,set_subtraction).
alloy_to_b_operator_aux(true,boolean_true(none)).
alloy_to_b_operator_aux(false,boolean_false(none)).
alloy_to_b_operator_aux(closure1,reflexive_closure).
alloy_to_b_operator_aux(function,expression_definition).
alloy_to_b_operator_aux(predicate,predicate_definition).
alloy_to_b_operator_aux(Op,Op).

%%% 
% Accumulate the translated machine parts during the translation and build the machine AST afterwards.
build_machine_ast(b_machine(ListOfMachineParts,_SignatureNames),machine(generated(none,AbstractMachine))) :- 
    % filter empty machine parts
    findall(MachinePart,(member(MachinePart,ListOfMachineParts) , MachinePart =.. [_,_,L] , L \= []),TempListOfUsedMachineParts) , 
    % properties need to be conjoined
    select(properties(none,L),TempListOfUsedMachineParts,RestListOfUsedMachineParts) , 
    join_predicates(conjunct,L,FlatL) , 
    AbstractMachine =.. [abstract_machine,none,machine(none),machine_header(none,alloytranslation,[]),[properties(none,FlatL)|RestListOfUsedMachineParts]].

empty_machine_acc(b_machine([sets(none,[]),constants(none,[]),definitions(none,[]),properties(none,[]),assertions(none,[]),operations(none,[])],[])).

extend_machine_acc(signatures,b_machine(MachineParts,SignatureNames),New,b_machine(MachineParts,NewSignatureNames)) :- 
    append(New,SignatureNames,NewSignatureNames) , !.
extend_machine_acc(Functor,b_machine(MachineParts,SignatureNames),New,b_machine([NewMachinePart|RestMachineParts],SignatureNames)) :- 
    MachinePart =.. [Functor,none,List] , 
    select(MachinePart,MachineParts,RestMachineParts) , 
    NewMachinePart =.. [Functor,none,[New|List]].

get_signature_names_from_machine_acc(b_machine(_MachineParts,SignatureNames),SignatureNames).
%%%

%%% Assert signatures for singleton checks.
% Asserts signature_ID(Fields,Options) for variable ID.
% Options is a subset of [abstract,enum,meta,lone,one,private,some,subset,subsig,top_level].
assert_signature_term(signature(ID,Fields,_,Options,_)) :- 
    atom_concat_safe(signature_,ID,Functor) , 
    SignatureTerm =.. [Functor,Fields,Options] , 
    asserta(SignatureTerm).

retract_state(b_machine(_MachineParts,SignatureNames)) :- 
    retract_state_aux(signature_,SignatureNames).

retract_state_aux(_,[]).
retract_state_aux(Prefix,[ID|T]) :- 
    atom_concat_safe(Prefix,ID,Functor) , 
    Term =.. [Functor,_,_] , 
    retractall(Term) , 
    retract_state_aux(Prefix,T).

get_fields_and_options_from_signature(ID,Fields,Options) :- 
    atom_concat_safe(signature_,ID,Functor) , 
    SignatureTerm =.. [Functor,Fields,Options] , 
    on_exception(_,call(SignatureTerm),fail).

%is_singleton_set(ID) :- 
%    atom_or_identifier_term(ID,IDName) , 
%    get_fields_and_options_from_signature(IDName,_,Options) , 
%    memberchk(one,Options).

is_unary_relation(AlloyTerm) :- 
    get_type_and_arity_from_alloy_term(AlloyTerm,_Type,Arity) , 
    Arity == 1.

is_binary_relation(AlloyTerm) :- 
    get_type_and_arity_from_alloy_term(AlloyTerm,_Type,Arity) , 
    Arity == 2.

get_type_and_arity_from_alloy_term(AlloyTerm,Type,Arity) :- 
    AlloyTerm =.. [_|Args] , 
    member(type(Type,Arity),Args) , !.

atom_or_identifier_term(ID,ID) :- atom(ID).
atom_or_identifier_term(identifier(IDName,_,_),IDName).
%%%

% Join a list of untyped B ASTs using either conjunct/3 or disjunct/3.
join_predicates(Op,TArgList,TBinaryP) :- 
    alloy_to_b_operator(Op,BOp) , 
    (BOp = conjunct ; BOp = disjunct) , 
    join_predicates_aux(BOp,TArgList,TBinaryP).

join_predicates_aux(_,[],truth(none)).
join_predicates_aux(BOp,[H|T],Conjoined) :- 
    join_predicates_aux(BOp,T,H,Conjoined).

join_predicates_aux(_,[],Acc,Acc).
join_predicates_aux(BOp,[H|T],Acc,Conjoined) :- 
    NewAcc =.. [BOp,none,H,Acc] , 
    join_predicates_aux(BOp,T,NewAcc,Conjoined).

% Type safe atom concat.
atom_concat_safe(A,B,C) :- 
    atom(A) , atom(B) , 
    atom_concat(A,B,C).
