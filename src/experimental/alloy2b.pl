:- module(alloy2b,[translate_model/2]).

:- use_module(library(lists)).

:- dynamic singleton_set/1, total_function/1, ordered_signature/1, command_counter/1.
:- volatile singleton_set/1, total_function/1, ordered_signature/1, command_counter/1.

command_counter(0).

% An automated translation from Alloy to classical B.
% The Alloy abstract syntax tree is translated to an untyped B AST as supported by ProB.
% Afterwards, the untyped AST can be typechecked and loaded by ProB.
% The used positions are from the Alloy parser and thus refer to the Alloy model.

translate_model(alloy_model(facts(Facts),assertions(Assertions),commands(Commands),functions(Functions),signatures(Signatures)),BAst) :- 
    % singleton sets are asserted at runtime using singleton_set/1
    % accumulate all translations, afterwards we build the untyped machine ast
    empty_machine_acc(MAcc) , 
    map_translate(signature,MAcc,Signatures,MAcc1) , 
    map_translate(assertion,MAcc1,Assertions,MAcc2) , 
    map_translate(command,MAcc2,Commands,MAcc3) , 
    map_translate(function,MAcc3,Functions,MAcc4) , 
    map_translate(fact,MAcc4,Facts,MAcc5) , ! , 
    build_machine_ast(MAcc5,BAst) , 
    retract_state.

% Map the translation over a list and accumulate the results. 
% Type is one of signature, assertion, command, function, field or fact.
map_translate(_,MAcc,[],MAcc).
map_translate(Type,MAcc,[Part|T],Res) :- 
    map_translate_aux(Type,MAcc,Part,NewMAcc,TranslationCall) , 
    call(TranslationCall) , 
    map_translate(Type,NewMAcc,T,Res).

map_translate_aux(Type,MAcc,Part,NewMAcc,TranslationCall) :- 
    compound(Type) , 
    Type =.. [CompoundFunctor|Args] , 
    atom_concat_safe(translate_,CompoundFunctor,Functor) , 
    append(Args,[MAcc,Part,NewMAcc],NewArgs) , 
    TranslationCall =.. [Functor|NewArgs].
map_translate_aux(Type,MAcc,Part,NewMAcc,TranslationCall) :- 
    atom_concat_safe(translate_,Type,Functor) , 
    TranslationCall =.. [Functor,MAcc,Part,NewMAcc].

% signature
translate_signature(MAcc,signature(Name,Fields,Facts,Options,pos(Col,Row)),NewMAcc) :- 
    % assert signatures for singleton checks
    assert_singleton_set(Options,Name) , 
    extend_machine_acc(signatures,MAcc,[Name],MAcc1) , 
    translate_signature_aux(Name,Options,pos(Col,Row),MAcc1,MAcc2) ,  
    translate_signature_fields(MAcc2,Name,Fields,MAcc3) , 
    translate_e_p(Name,TName) , 
    map_translate(signature_fact(TName),MAcc3,Facts,NewMAcc).

translate_signature_aux(Name,Options,Pos,MAcc,NewMAcc) :- 
    % ordered signatures are defined as distinct sets of integer when translating a command
    member(ordered,Options) , ! , 
    asserta(ordered_signature(Name)) , 
    define_ordered_signature_functions(Pos,MAcc,Name,MAcc1) , 
    extend_machine_acc(signatures,MAcc1,[Name],MAcc2) , 
    extend_machine_acc(properties,MAcc2,[member(none,identifier(none,Name),pow_subset(none,integer_set(none)))],MAcc3) , 
    define_sig_as_set_or_constant_aux(constants,MAcc3,Name,Options,Pos,NewMAcc).
translate_signature_aux(Name,Options,Pos,MAcc,NewMAcc) :- 
    define_sig_as_set_or_constant(MAcc,Name,Options,Pos,NewMAcc).

define_ordered_signature_functions(pos(Col,Row),MAcc,Name,NewMAcc) :- 
    TPos = pos(0,0,Row,Col,0,0) , 
    TIDX = identifier(TPos,x) , 
    TIDS = identifier(TPos,s) ,
    translate_e_p(Name,TName) ,  
    MemberX = member(TPos,TIDX,TName) , 
    % next_Sig(s)  == {x|x=s+1 & x:Sig}
    atom_concat(next_,Name,NextName) , 
    NextBody = conjunct(TPos,equal(TPos,TIDX,add(TPos,TIDS,integer(TPos,1))),MemberX) , 
    extend_machine_acc(definitions,MAcc,expression_definition(TPos,NextName,[TIDS],comprehension_set(TPos,[TIDX],NextBody)),MAcc1) , 
    % nexts_Sig(s) == {x|x>s & x:Sig}
    atom_concat(nexts_,Name,NextsName) , 
    NextsBody = conjunct(TPos,greater(TPos,TIDX,TIDS),MemberX) , 
    extend_machine_acc(definitions,MAcc1,expression_definition(TPos,NextsName,[TIDS],comprehension_set(TPos,[TIDX],NextsBody)),MAcc2) , 
    atom_concat(prev_,Name,PrevName) , 
    % prev_Sig(s)  == {x|x=s-1 & x:Sig}
    PrevBody = conjunct(TPos,equal(TPos,TIDX,minus(TPos,TIDS,integer(TPos,1))),MemberX) , 
    extend_machine_acc(definitions,MAcc2,expression_definition(TPos,PrevName,[TIDS],comprehension_set(TPos,[TIDX],PrevBody)),MAcc3) , 
    % prevs_Sig(s) == {x|x<s & x:Sig}
    atom_concat(prevs_,Name,PrevsName) , 
    PrevsBody = conjunct(TPos,less(TPos,TIDX,TIDS),MemberX) , 
    extend_machine_acc(definitions,MAcc3,expression_definition(TPos,PrevsName,[TIDS],comprehension_set(TPos,[TIDX],PrevsBody)),NewMAcc).

% list of fields from a specific signature
translate_signature_fields(MAcc,SignatureName,Fields,NewMAcc) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    translate_signature_fields_aux(MAcc,TSignatureName,Fields,NewMAcc).

translate_signature_fields_aux(MAcc,_,[],MAcc).
translate_signature_fields_aux(MAcc,TSignatureName,[Field|T],NewMAcc) :- 
    translate_signature_field(TSignatureName,MAcc,Field,MAcc1) , 
    translate_signature_fields_aux(MAcc1,TSignatureName,T,NewMAcc).

% field
translate_signature_field(TSignatureName,MAcc,Field,NewMAcc) :- 
    translate_signature_field_aux(TSignatureName,MAcc,Field,TField,MAcc1) , 
    extend_machine_acc(properties,MAcc1,TField,NewMAcc).

translate_signature_field_aux(TSignatureName,MAcc,field(Name,Expr,type(_Type,_Arity),pos(Col,Row)),TField,NewMAcc) :- 
    translate_e_p(Name,TName) , 
    extend_machine_acc(constants,MAcc,TName,NewMAcc) , 
    field_decl_special_cases(pos(0,0,Row,Col,0,0),TSignatureName,Expr,TName,TField) , !.

translate_function_field(field(Name,Expr,type(_Type,_Arity),pos(Col,Row)),TField,Name) :- 
    assert_singleton_set(Name) , 
    translate_e_p(Name,TName) , 
    fun_or_pred_decl_special_cases(pos(0,0,Row,Col,0,0),Expr,TName,TField) , !.

% fact
translate_fact(MAcc,fact(Expr,_Pos),NewMAcc) :- 
    translate_e_p(Expr,TExpr) , 
    extend_machine_acc(properties,MAcc,TExpr,NewMAcc).

% Signature facts: Identifiers may refer to the signature and are joined with 'this'.
% We then need a universal quantification using 'this'.
translate_signature_fact(TSignatureName,MAcc,Expr,NewMAcc) :- 
    translate_e_p(Expr,TExpr) , 
    alloy_expr_contains_join_with_this(Expr) , 
    ThisID = identifier(none,'this') , 
    TImplication = implication(none,subset(none,set_extension(none,[ThisID]),TSignatureName),TExpr) , 
    extend_machine_acc(properties,MAcc,forall(none,[ThisID],TImplication),NewMAcc).
translate_signature_fact(_TSignatureName,MAcc,Expr,NewMAcc) :- 
    translate_e_p(Expr,TExpr) , 
    extend_machine_acc(properties,MAcc,TExpr,NewMAcc).

alloy_expr_contains_join_with_this(AndOr) :- 
    AndOr =.. [Functor,ListOfAsts,_Pos] , 
    member(Functor,[and,or]) , 
    findall(Ast,(member(Ast,ListOfAsts) , alloy_expr_contains_join_with_this(Ast)),Temp) , 
    length(Temp,LTemp) , ! , LTemp > 0.
alloy_expr_contains_join_with_this(join(Arg1,Arg2,_Type,_Pos)) :- 
    Arg1 = identifier('this',_,_) ; Arg2 = identifier('this',_,_).
alloy_expr_contains_join_with_this(Expr) :- 
    Expr =.. [_Functor,Arg1,Arg2,_Type,_Pos] , 
    (alloy_expr_contains_join_with_this(Arg1) ;
     alloy_expr_contains_join_with_this(Arg2)).
alloy_expr_contains_join_with_this(Expr) :- 
    Expr =.. [_Functor,Arg,_Type,_Pos] , 
    alloy_expr_contains_join_with_this(Arg).

% function or predicate
translate_function(MAcc,FunctionOrPredicate,NewMAcc) :- 
    FunctionOrPredicate =.. [Functor,Name,Params,Decls,Body,pos(Col,Row)] , 
    alloy_to_b_operator(Functor,BFunctor) , 
    maplist(translate_e_p,Params,TParams) , 
    maplist(translate_function_field,Decls,TDecls,SingletonSetNames) , 
    translate_e_p(Body,TBody) , 
    append(TDecls,[TBody],BehaviorList) , 
    join_predicates_aux(conjunct,BehaviorList,Behavior) , 
    UAst =.. [BFunctor,pos(0,0,Row,Col,0,0),Name,TParams,Behavior] , 
    extend_machine_acc(definitions,MAcc,UAst,NewMAcc) , 
    maplist(retract_singleton_set,SingletonSetNames).

% assertion
translate_assertion(MAcc,Assertion,NewMAcc) :- 
    translate_fact(MAcc,Assertion,NewMAcc).

% check and run command
translate_command(MAcc,Command,NewMAcc) :- 
    Command =.. [Functor,Body,GlobalScope,ExactScopes,BitWidth,pos(Col,Row)] , 
    (Functor = check ; Functor = run) , 
    translate_e_p(Body,TBody) , 
    % we need all signature names to define the global scope
    get_signature_names_from_machine_acc(MAcc,SignatureNames) , 
    translate_scopes(SignatureNames,GlobalScope,ExactScopes,BitWidth,TScopesPred) , 
    TPos = pos(0,0,Row,Col,0,0) , 
    Precondition = conjunct(TPos,TScopesPred,TBody) , 
    get_command_counter_atom(CommandCounter) , 
    atom_concat(Functor,CommandCounter,OperationName) , 
    Operation = operation(TPos,identifier(TPos,OperationName),[],[],precondition(TPos,Precondition,skip(none))) , 
    extend_machine_acc(operations,MAcc,Operation,NewMAcc).

% global scope, exact scopes and bitwidth
% We do not need to set the bitwidth since we have real integers in B. 
% Note: If only one command is defined in the Alloy model we could set min and max int of ProB in the definitions.
translate_scopes(SignatureNames,global_scope(GlobalScope),exact_scopes(ExactScopes),_BitWidth,conjunct(none,TExactScopes,TGlobalScopes)) :- 
    % translate exact scopes first
    translate_exact_scopes(0,ExactScopes,ExactSignatureNames,TExactScopes,OrderedSignatureCounter) ,  
    % if present, define the global scope for the remaining signatures
    translate_global_scope(OrderedSignatureCounter,GlobalScope,SignatureNames,ExactSignatureNames,TGlobalScopes,_).

% no exact scopes defined
translate_exact_scopes(OrderedSignatureCounter,[],_,truth(none),OrderedSignatureCounter) :- !.
translate_exact_scopes(OrderedSignatureCounter,ExactScopes,ExactSignatureNames,TExactScopes,NewOrderedSignatureCounter) :- 
    translate_exact_scopes_aux(OrderedSignatureCounter,ExactScopes,[],ExactSignatureNames,[],TempTExactScopes,NewOrderedSignatureCounter) , 
    join_predicates(conjunct,TempTExactScopes,TExactScopes).

translate_exact_scopes_aux(OrderedSignatureCounter,[],NamesAcc,NamesAcc,TScopeAcc,TScopeAcc,OrderedSignatureCounter).
translate_exact_scopes_aux(OrderedSignatureCounter,[(SignatureName,Scope)|T],NameAcc,ExactSignatureNames,TScopeAcc,TempTExactScopes,NewOrderedSignatureCounter) :- 
    is_ordered_signature(SignatureName) , ! , 
    define_ordered_signature_as_integer_set(OrderedSignatureCounter,SignatureName,Scope,TScope,TNewOrderedSignatureCounter) , 
    translate_exact_scopes_aux(TNewOrderedSignatureCounter,T,[SignatureName|NameAcc],ExactSignatureNames,[TScope|TScopeAcc],TempTExactScopes,NewOrderedSignatureCounter).
translate_exact_scopes_aux(OrderedSignatureCounter,[(SignatureName,Scope)|T],NameAcc,ExactSignatureNames,TScopeAcc,TempTExactScopes,NewOrderedSignatureCounter) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    TScope = equal(none,card(none,TSignatureName),integer(none,Scope)) , 
    translate_exact_scopes_aux(OrderedSignatureCounter,T,[SignatureName|NameAcc],ExactSignatureNames,[TScope|TScopeAcc],TempTExactScopes,NewOrderedSignatureCounter).

define_ordered_signatures_as_integer_set(OrderedSignatureCounter,[],_,[],OrderedSignatureCounter).
define_ordered_signatures_as_integer_set(OrderedSignatureCounter,[TSignatureName|T],Scope,[TScope|TScopes],NewOrderedSignatureCounter) :- 
    define_ordered_signature_as_integer_set(OrderedSignatureCounter,TSignatureName,Scope,TScope,TNewOrderedSignatureCounter) , 
    define_ordered_signatures_as_integer_set(TNewOrderedSignatureCounter,T,Scope,TScopes,NewOrderedSignatureCounter).

define_ordered_signature_as_integer_set(OrderedSignatureCounter,TSignatureName,Scope,TScope,NewOrderedSignatureCounter) :- 
    NewOrderedSignatureCounter is OrderedSignatureCounter + Scope , 
    NewOrderedSignatureCounter1 is NewOrderedSignatureCounter - 1 , 
    TScope = equal(none,identifier(none,TSignatureName),interval(none,integer(none,OrderedSignatureCounter),integer(none,NewOrderedSignatureCounter1))).

translate_global_scope(OrderedSignatureCounter,-1,_,_,truth(none),OrderedSignatureCounter) :- !. % no global scope defined
translate_global_scope(OrderedSignatureCounter,GlobalScope,SignatureNames,ExactSignatureNames,TGlobalScopes,NewOrderedSignatureCounter) :- 
    findall(SignatureName,(member(SignatureName,SignatureNames) , \+member(SignatureName,ExactSignatureNames) , is_ordered_signature(SignatureName)),OrderedSignatureNames) , 
    findall(SignatureName,(member(SignatureName,SignatureNames) , \+member(SignatureName,ExactSignatureNames) , \+is_ordered_signature(SignatureName)),RestSignatureNames) , 
    translate_global_scope_aux(RestSignatureNames,GlobalScope,[],TempTGlobalScopes) , 
    join_predicates(conjunct,TempTGlobalScopes,TTGlobalScopes) , 
    define_ordered_signatures_as_integer_set(OrderedSignatureCounter,OrderedSignatureNames,GlobalScope,TOrderedScopes,NewOrderedSignatureCounter) , 
    join_predicates(conjunct,[TTGlobalScopes|TOrderedScopes],TGlobalScopes).

translate_global_scope_aux([],_,Acc,Acc).
translate_global_scope_aux([SignatureName|T],GlobalScope,Acc,TGlobalScopes) :- 
    translate_e_p(SignatureName,TSignatureName) , 
    TScope = less_equal(none,card(none,TSignatureName),integer(none,GlobalScope)) , 
    translate_global_scope_aux(T,GlobalScope,[TScope|Acc],TGlobalScopes).

% signature in (subset)
define_sig_as_set_or_constant(MAcc,Name,Options,pos(Col,Row),NewMAcc) :- 
    memberchk(subset(Parents),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,pos(Col,Row),MAcc1) , 
    % TODO: consider several parents -> we need the universe type
    Parents = [Parent|_] , 
    translate_e_p(Name,TName) , 
    translate_e_p(Parent,TParent) , 
    get_constant_definition_operator_from_sig_options(Options,BOp) , 
    TNode =.. [BOp,pos(0,0,Row,Col,0,0),TName,TParent] , 
    extend_machine_acc(properties,MAcc1,TNode,NewMAcc).
% signature extends
define_sig_as_set_or_constant(MAcc,Name,Options,pos(Col,Row),NewMAcc) :- 
    member(subsig(Parent),Options) , ! , 
    define_sig_as_set_or_constant_aux(constants,MAcc,Name,Options,pos(Col,Row),MAcc1) ,
    translate_e_p(Name,TName) , 
    translate_e_p(Parent,TParent) , 
    get_constant_definition_operator_from_sig_options(Options,BOp) , 
    TNode =.. [BOp,pos(0,0,Row,Col,0,0),TName,TParent] , 
    extend_machine_acc(properties,MAcc1,TNode,NewMAcc).
% default signature
define_sig_as_set_or_constant(MAcc,Name,Options,Pos,NewMAcc) :- 
    define_sig_as_set_or_constant_aux(sets,MAcc,Name,Options,Pos,NewMAcc).

define_sig_as_set_or_constant_aux(sets,MAcc,Name,_Options,pos(Col,Row),NewMAcc) :- 
    % use deferred_set/2 instead of identifier/2 like for constants
    extend_machine_acc(sets,MAcc,deferred_set(pos(0,0,Row,Col,0,0),Name),NewMAcc).
define_sig_as_set_or_constant_aux(constants,MAcc,Name,_Options,pos(Col,Row),NewMAcc) :- 
    extend_machine_acc(constants,MAcc,identifier(pos(0,0,Row,Col,0,0),Name),NewMAcc).

get_constant_definition_operator_from_sig_options(Options,member) :- 
    member(one,Options) , !.
get_constant_definition_operator_from_sig_options(_,subset).

% translate expression or predicate
translate_e_p(A,TA) :- 
    % check quantifiers first
    translate_quantifier_e(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_unary_e_p(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_binary_e_p(A,TA) , !.
translate_e_p(A,TA) :- 
    translate_if_then_else(A,TA).
translate_e_p(A,_) :- 
    format("Translation failed for ~w.~n",[A]).

translate_if_then_else(if_then_else(ConditionPred,TruthExpr,FalsityExpr,_Type,pos(Col,Row)),TIfThenElse) :- 
    translate_e_p(ConditionPred,TConditionPred) , 
    translate_e_p(TruthExpr,TTruthExpr) , 
    translate_e_p(FalsityExpr,TFalsityExpr) , 
    TIfThenElse = conjunct(pos(0,0,Row,Col,0,0),
        implication(pos(0,0,Row,Col,0,0),TConditionPred,TTruthExpr),
        implication(pos(0,0,Row,Col,0,0),negation(pos(0,0,Row,Col,0,0),TConditionPred),TFalsityExpr)).

% quantifiers
translate_quantifier_e(Quantifier,TQuantifier) :- 
    Quantifier =.. [Functor,Params,Fields,Body,_Type,pos(Col,Row)] , 
    member(Functor,[all,no,some,one,lone]) , 
    maplist(translate_e_p,Params,TParams) , 
    maplist(assert_singleton_set,Params) , 
    maplist(translate_function_field,Fields,TFieldsList,SingletonSetNames) , 
    join_predicates_aux(conjunct,TFieldsList,TFields) , 
    translate_e_p(Body,TBody) , 
    maplist(retract_singleton_set,Params) , 
    translate_quantifier_e_aux(pos(0,0,Row,Col,0,0),Functor,TParams,TFields,TBody,TQuantifier) , 
    maplist(retract_singleton_set,SingletonSetNames).

translate_quantifier_e_aux(Pos,all,TParams,TFields,TBody,forall(Pos,TParams,implication(none,TFields,TBody))).
translate_quantifier_e_aux(Pos,no,TParams,TFields,TBody,negation(Pos,exists(none,TParams,conjunct(none,TFields,TBody)))).
translate_quantifier_e_aux(Pos,some,TParams,TFields,TBody,exists(Pos,TParams,conjunct(none,TFields,TBody))).
translate_quantifier_e_aux(Pos,one,TParams,TFields,TBody,equal(Pos,card(none,comprehension_set(none,TParams,conjunct(none,TFields,TBody))),integer(none,1))).
translate_quantifier_e_aux(Pos,lone,TParams,TFields,TBody,less_equal(Pos,card(none,comprehension_set(none,TParams,conjunct(none,TFields,TBody))),integer(none,1))).

% unary expressions and predicates
translate_unary_e_p(Int,integer(none,Int)) :- integer(Int) , !.
translate_unary_e_p(identifier(ID,_Type,pos(Col,Row)),set_extension(pos(0,0,Row,Col,0,0),[identifier(none,ID)])) :- 
    is_singleton_set(ID) , !.
translate_unary_e_p(ID,set_extension(none,[identifier(none,ID)])) :- 
    is_singleton_set(ID) , !.
translate_unary_e_p(identifier('Int_',_Type,pos(Col,Row)),integer_set(pos(0,0,Row,Col,0,0))) :- !.
translate_unary_e_p('Int_',integer_set(none)) :- !.
translate_unary_e_p(identifier(ID,_Type,pos(Col,Row)),identifier(pos(0,0,Row,Col,0,0),ID)) :- !.
translate_unary_e_p(ID,identifier(none,ID)) :- atom(ID) , !.
translate_unary_e_p(integer(A,pos(Col,Row)),integer(pos(0,0,Row,Col,0,0),A)) :- !.
translate_unary_e_p(boolean(true,pos(Col,Row)),boolean_true(pos(0,0,Row,Col,0,0))) :- !.
translate_unary_e_p(boolean(false,pos(Col,Row)),boolean_false(pos(0,0,Row,Col,0,0))) :- !.
translate_unary_e_p(UnaryP,TUnaryP) :- 
    % and/or defines a list of ast nodes
    UnaryP =.. [Op,ArgList|_] , 
    memberchk(Op,[and,or]) , 
    is_list(ArgList) , ! , 
    maplist(translate_e_p,ArgList,TArgList) , 
    join_predicates(Op,TArgList,TUnaryP).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    UnaryP =.. [Op,Arg,_Type,pos(Col,Row)] , 
    member(Op,[no,one,some,lone]) , ! ,
    translate_e_p(Arg,TArg) ,  
    translate_quantified_e(pos(0,0,Row,Col,0,0),Op,TArg,TUnaryP).
translate_unary_e_p(UnaryP,TUnaryP) :- 
    UnaryP =.. [Op,Arg,_Type,pos(Col,Row)] , 
    translate_e_p(Arg,TArg) , 
    alloy_to_b_operator(Op,BOp) , 
    TUnaryP =.. [BOp,pos(0,0,Row,Col,0,0),TArg].

integer_function_to_b('integer_\'minus_',minus).
integer_function_to_b('integer_\'plus_',add).
integer_function_to_b('integer_\'div_',div).
integer_function_to_b('integer_\'mul_',multiplication).
integer_function_to_b('integer_\'rem_',modulo).

ordering_function_to_b(type([SignatureName|_],_),OrderingFunctionName,FunctionName) :- 
    atom_concat(Prefix,TempFunctionName,OrderingFunctionName) , 
    (Prefix = 'ordering_\'' ; Prefix = SignatureName) , 
    ((TempFunctionName = first_ , FunctionName = min) ; (TempFunctionName = last_ , FunctionName = max)) , !.
ordering_function_to_b(type([SignatureName|_],_),OrderingFunctionName,FunctionName) :- 
    atom_concat('ordering_\'',TempFunctionName,OrderingFunctionName) , 
    atom_concat(TempFunctionName,SignatureName,FunctionName).

% binary expressions and predicates
translate_binary_e_p(join(Arg1,Arg2,_Type,pos(Col,Row)),TBinaryJoin) :- ! , 
    translate_join(pos(0,0,Row,Col,0,0),Arg1,Arg2,TBinaryJoin).
translate_binary_e_p(Call,TCall) :- 
    Call =.. [Functor,Name,Params,_Type,pos(Col,Row)] , 
    (Functor = pred_call ; Functor = fun_call ) , ! , 
    translate_function_call(Name,Params,_Type,pos(Col,Row),TCall).
translate_binary_e_p(Binary,TBinary) :- 
    Binary =.. [Op,Arg1,Arg2,_Type,pos(Col,Row)] , 
    translate_e_p(Arg1,TArg1) , 
    translate_e_p(Arg2,TArg2) , 
    alloy_to_b_operator(Op,BOp) , 
    TBinary =.. [BOp,pos(0,0,Row,Col,0,0),TArg1,TArg2].

translate_function_call(Name,_Params,type([Type|_],_Arity),pos(Col,Row),TCall) :- 
    % function calls like stakes/last
    % first and last from util/ordering are translated to min and max since we use sets of integer
    ordering_function_to_b(type([Type|_],_Arity),Name,BFunctor) , 
    (BFunctor = max ; BFunctor = min) , ! , 
    % the types of ordering calls have been preprocessed in Kotlin Alloy2Prolog, i.e., 
    % n-ary types have been flattened to the ordered signature name
    Type = SignatureName , 
    translate_e_p(SignatureName,TSignatureName) , 
    TCall =.. [BFunctor,pos(0,0,Row,Col,0,0),TSignatureName].
translate_function_call(Name,Params,Type,pos(Col,Row),TCall) :- 
    % utility functions from integers or ordering
    (ordering_function_to_b(Type,Name,BOp) ; integer_function_to_b(Name,BOp)) , ! , 
    maplist(translate_e_p,Params,TParams) , 
    TCall =.. [definition,pos(0,0,Row,Col,0,0),BOp,TParams].
translate_function_call(Name,Params,_Type,pos(Col,Row),TCall) :- 
    % predicate and function call
    maplist(translate_e_p,Params,TParams) , 
    TCall =.. [definition,pos(0,0,Row,Col,0,0),Name,TParams].

% Translation of the dot join operator has several special cases depending on the arity of the arguments.
translate_join(Pos,Arg1,Arg2,TBinaryJoin) :- 
    translate_e_p(Arg1,TArg1) , 
    translate_e_p(Arg2,TArg2) , 
    translate_join_aux(Pos,Arg1,Arg2,TArg1,TArg2,TBinaryJoin).

% univ._
translate_join_aux(Pos,identifier('univ_',type(['univ_'],1),_),_Arg2,_TArg1,TArg2,ran(Pos,TArg2)).
% _.univ
translate_join_aux(Pos,_Arg1,identifier('univ_',type(['univ_'],1),_),TArg1,_TArg2,dom(Pos,TArg1)).
% [first,last]._
translate_join_aux(_Pos,Arg1,_Arg2,TArg1,_TArg2,TArg1) :- 
    Arg1 = fun_call(OrderingFunction,_Params,Type,_Pos1) , 
    ordering_function_to_b(Type,OrderingFunction,_BFunction) , !.
% _.[next,nexts,prev,prevs]
translate_join_aux(_Pos,Arg1,Arg2,_TArg1,_TArg2,TJoin) :- 
    % set rhs as the parameter of lhs function call
    Arg2 = fun_call(OrderingFunction,_Params,Type,Pos1) , 
    ordering_function_to_b(Type,OrderingFunction,_BFunction) , ! , 
    translate_e_p(fun_call(OrderingFunction,[Arg1],Type,Pos1),TJoin).
% unary._
translate_join_aux(Pos,Arg1,_Arg2,TArg1,TArg2,function(Pos,TArg2,[TTArg1])) :- 
    % function call if rhs is a total function and lhs a unary relation
    clear_pos_from_untyped_b_ast(TArg2,TArg2Clean) , 
    is_unary_relation(Arg1) , is_total_function(TArg2Clean) , ! , 
    remove_singleton_set(TArg1,TTArg1).
translate_join_aux(Pos,Arg1,_Arg2,TArg1,TArg2,image(Pos,TArg2,TArg1)) :- 
    is_unary_relation(Arg1).
% binary.unary
translate_join_aux(Pos,Arg1,Arg2,TArg1,TArg2,function(Pos,reverse(Pos,TArg1),[TTArg2])) :- 
    % function call if lhs is a total function and rhs a unary relation
    clear_pos_from_untyped_b_ast(TArg1,TArg1Clean) , 
    is_binary_relation(Arg1) , is_unary_relation(Arg2) , is_total_function(TArg1Clean) , ! , 
    remove_singleton_set(TArg2,TTArg2).
translate_join_aux(Pos,Arg1,Arg2,TArg1,TArg2,image(Pos,reverse(Pos,TArg1),TArg2)) :- 
    is_binary_relation(Arg1) , is_unary_relation(Arg2).
% binary.binary
translate_join_aux(Pos,Arg1,Arg2,TArg1,TArg2,parallel_product(Pos,TArg1,TArg2)) :-
    is_binary_relation(Arg1) , is_binary_relation(Arg2).
translate_join_aux(Pos,Arg1,Arg2,_TArg1,_TArg2,empty_set(Pos)) :- 
    format("Join not supported this way:~nLeft: ~w~nRight: ~w~n",[Arg1,Arg2]).

% Unary quantified expressions: no, one, some, lone
translate_quantified_e(Pos,no,TArg,equal(Pos,TArg,empty_set(none))).
translate_quantified_e(Pos,one,TArg,equal(Pos,card(none,TArg),integer(none,1))).
translate_quantified_e(Pos,some,TArg,greater(Pos,card(none,TArg),integer(none,0))).
translate_quantified_e(Pos,lone,TArg,less_equal(Pos,card(none,TArg),integer(none,1))).

% Field declarations have several special cases depending on the keywords set, one, some or lone.
field_decl_special_cases(Pos,TSignatureName,DeclTerm,TFieldID,TField) :- 
    DeclTerm =.. [_,SetID|_] , 
    translate_e_p(SetID,TSetID) , 
    field_decl_special_cases_aux(Pos,DeclTerm,TSignatureName,TSetID,TFieldID,TField).

field_decl_special_cases_aux(Pos,setof(_,_,_),TSignatureName,TSetID,TFieldID,member(Pos,TFieldID,relations(none,TSignatureName,TSetID))).
field_decl_special_cases_aux(Pos,oneof(_,_,_),TSignatureName,TSetID,TFieldID,member(Pos,TFieldID,total_function(none,TSignatureName,TSetID))) :- 
    asserta(total_function(TFieldID)).
field_decl_special_cases_aux(Pos,loneof(_,_,_),TSignatureName,TSetID,TFieldID,member(Pos,TFieldID,partial_function(none,TSignatureName,TSetID))).
field_decl_special_cases_aux(Pos,Term,_,_,_,_) :- 
    format("Field declaration not implemented: ~w~nPosition ~w",[Term,Pos]).

% In function or predicate definitions one can use either set or one.
fun_or_pred_decl_special_cases(Pos,setof(Expr,_,_),TFieldID,subset(Pos,TFieldID,TExpr)) :- 
    translate_e_p(Expr,TExpr).
fun_or_pred_decl_special_cases(Pos,oneof(Expr,_,_),TFieldID,subset(Pos,TFieldID,TExpr)) :- 
    translate_e_p(Expr,TExpr).
fun_or_pred_decl_special_cases(_,Term,_,_) :- 
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
alloy_to_b_operator_aux(closure1,reflexive_closure).
alloy_to_b_operator_aux(cartesian,cartesian_product).
alloy_to_b_operator_aux(function,expression_definition).
alloy_to_b_operator_aux(predicate,predicate_definition).
alloy_to_b_operator_aux(Op,Op).

%%% 
% Accumulate the translated machine parts and signature names during the translation and build the machine AST afterwards.
% We may need the signature names later on if translating a global scope.
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

% Get the value of command_counter/1 as codes and assert the increased counter.
get_command_counter_atom(CommandCounterAtom) :- 
    command_counter(CommandCounter) , 
    number_codes(CommandCounter,CommandCounterCodes) , 
    atom_codes(CommandCounterAtom,CommandCounterCodes) , 
    retractall(command_counter(_)) , 
    NewCommandCounter is CommandCounter + 1 , 
    asserta(command_counter(NewCommandCounter)).
%%%

% Assert signatures for singleton checks.
assert_singleton_set(Options,Name) :- 
    member(one,Options) , ! , 
    assert_singleton_set(Name).
assert_singleton_set(_,_).

assert_singleton_set(Name) :- 
    (on_exception(_,singleton_set(Name),fail) ; 
     asserta(singleton_set(Name))).
assert_singleton_set(_).

% The singleton set annotation is local for a quantifier or parameters of a function definition.
retract_singleton_set(Name) :- 
    retractall(singleton_set(Name)).

retract_state :- 
    retractall(command_counter(_)) , 
    asserta(command_counter(0)) , 
    retractall(singleton_set(_)) , 
    retractall(ordered_signature(_)) , 
    retractall(total_function(_)).

is_singleton_set(IDName) :- 
    on_exception(_,singleton_set(IDName),fail).

is_total_function(IDName) :- 
    on_exception(_,total_function(IDName),fail).

is_ordered_signature(IDName) :- 
    on_exception(_,ordered_signature(IDName),fail).

is_unary_relation(AlloyTerm) :- 
    get_type_and_arity_from_alloy_term(AlloyTerm,_Type,Arity) , 
    Arity == 1.

is_binary_relation(AlloyTerm) :- 
    get_type_and_arity_from_alloy_term(AlloyTerm,_Type,Arity) , 
    Arity == 2.

% Only used for comparison checks between untyped ASTs. All output ASTs have to be ground.
clear_pos_from_untyped_b_ast(UntypedBAst,Clean) :- 
    UntypedBAst =.. [Functor,_Pos|Args] , 
    Clean =.. [Functor,_|Args].

remove_singleton_set(set_extension(_,[identifier(Pos,Name)]),identifier(Pos,Name)).
remove_singleton_set(AST,AST).

get_type_and_arity_from_alloy_term(AlloyTerm,Type,Arity) :- 
    AlloyTerm =.. [_|Args] , 
    member(type(Type,Arity),Args) , !.

atom_or_identifier_term(ID,ID) :- atom(ID).
atom_or_identifier_term(identifier(IDName,_,_),IDName).

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
