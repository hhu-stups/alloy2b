:- module(alloy2b_tests,[run_tests/0]).

:- use_module(alloy2b).
:- use_module(library(plunit)).

:- begin_tests(full_machine_translation).

test(cards,[]) :- 
    translate_model(alloy_model(facts([]),assertions([]),commands([]),functions([predicate('this'/'ThreeOfAKind',['hand'],[field('hand',setof('this'/'Card',type(['this'/'Card']),pos(26,3)),pos(20,3))],and([equal(card(join('hand','suit',type(['this'/'Suit']),pos(8,4)),type(['Int']),pos(3,4)),integer(1,pos(14,4)),type([]),pos(13,4)), equal(card('hand',type(['Int']),pos(20,4)),integer(3,pos(26,4)),type([]),pos(25,4))],pos(16,4)),pos(1,3))]),signatures([signature('this'/'Card',[field('suit',oneof('this'/'Suit',type(['this'/'Suit']),pos(16,1)),pos(11,1))],[],[],pos(5,1)),signature('this'/'Suit',[],[],[],pos(5,2))])),Translated) , ! , 
    Translated == machine(generated(none,abstract_machine(none,machine(none),machine_header(none,alloytranslation,[]),definitions(none,[predicate_definition(none,this/'ThreeOfAKind',[identifier(none,hand)],conjunct(none,conjunct(none,equal(none,card(none,identifier(none,hand)),integer(none,3)),equal(none,card(none,join(none,identifier(none,hand),identifier(none,suit))),integer(none,1))),member(none,identifier(none,hand),identifier(none,'Card'))))]),sets(none,[identifier(none,this/'Suit'),identifier(none,this/'Card')]),properties(none,[conjunct(none,member(none,identifier(none,suit),identifier(none,'Suit')),equal(none,card(none,identifier(none,suit)),integer(none,1)))]),constants(none,[]),assertions(none,[]),operations(none,[])))).

:- end_tests(full_machine_translation).

:- begin_tests(single_expr_translation).

% fields
test(field_some_of,[]) :- 
    alloy2b:translate_field_aux(field('hand',setof('this'/'Card',type(['this'/'Card']),pos(26,3)),pos(20,3)),Translated) , ! , 
    Translated == member(none,identifier(none,hand),identifier(none,'Card')).

test(field_one_of,[]) :- 
    alloy2b:translate_field_aux(field('suit',oneof('this'/'Suit',type(['this'/'Suit']),pos(16,1)),pos(11,1)),Translated) , ! , 
    Translated == conjunct(none,member(none,identifier(none,suit),identifier(none,'Suit')),equal(none,card(none,identifier(none,suit)),integer(none,1))).

test(field_some_of,[]) :- 
    alloy2b:translate_field_aux(field('suit',someof('this'/'Suit',type(['this'/'Suit']),pos(16,1)),pos(11,1)),Translated) , ! , 
    Translated == conjunct(none,member(none,identifier(none,suit),identifier(none,'Suit')),greater(none,card(none,identifier(none,suit)),integer(none,0))).

test(field_lone_of,[]) :- 
    alloy2b:translate_field_aux(field('suit',loneof('this'/'Suit',type(['this'/'Suit']),pos(16,1)),pos(11,1)),Translated) , ! , 
    Translated == conjunct(none,member(none,identifier(none,suit),identifier(none,'Suit')),less_equal(none,card(none,identifier(none,suit)),integer(none,1))).

test(expr_and,[]) :- 
    alloy2b:translate_e_p(and([equal(card(join('hand','suit',type(['this'/'Suit']),pos(8,4)),type(['Int']),pos(3,4)),integer(1,pos(14,4)),type([]),pos(13,4)), equal(card('hand',type(['Int']),pos(20,4)),integer(3,pos(26,4)),type([]),pos(25,4))],pos(16,4)),Translated) , ! , 
    Translated == conjunct(none,equal(none,card(none,identifier(none,hand)),integer(none,3)),equal(none,card(none,join(none,identifier(none,hand),identifier(none,suit))),integer(none,1))).

:- end_tests(single_expr_translation).
