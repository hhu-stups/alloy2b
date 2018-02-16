% Experimental Prolog Alloy to B Translator
% Purpose: check if practical to implement in Prolog and to elicit mathematical rules

/*
Original Alloy model:

open util/integer
abstract sig setX { }
one sig V {
  SS:   setX -> setX
}
assert Bug {
 #(V.SS)>1 implies #(V.SS->V.SS) >3  // counter example with eg check Bug for 3 setX, 7 int
}
check Bug for 3 setX, 7 int // for 8 int Translation capacity exceeded
// With ForbidOverflow no counter example detected by Alloy.

Translation:
alloy_model(facts([]),assertions([fact(implication(greater(card(join(this/v,ss,pos(5,7)),pos(2,7)),1,pos(9,7)),greater(card(cartesian(join(this/v,ss,pos(23,7)),join(this/v,ss,pos(29,7)),pos(26,7)),pos(20,7)),3,pos(34,7)),pos(12,7)),(1,6))]),commands([check(and([not(implication(greater(card(join(this/v,ss,pos(5,7)),pos(2,7)),1,pos(9,7)),greater(card(cartesian(join(this/v,ss,pos(23,7)),join(this/v,ss,pos(29,7)),pos(26,7)),pos(20,7)),3,pos(34,7)),pos(12,7)),pos(1,6))],pos(1,6)),global_scope(-1),exact_scopes([]),bitwidth(7),pos(1,9)]),functions([]),signatures([signature(this/setx,[],[],[abstract],pos(14,2)),signature(this/v,[cartesian(this/setx,this/setx,pos(14,4))],[],[one],pos(9,3))])).

*/

% #(V.SS)>1 implies #(V.SS->V.SS) >3
assertion_fact(
 implication(
  greater(card(join(this/v,ss,pos(5,7)),pos(2,7)),1,pos(9,7)),
  greater(card(cartesian(join(this/v,ss,pos(23,7)),
                         join(this/v,ss,pos(29,7)),pos(26,7)),pos(20,7)),
          3,pos(34,7)),pos(12,7))).
 

% abstract sig setX { }
signature(this/setx,[],[],[abstract],pos(14,2)).
 
signature(this/v,[field(ss,cartesian(this/setx,this/setx,pos(14,4)))],[],[one],pos(9,3)).
 
 % ----------------
 % Translator
 % ----------------
 
 % print predicates
 pp_p(implication(A,B,_)) :- print('('),pp_p(A), print(' => '), pp_p(B), print(')').
 pp_p(and(A,B,_)) :- print('('),pp_p(A), print(' & '), pp_p(B), print(')').
 pp_p(greater(A,B,_)) :- print('('),pp_e(A), print(' > '), pp_e(B), print(')').
 pp_p(not(A,_)) :- print('not('),pp_p(A), print(')').
 
 % print expressions
 pp_e(card(A,_)) :- print('card('),pp_e(A), print(')').
 pp_e(join(A,B,_)) :- unary_relation(A),!, % this should be provided by the type checker
    pp_e(B), print('['), pp_e(A), print(']').
 pp_e(join(A,B,_)) :- binary_relation(A),unary_relation(B),!, % this should be provided by the type checker
    pp_e(A), print('~['), pp_e(B), print(']').
 pp_e(join(A,B,_)) :- binary_relation(A),
    print('('),pp_e(A), print(' ; '), pp_e(B), print(')').
% TO DO: support n-ary
 pp_e(Nr) :- number(Nr), print(Nr).
 pp_e(cartesian(A,B,_)) :- print('('),pp_e(A), print(' * '), pp_e(B), print(')').
 pp_e(ID) :- is_identifier(ID,PPID), print(PPID).

% print expression as type
pp_t(E) :- print('POW('), pp_e(E), print(')').

is_identifier(this/ID,ID) :- signature(this/ID,_,_,_,_).
is_identifier(ID,ID) :- is_field(_,ID,_).
 
is_field(Set,Name,Type) :- signature(Set,Fields,_,_,_),
     member(field(Name,Type),Fields).

unary_relation(A) :- signature(A,_,_,_,_).
binary_relation(_) :- fail.

singleton_set(S) :- signature(this/S,_,_,[one],_).

% Q&D version; does not insert separators, later we should construct AST nodes
pp_model :- print('MACHINE alloy'),nl,
    print('SETS '),nl,print(' '),
    signature(this/S,_,_,_,_), print(S), print(' '),fail.
pp_model :- nl,print('CONSTANTS '),nl,print('  '),
    is_field(_Set,Name,_Type), print(Name), print(' '),fail.
pp_model :- nl,print('PROPERTIES '),nl,print('  '),
    singleton_set(S), format(' card(~w)=1~n ',S),fail.
pp_model :-
    is_field(Set,Name,Type), format(' ~w : ',[Name]), pp_t(cartesian(Set,Type,pos)),nl,fail.
pp_model :- nl,print('ASSERTIONS'),nl,
   assertion_fact(A), pp_p(A),nl,fail.
pp_model :- print('END'),nl.

