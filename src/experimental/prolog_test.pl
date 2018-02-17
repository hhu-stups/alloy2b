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
alloy_model(facts([]),assertions([fact(implication(greater(card(
join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(5,7)),type(Int,pos(2,7)),
"1",type(PrimitiveBoolean,pos(9,7)),
greater(card(cartesian(join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(23,7)),join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(29,7)),type(this/setX->this/setX->this/setX->this/setX,pos(26,7)),type(Int,pos(20,7)),"3",type(PrimitiveBoolean,pos(34,7)),type(PrimitiveBoolean,pos(12,7)),(1,6))]),
commands([check(and([not(implication(greater(card(join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(5,7)),type(Int,pos(2,7)),"1",type(PrimitiveBoolean,pos(9,7)),greater(card(cartesian(join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(23,7)),join("this/V","field (this/V <: SS)",type(this/setX->this/setX,pos(29,7)),type(this/setX->this/setX->this/setX->this/setX,pos(26,7)),type(Int,pos(20,7)),"3",type(PrimitiveBoolean,pos(34,7)),type(PrimitiveBoolean,pos(12,7)),type(PrimitiveBoolean,pos(1,6))],pos(1,6)),global_scope(-1),exact_scopes([]),bitwidth(7),pos(1,9)]),functions([]),
signatures([
signature("this/setX",[],[],[abstract],pos(14,2)),
signature("this/V",[field("SS",cartesian("this/setX","this/setX",type(this/setX->this/setX,pos(14,4))],[],[one],pos(9,3))])).
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
 
 % TO DO: once stabilised a little, generate (probably untyped raw) B AST
 
% facts for straightforward Alloy Op -> B Operator translations:
unop_p(not(A,_),A,'not').
binop_p(implication(A,B,_),A,B,' => ').
binop_p(and(A,B,_),A,B,' & ').
binop_p(or(A,B,_),A,B,' or ').
 
binop_p_e(greater(A,B,_),A,B,' > ').
binop_p_e(greater_equal(A,B,_),A,B,' >= ').
binop_p_e(less(A,B,_),A,B,' < ').
binop_p_e(less_equal(A,B,_),A,B,' <= ').
binop_p_e(equal(A,B,_),A,B,' = ').
binop_p_e(in(A,B,_),A,B,' <: ').
 
unop_e(card(A,_),A,'card').
unop_e(closure(A,_),A,'closure1').

binop_e(cartesian(A,B,_),A,B,' * ').
binop_e(plus(A,B,_),A,B,' \\/ ').
binop_e(minus(A,B,_),A,B,' \\ ').
binop_e(intersection(A,B,_),A,B,' /\\ ').
 
% print predicates (TO DO: rewrite using DCG to generate strings or generate B AST)
pp_p(Pred) :- binop_p(Pred,A,B,Op), print('('),pp_p(A), print(Op), pp_p(B), print(')').
pp_p(Pred) :- binop_p_e(Pred,A,B,Op), print('('),pp_e(A), print(Op), pp_e(B), print(')').
pp_p(Pred) :- unop_p(Pred,A,Op), print(Op), print('('),pp_p(A), print(')').
pp_p(no(A,_)) :- print('('), pp_e(A), print(' = {})').
pp_p(some(A,_)) :- print('('), pp_e(A), print(' /= {})').
 
 
 % print expressions
pp_e(Expr) :- unop_e(Expr,A,Op), print(Op), print('('),pp_e(A), print(')').
pp_e(E) :- binop_e(E,A,B,Op), print('('),pp_e(A), print(Op), pp_e(B), print(')').
pp_e(join(A,B,_)) :- unary_relation(A),!, % this should be provided by the type checker
    pp_e(B), print('['), pp_e(A), print(']').
pp_e(join(A,B,_)) :- binary_relation(A),unary_relation(B),!, % this should be provided by the type checker
    pp_e(A), print('~['), pp_e(B), print(']').
pp_e(join(A,B,_)) :- binary_relation(A),
    print('('),pp_e(A), print(' ; '), pp_e(B), print(')').
% TO DO: support n-ary
pp_e(Nr) :- number(Nr), print(Nr).
pp_e(ID) :- is_identifier(ID,PPID), print(PPID).

% print expression as type
pp_t(E) :- print('POW('), pp_e(E), print(')').

% utility predicates, need to be rewritten to use type information:
is_identifier(this/ID,ID) :- signature(this/ID,_,_,_,_).
is_identifier(ID,ID) :- is_field(_,ID,_).
 
is_field(Set,Name,Type) :- signature(Set,Fields,_,_,_),
     member(field(Name,Type),Fields).

unary_relation(A) :- signature(A,_,_,_,_).
binary_relation(_) :- fail.

singleton_set(S) :- signature(this/S,_,_,[one],_).

% quick and dirty version; does not insert separators, later we should construct AST nodes
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

