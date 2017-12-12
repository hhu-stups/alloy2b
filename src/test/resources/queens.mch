/*@ generated */
MACHINE alloytranslation
SETS
    Queens_
CONSTANTS
    row_, col_
DEFINITIONS
    nothreat_(q1_,q2_) == {q1_} <: Queens_ & {q2_} <: Queens_  &  row_[{q1_}] = row_[{q2_}] & col_[{q1_}] = col_[{q2_}] & row_(q1_) - row_(q2_) = col_(q2_) - col_(q1_) & row_(q1_) - row_(q2_) = col_(q1_) - col_(q2_) ;
    valid_ ==   !(q1_, q2_).({q1_} <: Queens_ & {q2_} <: Queens_ => {q1_} = {q2_} => nothreat_(q1_, q2_))
PROPERTIES
    card(Queens_) = 32 &
    /* from signature declaration */ !(this_).({this_} <: Queens_ => row_(this_) >= 0 & row_(this_) < card(Queens_) & col_(this_) >= 0 & col_(this_) < card(Queens_)) &
    row_ : Queens_ --> INTEGER &
    col_ : Queens_ --> INTEGER
ASSERTIONS
    valid_
END
