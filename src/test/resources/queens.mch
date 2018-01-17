/*@ generated */
MACHINE alloytranslation
SETS
    Queens_
CONSTANTS
    row_Queens, col_Queens
DEFINITIONS
    nothreat_(q1_,q2_) == {q1_} <: Queens_ & {q2_} <: Queens_  &  (((not((row_Queens[{q1_}] = row_Queens[{q2_}])) & not((col_Queens[{q1_}] = col_Queens[{q2_}]))) & not(((row_Queens(q1_) - row_Queens(q2_)) = (col_Queens(q2_) - col_Queens(q1_))))) & not(((row_Queens(q1_) - row_Queens(q2_)) = (col_Queens(q1_) - col_Queens(q2_))))) ;
    valid_ ==   !(q1_, q2_).({q1_} <: Queens_ & {q2_} <: Queens_ => not(({q1_} = {q2_})) => nothreat_(q1_, q2_))
PROPERTIES
    (card(Queens_) = 32) &
    /* from signature declaration */ !(this_).({this_} <: Queens_ => ((((row_Queens(this_) >= 0) & (row_Queens(this_) < card(Queens_))) & (col_Queens(this_) >= 0)) & (col_Queens(this_) < card(Queens_)))) &
    row_Queens : Queens_ --> INTEGER &
    col_Queens : Queens_ --> INTEGER
OPERATIONS
    run_valid = PRE valid_ THEN skip END
END
