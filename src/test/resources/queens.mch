/*@ generated */
MACHINE alloytranslation
SETS
    Queens_
CONSTANTS
    row_, col_
DEFINITIONS
    nothreat_(q1_,q2_) == q1_ : Queens_ & q2_ : Queens_ & row_[{q1_}] = row_[{q2_}] & col_[{q1_}] = col_[{q2_}] & row_[{q1_}] - row_[{q2_}] = col_[{q2_}] - col_[{q1_}] & row_[{q1_}] - row_[{q2_}] = col_[{q1_}] - col_[{q2_}] ;
    valid_ ==  & !(q1_, q2_).(q1_ : Queens_ & q2_ : Queens_ => q1_ = q2_ => nothreat_(q1_, q2_))
PROPERTIES
    row_ : Queens_ <-> INTEGER &
    col_ : Queens_ <-> INTEGER &
    card(Queens_) = 32
ASSERTIONS
    valid_
END
