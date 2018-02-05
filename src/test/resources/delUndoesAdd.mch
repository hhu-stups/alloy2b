/*@ generated */
MACHINE alloytranslation
SETS
    Name_; Addr_; Book_
CONSTANTS
    addr_Book
DEFINITIONS
    add_(b_,b__, n_, a_) == {b_} <: Book_ & {b__} <: Book_ & {n_} <: Name_ & {a_} <: Addr_  &  ((addr_Book[{b__}] = (addr_Book[{b_}] \/ ({n_} <-> {a_})))) ;
    del_(b_,b__, n_) == {b_} <: Book_ & {b__} <: Book_ & {n_} <: Name_  &  ((addr_Book[{b__}] = (addr_Book[{b_}] - ({n_} <-> Addr_))))
PROPERTIES
    addr_Book : Book_ --> (Name_ +-> Addr_)
OPERATIONS
    check_delUndoesAdd = PRE not(/* delUndoesAdd */ (!(b_, b__, b___, n_, a_).({b_} <: Book_ & {b__} <: Book_ & {b___} <: Book_ & {n_} <: Name_ & {a_} <: Addr_ => (((addr_Book[{b_}][{n_}] = {} & add_Name(b_, b__, n_, a_)) & del_Name(b__, b___, n_)) => (addr_Book[{b_}] = addr_Book[{b___}]))))) THEN skip END
END
