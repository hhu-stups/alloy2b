/*@ generated */
MACHINE alloytranslation
SETS
    Name_; Addr_; Book_
CONSTANTS
    addr_Book
DEFINITIONS
    add_(b_,b__, n_, a_) == {b_} <: Book_ & {b__} <: Book_ & {n_} <: Name_ & {a_} <: Addr_  &  (addr_Book[{b_}][{n_}] = {}) & ((addr_Book[{b__}] = (addr_Book[{b_}] \/ ({n_} * {a_})))) ;
    add_wrong_(b_,b__, n_, a_) == {b_} <: Book_ & {b__} <: Book_ & {n_} <: Name_ & {a_} <: Addr_  &  ((addr_Book[{b__}] = (addr_Book[{b_}] \/ ({n_} * {a_}))))
PROPERTIES
    addr_Book : Book_ <-> (Name_ <-> Addr_)
OPERATIONS
    check_add_preserves_pfun = PRE not(/* add_preserves_pfun */ (!(b_, b__, n_, a_).({b_} <: Book_ & {b__} <: Book_ & {n_} <: Name_ & {a_} <: Addr_ => (((addr_Book[{b_}] <: (Name_ +-> Addr_)) & add_(b_, b__, n_, a_)) => (addr_Book[{b__}] <: (Name_ +-> Addr_)))))) THEN skip END
END
