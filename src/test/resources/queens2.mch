/*@ generated */
MACHINE alloytranslation
SETS
    queen_
CONSTANTS
    x_, x__, y_
DEFINITIONS
    show_ ==   1=1
PROPERTIES
    x_ : queen_ --> INTEGER &
    x__ : queen_ --> INTEGER &
    y_ : queen_ --> INTEGER &
    !(q_, q__).({q_} <: queen_ & {q__} <: queen_ - {q_} => not(x_(q_) = x_(q__)) & not(y_(q_) = y_(q__)) & not(x_(q_) + y_(q_) = x_(q__) + y_(q__)) & not(x__(q_) + y_(q_) = x__(q__) + y_(q__))) &
    /* from signature declaration */ !(this_).({this_} <: queen_ => x_(this_) >= 1 & y_(this_) >= 1 & x_(this_) <= card(queen_) & y_(this_) <= card(queen_) & x__(this_) >= 1 & x__(this_) <= card(queen_) & x__(this_) = card(queen_) + 1 - x_(this_))
ASSERTIONS
    show_
END
