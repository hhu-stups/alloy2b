/*@ generated */
MACHINE alloytranslation
SETS
    queen_
CONSTANTS
    x_queen, x__queen, y_queen
DEFINITIONS
    show_ ==   1=1
PROPERTIES
    !(q_, q__).({q_} <: queen_ & {q__} <: (queen_ - {q_}) => not((x_queen(q_) = x_queen(q__))) & not((y_queen(q_) = y_queen(q__))) & not(((x_queen(q_) + y_queen(q_)) = (x_queen(q__) + y_queen(q__)))) & not(((x__queen(q_) + y_queen(q_)) = (x__queen(q__) + y_queen(q__))))) &
    /* from signature declaration */ !(this_).({this_} <: queen_ => (x_queen(this_) >= 1) & (y_queen(this_) >= 1) & (x_queen(this_) <= card(queen_)) & (y_queen(this_) <= card(queen_)) & (x__queen(this_) >= 1) & (x__queen(this_) <= card(queen_)) & (x__queen(this_) = ((card(queen_) + 1) - x_queen(this_)))) &
    x_queen : queen_ --> INTEGER &
    x__queen : queen_ --> INTEGER &
    y_queen : queen_ --> INTEGER
OPERATIONS
    run_show = PRE show_ THEN skip END
END
