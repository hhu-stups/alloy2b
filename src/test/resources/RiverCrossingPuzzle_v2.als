// this version uses a more constructive (non-circular) crossRiver predicate
/* Impose an ordering on the State. */
open util/ordering[State]

/* Farmer and his possessions are objects. */
abstract sig Object { eats: set Object }
one sig Farmer, Fox, Chicken, Grain extends Object {}

/* Defines what eats what and the farmer is not around. */
fact { eats = Fox->Chicken + Chicken->Grain}

/* Stores the objects at near and far side of river. */
sig State { near, far: set Object }

/* In the initial state, all objects are on the near side. */
fact { first.near = Object && no first.far }

/* At most one item to move from 'from' to 'to' */
pred crossRiver [from, from', to, to': set Object] {
  some x: from | {
    from' = from - x - Farmer - (from - x - Farmer).eats
    to' = to + x + Farmer
  }
}

/* crossRiver transitions between states */
fact {
  all s: State, s': s.next {
    Farmer in s.near =>
      crossRiver [s.near, s'.near, s.far, s'.far]
    else
      crossRiver [s.far, s'.far, s.near, s'.near]
  }
}

/* the farmer moves everything to the far side of the river. */
run { last.far=Object } for exactly 8 State
//run { last.far=Farmer+Grain } for exactly 2 State

// for ProB viewing expression as table:
// %i.(i:1..8 | near_State[{i}] |-> far_State[{i}])