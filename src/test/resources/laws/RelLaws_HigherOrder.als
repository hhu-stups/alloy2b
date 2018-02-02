open util/integer
abstract sig setX { }
one sig V {
  SS:   setX -> setX,
  TT:   setX -> setX
}

assert HO {
  V.SS + V.SS = V.SS
   all xx : V.SS | (xx in V.TT implies xx in V.SS & V.TT)
}
check HO for 3 setX
//   counter example found when ForbidOverflow turned off and on
// for 8 int Translation capacity exceeded
