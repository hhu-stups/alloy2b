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

