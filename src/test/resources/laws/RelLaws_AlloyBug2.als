open util/integer

abstract sig setX { }
one sig V {
  SS:   setX -> setX
}


assert Bug {
#(V.SS->V.SS)=0 iff no V.SS
}

//check Bug for 3 setX, 7 int // for 8 int Translation capacity exceeded

check Bug for 3 setX, 7 int
// no counter example found when ForbidOverflow turned off
