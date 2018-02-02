open util/integer

abstract sig setX { }
one sig V {
  SS:   setX -> setX
}


assert Bug {
 #(V.SS)>1 implies #(V.SS->V.SS) >3 // counter example found when ForbidOverflow off
 #(V.SS->V.SS)=0 iff no V.SS // " when ForbidOverflow on
}

//check Bug for 3 setX, 7 int // for 8 int Translation capacity exceeded

check Bug for 3 setX, 7 int
//   counter example found when ForbidOverflow turned off and on
