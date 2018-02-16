MACHINE alloy
/* open util/integer
abstract sig setX { }
one sig V {
  SS:   setX -> setX
}
assert Bug {
 #(V.SS)>1 implies #(V.SS->V.SS) >3  
}
*/
SETS 
 setx; v 
CONSTANTS 
  ss 
PROPERTIES    card(setx) = 2 &
   card(v)=1 &
   ss : v --> POW((setx * setx))

ASSERTIONS
((card(ss[v]) > 1) => (card((ss[v] * ss[v])) > 3))
END