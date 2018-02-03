open util/integer

abstract sig setX { }
one sig V {
  SS:   setX -> setX,
  TT:   setX -> setX,
  VV:   setX -> setX,
  xx: set setX,
  yy: set setX,  
  EmptyS: set setX,
  Empty:   setX -> setX
}

fact EmptySet {  no V.EmptyS }
fact EmptyRel {  no V.Empty }

assert SetLaws {
 // first some of the Laws from SetLaws
  V.SS + V.SS = V.SS
  no V.SS - V.SS
  V.SS = V.SS & V.SS
 V.SS - V.Empty = V.SS
 V.Empty =  V.SS -  V.SS
 V.Empty -  V.SS =  V.Empty
V.SS +  V.TT =  V.TT +  V.SS
 V.SS &  V.TT =  V.TT &  V.SS
 V.SS +  (V.TT + V.VV) =  (V.SS +  V.TT) + V.VV
 V.SS &  (V.TT & V.VV) =  (V.SS &  V.TT) & V.VV
 V.SS + (V.TT & V.VV) = (V.SS + V.TT) & (V.SS + V.VV)
 V.SS & (V.TT + V.VV) = (V.SS & V.TT) + (V.SS & V.VV)
 (V.SS & V.TT) + (V.SS - V.TT) = V.SS
 (V.SS - V.TT) & V.TT = V.Empty
 V.SS - (V.TT - V.VV) = (V.SS - V.TT) + (V.SS & V.VV)
 (V.SS - V.TT) - V.VV = (V.SS - (V.TT + V.VV))
 V.SS + (V.TT - V.VV) = (V.SS + V.TT) - (V.VV - V.SS)
 V.SS & (V.TT - V.VV) = (V.SS & V.TT) - V.VV
 (V.SS + V.TT) - V.VV = (V.SS - V.VV) + (V.TT - V.VV)
 V.SS - (V.TT & V.VV) = ( V.SS - V.TT) + (V.SS - V.VV)
 V.SS in V.SS + V.TT
 V.TT in V.SS + V.TT
 ( V.SS in V.VV && V.TT in V.VV) implies (V.SS+V.TT in V.VV)
 V.SS & V.TT in V.SS
 V.SS & V.TT in V.TT
 (V.VV in V.SS && V.VV in V.TT) implies (V.VV in V.SS & V.TT)
// Cannot be done because higher-order: 
//all xx : V.SS | (xx in V.TT implies xx in V.SS & V.TT)
 //all xx : V.SS | not(xx in V.TT) implies (xx in V.SS - V.TT && not(xx in V.TT))
 //all xx : V.SS | (xx in V.TT && xx in V.VV) implies not(V.SS & V.TT & V.VV = V.Empty)
 //all xx : V.SS | not(xx in V.TT) implies ( not(V.SS + V.TT = V.TT) && not(V.SS - V.TT = V.Empty))
 (V.SS in V.VV)  or not(V.SS in V.VV)
 V.SS in V.VV implies (#(V.SS) =< #(V.VV))
 #(V.SS) < #(V.VV) implies not(V.VV in V.SS)
 (#(V.SS + V.VV) =< plus[#(V.SS) , #(V.VV)])
 (V.SS & V.VV = V.Empty  iff V.SS in (setX -> setX) - V.VV)
 one V.SS iff #(V.SS)=1
 lone V.SS iff #(V.SS) =<1
 some V.SS iff #(V.SS) > 0
 // all V.SS iff V.SS = setX no longer allowed
 one (V.SS + V.TT) implies (one V.SS or one V.TT)
 some (V.SS + V.TT) iff (some V.SS or some V.TT)
 some (V.SS & V.TT) implies (some V.SS && some V.TT)
 (lone V.SS implies lone (V.SS & V.TT))
 (lone V.SS && lone V.TT) implies lone (V.SS &  V.TT)
 lone V.SS implies lone (V.SS - V.TT)
 lone V.Empty
 not(some V.Empty)

// some relation tests
 one V.SS implies #(V.SS->V.SS) = 1  // this increases runtime quite a bit for 5 setX
 //#(V.SS)>1 implies #(V.SS->V.SS) >3 // Alloy incorrectly finds counterexample because of overflow
#(V.SS->V.SS)=0 iff no V.SS
#(V.SS -> V.TT)=0 iff ( no V.SS or no V.TT)
 no (V.SS -> V.SS).(V.EmptyS) 
//~(V.SS -> V.SS) = (V.SS -> V.SS) // ~ can only be used with binary relation

// Relation Laws
  V.SS in setX -> one setX  implies #(V.SS)=#(setX)
  V.SS in (setX -> one setX) implies  V.SS in (setX -> lone setX) 
 // V.SS in (setX -> one setX & setX lone -> lone setX) implies  V.SS in (setX lone -> one setX) // multiplicity expression only allowed on its own

  V.SS in (setX -> one setX) && V.SS in (setX lone -> lone setX)  implies  V.SS in (setX lone -> one setX)
}

assert Bug {
 #(V.SS)>1 implies #(V.SS->V.SS) >3  // counter example with eg check Bug for 3 setX, 7 int
}

//check Bug for 3 setX, 7 int // for 8 int Translation capacity exceeded

check SetLaws for 3 setX, 7 int
