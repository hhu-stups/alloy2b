
open util/integer

abstract sig setX { }
one sig V {
  SS: set setX,
  TT: set setX,
  VV: set setX,
  Empty: set setX
}

fact EmptySet {  no V.Empty }

assert SetLaws {
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
 all xx : V.SS | (xx in V.TT implies xx in V.SS & V.TT)
 all xx : V.SS | not(xx in V.TT) implies (xx in V.SS - V.TT && not(xx in V.TT))
 all xx : V.SS | (xx in V.TT && xx in V.VV) implies not(V.SS & V.TT & V.VV = V.Empty)
 all xx : V.SS | not(xx in V.TT) implies ( not(V.SS + V.TT = V.TT) && not(V.SS - V.TT = V.Empty))
 (V.SS in V.VV)  or not(V.SS in V.VV)
 V.SS in V.VV implies (#(V.SS) =< #(V.VV))
 #(V.SS) < #(V.VV) implies not(V.VV in V.SS)
 (#(V.SS + V.VV) =< plus[#(V.SS) , #(V.VV)])
 (V.SS & V.VV = V.Empty  iff V.SS in setX - V.VV)
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
 one V.SS implies #(V.SS->V.SS) = 1
 #(V.SS)>1 implies #(V.SS->V.SS) >3
#(V.SS->V.SS)=0 iff no V.SS
#(V.SS -> V.TT)=0 iff ( no V.SS or no V.TT)
 //all rr : (V.SS -> V.SS) | no rr.(V.Empty) // not allowed because of higher-order quantification
 (V.SS -> V.SS).(V.Empty) = V.Empty
~(V.SS -> V.SS) = (V.SS -> V.SS)
^(V.SS -> V.SS) = (V.SS -> V.SS)
(V.SS -> V.SS) .(V.SS -> V.SS) = (V.SS -> V.SS)
not(~(V.SS -> V.TT) = (V.SS -> V.TT)) implies not(V.SS = V.TT)
// some laws above using cartesian products:
 (V.SS -> V.SS)+ ((V.TT -> V.TT) & (V.VV -> V.VV)) = ((V.SS -> V.SS) + (V.TT -> V.TT)) & ((V.SS -> V.SS) + (V.VV -> V.VV))
}

assert SetLawsAlloyStrange {
  (#(V.SS + V.VV) =< (#(V.SS) + #(V.VV))) // Counter Example found by Alloy ! because + is not arith. +
}

assert SetLawsFalse {
 V.SS - V.TT = V.SS
}

check SetLaws for 5 setX, 7 int
