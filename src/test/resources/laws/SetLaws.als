
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
 V.SS in V.VV implies (#(V.SS) <= #(V.VV))
 #(V.SS) < #(V.VV) implies not(V.VV in V.SS)
 (#(V.SS + V.VV) <= plus[#(V.SS) , #(V.VV)])
 (V.SS & V.VV = V.Empty  iff V.SS in setX - V.VV)
}

assert SetLawsAlloyStrange {
  (#(V.SS + V.VV) <= (#(V.SS) + #(V.VV))) // Counter Example found by Alloy ! because + is not arith. +
}

assert SetLawsFalse {
 V.SS - V.TT = V.SS
}

check SetLaws for 5 setX, 7 int
