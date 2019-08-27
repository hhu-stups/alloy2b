one sig T {
s1 : seq T,
s2 : seq T
}

one sig I {
t1 : seq Int,
t2 : seq Int
}

run test1 {
no T.s1
T.s2 = T.s1.insert[0,T].insert[1,T]
!T.s1.hasDups
none = T.s1.inds
0 = T.s1.afterLastIdx
T.s1.isEmpty
T.s2.hasDups
0 = T.s2.idxOf[T]
1 = T.s2.lastIdxOf[T]
{0} + {1} = T.s2.inds
{0} + {1} = T.s2.indsOf[T]
2 = T.s2.afterLastIdx
T.s2 = T.s2.append[T.s1]
T.s1.add[T] = T.s2.delete[1]
T.s1 = T.s2.delete[1].delete[0]
T.s2 = T.s2.setAt[0,T].setAt[1,T]
T.s1.insert[0,T] = T.s2.subseq[0,0]

no I.t1
I.t2 = I.t1.insert[0,1].insert[1,{2}]
}

