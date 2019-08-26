one sig T {
s1 : seq T,
s2 : seq T
}

run test2 {
no T.s1
T.s2 = T.s1.setAt[1,T]
}

run test1 {
no T.s1
T.s2 = T.s1.insert[1,T]
}

run test3 {
no T.s1
T.s2 = T.s1.setAt[0,T] 
}


run test4 {
#(T.s1) = 2
T.s2 = T.s1.delete[-1]
}

run test5 {
#(T.s1) = 0
T.s2 = T.s1.delete[0]
}

run test6 {
#(T.s1) = 1
T.s2 = T.s1.subseq[-1,-2]
}
