open util/integer

abstract sig A {}

abstract sig B {}

abstract sig C {
 vertices: seq (A),
 transitions: seq (A),
 numbers : seq (Int),
 setA : set A,
 setB : set B,
 anything: set (B -> (A -> B))
} {
 vertices.first = transitions.last
 vertices.butlast != transitions.butlast
 !vertices.isEmpty
 !transitions.hasDups
 transitions.inds != vertices.inds
 (anything.setA).setB in setB
}


assert seqTest {
 #C.numbers = 7
 C.numbers.first = 12
 C.numbers.last = 12
 C.numbers.lastIdx = 6
 //C.numbers.afterLastIdx = 7
 C.numbers.idxOf[12] = 0
 C.numbers.lastIdxOf [12] = 6
}

check seqTest for 6 Int, 10 seq