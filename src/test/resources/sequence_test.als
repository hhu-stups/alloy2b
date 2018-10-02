open util/integer

abstract sig EventSet {}

abstract sig Walk {
 vertices: seq (EventSet),
 transitions: seq (EventSet),
 numbers : seq (Int)
} {
 vertices.first = transitions.last
 vertices.butlast != transitions.butlast
 !vertices.isEmpty
 !transitions.hasDups
 transitions.inds != vertices.inds
}

assert seqTest {
 #Walk.numbers = 7
 Walk.numbers.first = 12
 Walk.numbers.last = 12
 Walk.numbers.lastIdx = 6
 Walk.numbers.afterLastIdx = 7
 Walk.numbers.idxOf[12] = 0
 Walk.numbers.lastIdxOf [12] = 6
}

check seqTest for 6 Int, 10 seq