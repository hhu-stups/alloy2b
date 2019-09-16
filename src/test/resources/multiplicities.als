sig U {}
sig T {
 r: some U
}

assert multiplicities {
 r in T -> U
 r in T -> some U
 r in T -> one U
 r in T -> lone U
 r in T some -> U
 r in T some -> some U
 r in T some -> lone U
 r in T some -> one U
 r in T lone -> U
 r in T lone -> some U
 r in T lone -> lone U
 r in T lone -> one U
 r in T one -> U
 r in T one -> some U
 r in T one -> lone U
 r in T one -> one U
}

check multiplicities for 2 U, 2 T
