module binary_differently_typed

sig S {
 s: S
}

sig T {
 t: T
}

run { some S.s +T.t} for 2 S, 2 T

