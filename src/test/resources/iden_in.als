sig T {
r : set T
}

fact {
some r
}

sig U {
p : set U
}

fact {
some p
}

fact {T.r = T}

check test2 {iden in (~r).r} for 3 T, 3 U

