sig T {
 r: set T
} {
 r = T
}
check { all a: T | all a: a.r | some a } for 2 T