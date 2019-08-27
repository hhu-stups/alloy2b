open util/boolean

pred TestBool {
  True != False
  isTrue[True]
  isFalse[False]
  isTrue[And[True,True]]
  isTrue[Or[False,True]]
}

run TestBool

