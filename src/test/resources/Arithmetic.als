open util/integer

pred Arithmetic {
  3 > 2
  3 >= 2
  2 < 4
  4 =< 5
  plus[1,1] = 2
  mul[2,3] = 6
  mul[3,2] = 6
  minus[3,2] = 1
  minus[10,10] = 0
  div[10,5] = 2
  div[11,5] = 2
  rem[10,5] = 0
  rem[11,5] = 1
  sum[2] = 2
  plus[1,1] > 1
  mul[plus[2,2],mul[2,3]] = 24
  2->3 = plus[1,1]->plus[1,2]
  2->(3+7) = plus[1,1]->plus[1,2] + 2->minus[8,1]
 // sum[1+2+3] = 6 // does not work !
 //sum[{1}+{2}+{3}] = 6 // does not work yet either
  1 + 2 = {1} + {2}
  plus[ 1+plus[1,0], 2] = 3
  minus[2,3] = -1
  //  plus[1+2,3] = 4+5 // seems to be false
  // plus[{1}+{2},{3}]= {4}+{5} // also seems to be false
  //plus[{1}+{2},{3}] = 6 // this is true in Alloy, ProB's MU operator currently gives a WD error
  //plus[{1}-{1},{2}] = 2 true in Alloy, MU WD error in ProB
  //plus[none,none] = 0 true in Alloy, MU WD error in ProB
  //add[none,none] = 0 ditto
  add[ 1+sub[1,0], 2] = 3

  gt[3,2]
  gt[plus[1,1],1]
  gte[plus[1,1],1]
  lt[minus[1,1],1]
  lte[minus[1,1],1]
  min[1+2+3] = 1
  max[1+2+3] = 3
  min = -32
  max = 31
  //100 = if plus[1,2] = 3 then 100 else 200 // cannot be parsed in Alloy; seems to contradict book

  div[3,-2] = -1
  div[-3,-2] = 1
  div[-3,2] = -1
  //rem[3,-2] = 1  // gives WD Errors
  //rem[-3,2] = -1  // would give no WD error in Z/TLA mode but result is different
  //rem[-3,-2] = -1

  negate[-2] = 2
  next[1] = 2
  prev[1] = 0
  add[next[next[3]],3] = 8

  3.add[2] = 5
  1.add[4.mul[2]] = 9
  1.minus[4.mul[2]] = -7

  (3>2 => 7>1 else 8>9) // if-then-else as predicate
  (3>2 => 7 else 8) = 7 // if-then-else as expression

  //div[3,0] = -1 // this is what Alloys Console returns, ProB gives WD-error
}

fact {
  Arithmetic
}

assert {
  Arithmetic
}

run Arithmetic for 6 Int
