open util/integer

pred UtilInteger {
 int[{2}+{1}] = 3
 int[{1}+{1}] = 1

 plus[int[{1}+{3}], 2] = 6
 plus[1, {2}+{3}] = 6
 plus[{1}, {2}+{3}] = 6
 plus[{1}, {2}+{3}+{4}] = 10
 plus[{1}+{5}, {2}+{3}+{4}] = 15
 ({1}+{5}).plus[{2}+{3}+{4}] = 15


 add[{1}+{3}, 2] = 6
 add[1, {2}+{3}] = 6
 add[{1}, {2}+{3}] = 6
 add[{1}, {2}+{3}+{4}] = 10
 add[{1}+{5}, {2}+{3}+{4}] = 15
 ({1}+{5}).add[{2}+{3}+{4}] = 15

 sub[{1}+{2},2] != min
 sub[{1}+{3},2] = 2
 sub[{1}+{5},{2}+{1}] = 3


 mul[3,2] = 6
 mul[3+1,2] = 8
 mul[{3}+{1}, 2] = 8
 mul[{3}+{2}, {1}+{2}] = 15

 div[4, 2] = 2
 div[{1}+{3}, 2] = 2
 div[{1}+{3}, {1}+{1}] = 4
 div[{1}+{3}, {2}+{0}] = 2

 rem[4, 2] = 0
 rem[4, 3] = 1
 rem[{1}+{3}, 2] = 0
 rem[{4}+{3}, {1}+{2}] = 1
}

pred Comparison {

 lt[{1},{2}]
 lt[{2},{1}+{3}]
 not gt[{4},{1}+{3}]
 not lt[{2}+{3}+{4},{1}+{3}+{1}]
 lte[{2}+{3}+{4},{1}+{6}+{2}]
 gte[{2}+{3}+{4},{1}+{7}+{1}]
 eq[{2}+{3}+{4},{1}+{6}+{2}]
 // lhs is empty and thus 0
 lt[{1}&{2},{1}]
 lte[{1}&{2},{0}]
 gte[{1}&{2},{0}]
 eq[{1}&{2},{0}]
 {1}&{2} + {3} = 3
 minus[{1}&{2},{2}&{3}] = 0
 plus[{1}&{2},{2}&{3}] = 0
 plus[none,none] = 0

 signum[1] = 1
 signum[7] = 1
 signum[-1] = -1
 signum[-5] = -1
 signum[0] = 0
 signum[minus[0,1]] = -1

 {1}&{2} > -1 // of course this is true ^^
 gt[2, 1]
 gt[{4}+{3}, {3}+{2}+{1}]
 {4}+{3} > {3}+{2}+{1}

 smaller[1,2] = 1
 smaller[{1}+{2},2] = 2
 smaller[{1}+{2},{2}+{3}] = 3
}

assert {
  UtilInteger
}

assert {
  Comparison
}

pred eval {
  UtilInteger and Comparison
}

run eval for 5 Int
