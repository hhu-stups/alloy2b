sig Queens {
  row : one Int,
  col: one Int
} {
 row >= 0 and row < #Queens
 and col >= 0 and col < #Queens
}

pred nothreat(q1,q2 : Queens) {
	q1.row != q2.row 
	and q1.col != q2.col 
	and int[q1.row]  + int[q1.col]  != int[q2.col] + int[q2.row]
    and int[q1.row]  + int[q2.col]  != int[q1.col] + int[q2.row]
// with Alloy 4.2 one no longer finds solutions to the original encoding:
//	and int[q1.row] - int[q2.row] != int[q2.col] - int[q1.col]
//    and int[q1.row] - int[q2.row] != int[q1.col] - int[q2.col]
}

pred valid { all q1,q2 : Queens |
    q1 != q2 => nothreat[q1, q2]
 }

fact card {#Queens = 4}
run valid for 4 Queens, 7 int

// solving time 1-1.5 + 2 secs for run valid for 16 Queens, 6 int
