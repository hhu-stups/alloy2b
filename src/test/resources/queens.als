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
	and int[q1.row] - int[q2.row] != int[q2.col] - int[q1.col]
    and int[q1.row] - int[q2.row] != int[q1.col] - int[q2.col]
}

pred valid { all q1,q2 : Queens |
    q1 != q2 => nothreat[q1, q2]
 }

run valid for exactly 8 Queens, 5 int
