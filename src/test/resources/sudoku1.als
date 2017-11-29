/*
This is a pretty straightforward specification of a sudoko model. I use row
and column types, and declare singleton sigs for each row and column.
*/
module sudoku

abstract sig Row, Column, Number {}

one sig Row1, Row2, Row3, Row4, Row5,
	Row6, Row7, Row8, Row9 extends Row {}
one sig Col1, Col2, Col3, Col4, Col5, Col6,
	Col7, Col8, Col9 extends Column {}
one sig One, Two, Three, Four, Five, Six,
	Seven, Eight, Nine extends Number {}

sig Board {
	array : Row -> Column ->one Number
}{
	all r : Row		| array[r][Column]	= Number
	all c : Column	| array[Row][c]		= Number

	array[Row1+Row2+Row3][Col1+Col2+Col3]	= Number
	array[Row1+Row2+Row3][Col4+Col5+Col6]	= Number
	array[Row1+Row2+Row3][Col7+Col8+Col9]	= Number
	array[Row4+Row5+Row6][Col1+Col2+Col3]	= Number
	array[Row4+Row5+Row6][Col4+Col5+Col6]	= Number
	array[Row4+Row5+Row6][Col7+Col8+Col9]	= Number
	array[Row7+Row8+Row9][Col1+Col2+Col3]	= Number
	array[Row7+Row8+Row9][Col4+Col5+Col6]	= Number
	array[Row7+Row8+Row9][Col7+Col8+Col9]	= Number
}

run {some Board} for 1

check {one Board} for 1


// Solved in 0:44
/*
fact EasyPuzzle {
	Board.array[Row1][Col1] = Seven
	Board.array[Row1][Col2] = Eight
	Board.array[Row1][Col3] = One
	Board.array[Row1][Col4] = Six
	Board.array[Row1][Col6] = Two
	Board.array[Row1][Col7] = Nine
	Board.array[Row1][Col9] = Five

	Board.array[Row2][Col1] = Nine
	Board.array[Row2][Col3] = Two
	Board.array[Row2][Col4] = Seven
	Board.array[Row2][Col5] = One

	Board.array[Row3][Col3] = Six
	Board.array[Row3][Col4] = Eight
	Board.array[Row3][Col8] = One
	Board.array[Row3][Col9] = Two

	Board.array[Row4][Col1] = Two
	Board.array[Row4][Col4] = Three
	Board.array[Row4][Col7] = Eight
	Board.array[Row4][Col8] = Five
	Board.array[Row4][Col9] = One

	Board.array[Row5][Col2] = Seven
	Board.array[Row5][Col3] = Three
	Board.array[Row5][Col4] = Five
	Board.array[Row5][Col9] = Four

	Board.array[Row6][Col3] = Eight
	Board.array[Row6][Col6] = Nine
	Board.array[Row6][Col7] = Three
	Board.array[Row6][Col8] = Six

	Board.array[Row7][Col1] = One
	Board.array[Row7][Col2] = Nine
	Board.array[Row7][Col6] = Seven
	Board.array[Row7][Col8] = Eight

	Board.array[Row8][Col1] = Eight
	Board.array[Row8][Col2] = Six
	Board.array[Row8][Col3] = Seven
	Board.array[Row8][Col6] = Three
	Board.array[Row8][Col7] = Four
	Board.array[Row8][Col9] = Nine

	Board.array[Row9][Col3] = Five
	Board.array[Row9][Col7] = One
}
*/

/*
// Solved in 1:38
fact HardPuzzle {
	Board.array[Row1][Col1] = Two
	Board.array[Row1][Col4] = Three
	Board.array[Row1][Col7] = Four
	Board.array[Row1][Col9] = Eight

	Board.array[Row2][Col2] = Seven
	Board.array[Row2][Col3] = Four

	Board.array[Row3][Col6] = Nine
	Board.array[Row3][Col9] = One

	Board.array[Row4][Col1] = Eight
	Board.array[Row4][Col4] = Nine
	Board.array[Row4][Col7] = Two
	Board.array[Row4][Col8] = Six

	Board.array[Row5][Col6] = Eight
	Board.array[Row5][Col9] = Five

	Board.array[Row6][Col1] = Six
	Board.array[Row6][Col5] = Five

	Board.array[Row7][Col3] = Eight
	Board.array[Row7][Col5] = Two
	Board.array[Row7][Col8] = Four

	Board.array[Row8][Col1] = Four
	Board.array[Row8][Col8] = Two

	Board.array[Row9][Col1] = Seven
	Board.array[Row9][Col6] = One
	Board.array[Row9][Col8] = Five
}
*/

// Solved in 1:25
fact VeryHardPuzzle {
	Board.array[Row1][Col5] = Five
	Board.array[Row1][Col6] = Two
	Board.array[Row1][Col8] = Four

	Board.array[Row2][Col1] = One
	Board.array[Row2][Col3] = Five
	Board.array[Row2][Col4] = Three

	Board.array[Row3][Col2] = Six
	Board.array[Row3][Col9] = Five

	Board.array[Row4][Col3] = Four
	Board.array[Row4][Col6] = Seven
	Board.array[Row4][Col8] = Nine

	Board.array[Row5][Col2] = Eight
	Board.array[Row5][Col4] = Two
	Board.array[Row5][Col6] = Nine
	Board.array[Row5][Col8] = Five

	Board.array[Row6][Col2] = Nine
	Board.array[Row6][Col4] = Six
	Board.array[Row6][Col7] = Three

	Board.array[Row7][Col1] = Six
	Board.array[Row7][Col8] = Two

	Board.array[Row8][Col6] = Three
	Board.array[Row8][Col7] = Nine
	Board.array[Row8][Col9] = One

	Board.array[Row9][Col2] = Seven
	Board.array[Row9][Col4] = Eight
	Board.array[Row9][Col5] = Two
}
