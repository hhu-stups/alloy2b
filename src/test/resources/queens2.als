module queens2

open util/integer

// x and y are column and row address, x' is the reverse column address and is introduced to simplify calculation
// of same diagonal
sig queen { x:Int, x':Int, y:Int } {
    x >= 1
    y >= 1
    x <= #queen
    y <= #queen
    x' >=1
    x' <= #queen
    x' = minus[plus[#queen,1],x]
}

fact { all q:queen, q':(queen-q) {
    ! q.x = q'.x                                    //different cols
    ! q.y = q'.y                                    //different rows
    ! plus[q.x,q.y] = plus[q'.x,q'.y]               //different top left -> bottom right diagonal
    ! plus[q.x',q.y] = plus[q'.x',q'.y]             //different top right -> bottom left diagonal
}}

pred show {}

run show for exactly 20 queen, 6 int
