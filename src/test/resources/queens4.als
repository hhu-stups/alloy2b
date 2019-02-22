open util/integer

one sig Board{
    cols: set Column,
    rows: set Row,
    q: set Queen
}


sig Row {
  idr: disj Int
}{
   this in Board.rows
}

fact id{
    all r:Row| (minus[r.idr,1] in Row.idr or r.idr=1) and r.idr>0
    all c:Column| (minus[c.idc,1] in Column.idc or c.idc=1) and c.idc>0
}

sig Column {
  idc: disj Int
}{
  this in Board.cols
}
sig Queen{
    x:Column,
    y:Row
}
pred aligned[q1,q2:Queen]{
   q1.y=q2.y
   or
   q1.x=q2.x
   or some a,b:Int{
       plus[q1.x.idc,a]= q2.x.idc
       plus[q1.y.idr,b]= q2.y.idr
       a=b or a=minus[0,b]
    }
}
pred Show {
   no disj q1,q2:Queen| aligned[q1,q2]
}
run Show for exactly 4 Row, exactly 4 Column, exactly 4 Queen, 4 Int