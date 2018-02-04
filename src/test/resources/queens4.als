open util/integer

one sig Board{
    cols: set Column,
    rows: set Row,
    q: set Queen
}


sig Row {
  id: disj Int
}{
   this in Board.rows
}

fact id{
    all r:Row| (minus[r.id,1] in Row.id or r.id=1) and r.id>0
    all c:Column| (minus[c.id,1] in Column.id or c.id=1) and c.id>0
}

sig Column {
  id: disj Int
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
       plus[q1.x.id,a]= q2.x.id
       plus[q1.y.id,b]= q2.y.id
       a=b or a=minus[0,b]
    }
}
pred Show {
   no disj q1,q2:Queen| aligned[q1,q2]
}
run Show for exactly 4 Row, exactly 4 Column, exactly 4 Queen, 4 Int