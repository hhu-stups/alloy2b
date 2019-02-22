
one sig Board{
    cols: seq Column,
    rows: seq Row,
    q: set Queen
}
sig Row {
  idr:Int
}{
   idr=Board.rows.idxOf[this]
   this in Board.rows.elems
}

sig Column {
  idc:Int
}{
  idc=Board.cols.idxOf[this]
  this in Board.cols.elems
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
       add[q1.x.idc,a]= q2.x.idc 
       add[q1.y.idr,b]= q2.y.idr
       a=b or a=sub[0,b]
    } 
}
pred Show {
   no disj q1,q2:Queen| aligned[q1,q2]
}
run Show for exactly 4 Row, exactly 4 Column, exactly 4 Queen, 4 seq
