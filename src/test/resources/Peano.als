sig NaturalNumber {succ: one NaturalNumber,
                   predc: lone NaturalNumber}

assert PeanoAxiomAdapted {
 all x: NaturalNumber, y: NaturalNumber, z:NaturalNumber | 
//      y.^predc = x and z.^predc=y => x.^succ = z
      x in y.^predc and y in z.^predc => z in x.^succ
}

pred Ordering (x,y: NaturalNumber) {
   x.succ =y => y.predc = x
}

fact {
  all x,y:NaturalNumber | Ordering[x,y]
}

check PeanoAxiomAdapted