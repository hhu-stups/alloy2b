abstract sig Language {}
one sig french, german, spanish extends Language {}

pred HOQuant {
   some x : Language -> Language | no x
   all x : Language | not no x
   all x: german+french | not no x
   some x: Language -> one Language | french.x = french and german.x = german
   some x : Language -> one Language | x in Language -> one Language
}

pred HOQuant2 {
 // these cannot be dealt with by Alloy
  all x : Language -> one Language | #x = 3 // requires higher-order quantification
  all x : Language -> one Language | x in Language -> one Language // requires HO
}
pred OtherQuant {
   some x1 : german+spanish | x1 = german
   all x2 : german+spanish | x2 != french
 //  some x : Language -> Language | x !in Language -> one Language // Alloy says: multiplicity not allowed here
  some x : Language -> Language | not x in Language -> one Language
}


pred Quant {
  all q0 : Language | q0 = french or q0 = spanish or q0 = german
  some q1 : Language | q1 = french
  one q1 : Language | q1 = french
  some q2 : Language | q2 in (french+german)

 // multiple IDs
  some disjoint x,y : Language | x=french and y=spanish
  some x,y : Language | x=y
  not some disjoint x,y : Language | x=y // not yet translated
  not  some disjoint x,y,z : Language | x=y or x=z
  all disjoint x,y : Language | x=french implies y in spanish+german
  one disjoint x,y,z : Language | x=french and y=spanish
  some x:Language, y:Language | x=y
  all disjoint x,y : Language, z:Language | x=z implies y !=z
  one x:Language | x=french
  not one x:Language | x != french
  lone x:Language | x=french
  lone x:Language | x=french and x=spanish
  some disjoint x:french+spanish, y:german+spanish | x = y // disjoint applies only to first arg


 // multiplicities in bounding expression:
  some q1 : lone Language | q1 = french
  some q1 : lone Language | no q1
  all q1 : one Language | q1 = french or q1 = spanish or q1 = german
  not all q1 : set Language | q1 = french or q1 = spanish or q1 = german // ok in Alloyt, without not: requires higher-order quantification
  some q3 : some Language | q3 = french+german // ok in Alloy, with all: requires higher-order quantification

  HOQuant
  OtherQuant
  HOQuant2
 }

pred False {
   some x : Language | no x // this is false  -> only singleton sets allowed
   some x : Language -> Language | no x // this is true !!  -> empty set allowed
}

run Quant
