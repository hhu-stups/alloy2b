// test joins with binary relation
abstract sig Language {}
one sig french, german, spanish extends Language {}
abstract sig Personell
 { speaks : set Language,
   speaks_pf : lone Language, // translated as partial function
   speaks_tf : one Language // translated as total function
 }
sig male in Personell {}

one sig tom, david, jeremy, carol, janet, tracy extends Personell {}


fact defmale {
  male = tom+david+jeremy
}

fact deflang {
  speaks =tom->german + david->french + jeremy->german + carol->spanish + janet->french + tracy->spanish
  speaks_pf =tom->german + david->french + jeremy->german + carol->spanish + janet->french + tracy->spanish
  speaks_tf =tom->german + david->french + jeremy->german + carol->spanish + janet->french + tracy->spanish
}

pred check_german {
   speaks.german = tom+jeremy
   german.~speaks = tom+jeremy
}
pred check_tom {
   ~speaks.tom = german
   tom.speaks = german
}
pred check_male {
   ~speaks.male = german+french
   male.speaks = german+french
}

pred check_univ {
   ~speaks.univ = german+french+spanish
   univ.speaks = german+french+spanish
}

pred check_double {
   ~speaks.speaks = (german -> german) + (french -> french) + (spanish -> spanish)
}

// now the checks again with partial function
pred check_pf {
   speaks_pf.german = tom+jeremy
   german.~speaks_pf = tom+jeremy
   ~speaks_pf.tom = german
   tom.speaks_pf = german
   ~speaks_pf.male = german+french
   male.speaks_pf = german+french
   ~speaks_pf.univ = german+french+spanish
   univ.speaks_pf = german+french+spanish
   ~speaks_pf.speaks_pf = (german -> german) + (french -> french) + (spanish -> spanish)
}

// now the checks again with total function
pred check_tf {
   speaks_tf.german = tom+jeremy
   german.~speaks_tf = tom+jeremy
   ~speaks_tf.tom = german
   tom.speaks_tf = german
   ~speaks_tf.male = german+french
   male.speaks_tf = german+french
   ~speaks_tf.univ = german+french+spanish
   univ.speaks_tf = german+french+spanish
   ~speaks_tf.speaks_tf = (german -> german) + (french -> french) + (spanish -> spanish)
}

run check_german
run check_tom
run check_male
run check_univ
run check_double
run check_pf
run check_tf