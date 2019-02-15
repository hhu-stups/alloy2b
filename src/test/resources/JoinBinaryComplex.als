// test joins with binary relation
abstract sig Language {}
one sig french, german, spanish extends Language {}
abstract sig Personell
 { speaks : set Language,
   prefers : Language -> set Language, // ternary relation
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
fact defprefers {
  prefers = tom->german->french + tom->german->spanish +
            david->french->german +
            jeremy->german->french +
            carol->spanish->(german+french) +
            janet->french->german +
            tracy->spanish->french
}

pred check_1_3 {
   david.prefers = french -> german
}

pred check_3_1 {
    prefers.spanish = tom->german
}

pred check_3_2 {
     (tom->german->french).~speaks =  tom->german->david + tom->german->janet
}
pred check_2_3 {
     ~speaks.prefers =  (spanish->(spanish->french)) +
                        (spanish->(spanish->german)) +
                        (french->(french->german)) +
                        (german->(german->spanish)) +
                        (german->(german->french))
     ~speaks.prefers = ~speaks_pf.prefers
     ~speaks.prefers = ~speaks_tf.prefers
}

pred check_3_2b {
     prefers.~speaks =  tom->german->janet + tom->german->david +
            tom->german->tracy + tom->german->carol +
            david->french->tom + david->french->jeremy +
            jeremy->german->janet + jeremy->german->david +
            carol->spanish->david + carol->spanish->janet + carol->spanish->tom + carol->spanish->jeremy +
            janet->french->tom + janet->french->jeremy +
            tracy->spanish->janet + tracy->spanish->david
     prefers.~speaks = prefers.~speaks_pf
     prefers.~speaks = prefers.~speaks_tf
}


//assert check_3{
//    check_1_3
//    check_2_3
//}

pred check_univ {
   univ.prefers = french -> german + german -> french + german->spanish + spanish->german + spanish -> french
   univ.(univ.prefers) = german + french + spanish
   prefers.univ = speaks
   prefers.univ = speaks_pf
   prefers.univ = speaks_tf
   prefers.univ.univ = Personell
}

run check_univ
run check_1_3
run check_2_3
run check_3_1
run check_3_2
run check_3_2b