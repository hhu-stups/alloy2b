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

pred check_dom {
   tom <: speaks = tom->german
   tom <: speaks_pf = tom->german
   tom <: speaks_tf = tom->german
   male <: speaks = tom->german + david->french + jeremy->german
   tom <: prefers = tom->german->french + tom->german->spanish
}
pred check_ran {
   speaks :> german = tom->german + jeremy->german
   prefers :> spanish = tom->german->spanish
   //prefers :> (german->spanish) = tom->german->spanish // not allowed by Alloy : This must be a unary set
}

run check_dom
run check_ran
