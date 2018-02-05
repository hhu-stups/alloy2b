open util/ordering [Flight]

abstract sig Language {}
one sig french, german, spanish extends Language {}
abstract sig Personell { speaks : set Language }
one sig tom, david, jeremy, carol, janet, tracy extends Personell {}
sig male in Personell {}
sig Flight {
  assign : set Personell
}

fact defmale {
  male = tom+david+jeremy
}
fact deflang {
  speaks =tom->german + david->french + jeremy->german + carol->spanish + janet->french + tracy->spanish
}
pred allLanguages {
  all f:Flight | f.assign.speaks = Language
}
pred allSexes {
  all f:Flight | some (f.assign-male) and some (f.assign & male)
}
pred scheduleOk {
  all p:Personell, f : Flight | p in f.assign and p in next[f].assign => not (p in next[next[f]].assign)
}
pred everybodyInSchedule {
  univ.assign = Personell
}
pred crewAlloc {
  allLanguages and allSexes and scheduleOk and everybodyInSchedule
}

run crewAlloc for 3 Flight