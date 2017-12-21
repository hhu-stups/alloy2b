
abstract sig Language {}
one sig french, german, spanish extends Language {}
abstract sig Personell { speaks : set Language }
sig male in Personell {}

one sig tom, david, jeremy, carol, janet, tracy extends Personell {}
one sig tom2, david2, jeremy2, carol2, janet2, tracy2 extends Personell {}
one sig tom3, david3, jeremy3, carol3, janet3, tracy3 extends Personell {}
one sig tom4, david4, jeremy4, carol4, janet4, tracy4 extends Personell {}
one sig tom5, david5, jeremy5, carol5, janet5, tracy5 extends Personell {}
one sig tom6, david6, jeremy6, carol6, janet6, tracy6 extends Personell {}
one sig tom7, david7, jeremy7, carol7, janet7, tracy7 extends Personell {}
one sig tom8, david8, jeremy8, carol8, janet8, tracy8 extends Personell {}
one sig tom9, david9, jeremy9, carol9, janet9, tracy9 extends Personell {}


fact defmale {
  male = tom+david+jeremy
}

fact deflang {
  speaks =tom->german + david->french + jeremy->german + carol->spanish + janet->french + tracy->spanish +
          tom2->french + david2->french + jeremy2->french + carol2->spanish + janet2->french + tracy2->spanish +
          tom3->french + david3->french + jeremy3->french + carol3->spanish + janet3->french + tracy3->spanish +
          tom4->french + david4->french + jeremy4->french + carol4->spanish + janet4->french + tracy4->spanish +
          tom5->french + david5->french + jeremy5->french + carol5->spanish + janet5->french + tracy5->spanish +
          tom6->french + david6->french + jeremy6->french + carol6->spanish + janet6->french + tracy6->spanish +
          tom7->french + david7->french + jeremy7->french + carol7->spanish + janet7->french + tracy7->spanish +
          tom8->french + david8->french + jeremy8->french + carol8->spanish + janet8->french + tracy8->spanish +
          tom9->french + david9->french + jeremy9->french + carol9->spanish + janet9->french + tracy9->spanish
}

pred checkGerman {
   ~speaks[german] = tom+jeremy
}

pred checkFrench {
   #~speaks[french] = 34
}

//run checkGerman
run checkFrench