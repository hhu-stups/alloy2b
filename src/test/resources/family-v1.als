sig Person{}

one sig Humans {

men : set Person,
women : set Person,
mother : Person -> lone women,
father : Person -> lone men,
husband : women lone -> lone men,
wife : men lone -> lone women,
}

fact {
// a person is either a man or a woman
Humans.women = Person - Humans.men

// nobody can be his own ancestor
//no *Humans.mother & iden

# Humans.husband > 1
# Humans.wife> 1



}

pred show []{}

run show for 10