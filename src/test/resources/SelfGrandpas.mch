MACHINE SelfGrandpas

SETS Person

CONSTANTS Man, Woman, father, mother, wife, husband, m

DEFINITIONS
    scope_Person == 4;
    grandpas(p) == {pe | p : Person & pe : Person & pe : LET parent BE
    parent = mother \/ father \/ (father ; wife) \/ (mother ; husband)
    IN (parent ; parent)[{p}] /\ Man END};
    ownGrandpa(m) == m : Man & m : grandpas(m);

PROPERTIES
    Man <: Person &
    Woman <: Person &
    Man /\ Woman = {} &
    Man \/ Woman = Person &
    father : Person <-> Man &
    !(p).(p : Person => card(father[{p}]) <= 1) &
    mother : Person <-> Woman &
    !(p).(p : Person => card(mother[{p}]) <= 1) &
    wife : Man <-> Woman &
    !(ma).(ma : Man => card(wife[{ma}]) <= 1) &
    husband : Woman <-> Man &
    !(w).(w : Woman => card(husband[{w}]) <= 1) &
    card({p | p : Person & p : closure1(mother \/ father)[{p}]}) = 0 &
    wife = husband~ &
    card(wife /\ (closure(mother \/ father) ; mother)) = 0 &
    card(husband /\ (closure(mother \/ father) ; father)) = 0 &
    ownGrandpa(m)
END