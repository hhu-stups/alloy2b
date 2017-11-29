MACHINE alloytranslation
SETS
    Person
CONSTANTS
    father, mother, wife, Man, husband, Woman
DEFINITIONS
    grandpas(p) == { temp | p <: Person & temp : LET parent BE parent = mother \/ father \/ (father ; wife) \/ (mother ; husband) IN parent[parent[p]] /\ Man END} ;
    ownGrandpa(m) == m <: Man & m <: grandpas(m)
PROPERTIES
    father : Person +-> Man &
    mother : Person +-> Woman &
    wife : Man +-> Woman &
    Man <: Person &
    husband : Woman +-> Man &
    Woman <: Person &
    not(#(p).(p <: Person & p <: closure1(mother \/ father)[p])) &
    wife = husband~ &
    wife /\ (closure(mother \/ father) ; mother) = {} & husband /\ (closure(mother \/ father) ; father) = {} &
    Man /\ Woman = {}
ASSERTIONS
    #(m).(ownGrandpa(m))
END
