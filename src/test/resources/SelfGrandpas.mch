/*@ generated */
MACHINE alloytranslation
SETS
    Person_
CONSTANTS
    Man_, Woman_, father_Person, mother_Person, wife_Person, husband_Person
DEFINITIONS
    grandpas_(p_) == { temp | {p_} <: Person_ & temp : LET parent_ BE parent_ = (((mother_Person \/ father_Person) \/ (father_Person ; wife_Person)) \/ (mother_Person ; husband_Person)) IN (parent_Person[parent_Person[{p_}]] /\ Man_) END} ;
    ownGrandpa_(m_) == {m_} <: Man_  &  ({m_} <: grandpas_(m_))
PROPERTIES
    Man_ <: Person_ &
    Woman_ <: Person_ &
    not(#(p_).({p_} <: Person_ => ({p_} <: closure1((mother_Person \/ father_Person))[{p_}]))) &
    (wife_Person = husband_Person~) &
    (wife_Person /\ (closure((mother_Person \/ father_Person)) ; mother_Person)) = {} & (husband_Person /\ (closure((mother_Person \/ father_Person)) ; father_Person)) = {} &
    Man_ /\ Woman_ = {} &
    Man_ \/ Woman_ = Person_ &
    father_Person : Person_ +-> Man_ &
    mother_Person : Person_ +-> Woman_ &
    wife_Person : Man_ +-> Woman_ &
    husband_Person : Woman_ +-> Man_
OPERATIONS
    run_ownGrandpa = PRE #(m_).(ownGrandpa_(m_)) THEN skip END
END
