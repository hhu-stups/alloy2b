/*@ generated */
MACHINE alloytranslation
SETS
    Person_
CONSTANTS
    father_, mother_, wife_, Man_, husband_, Woman_
DEFINITIONS
    grandpas_(p_) == { temp | p_ <: Person_ & temp : LET parent_ BE parent_ = mother_ \/ father_ \/ (father_ ; wife_) \/ (mother_ ; husband_) IN parent_[parent_[p_]] /\ Man_ END} ;
    ownGrandpa_(m_) == m_ <: Man_  &  m_ <: grandpas_(m_)
PROPERTIES
    father_ : Person_ +-> Man_ &
    mother_ : Person_ +-> Woman_ &
    wife_ : Man_ +-> Woman_ &
    Man_ <: Person_ &
    husband_ : Woman_ +-> Man_ &
    Woman_ <: Person_ &
    card({p_ | {p_} <: Person_ & {p_} <: closure1(mother_ \/ father_)[{p_}]}) = 0 &
    wife_ = husband_~ &
    wife_ /\ (closure(mother_ \/ father_) ; mother_) = {} & husband_ /\ (closure(mother_ \/ father_) ; father_) = {} &
    Man_ /\ Woman_ = {} &
    Man_ \/ Woman_ = Person_
ASSERTIONS
    #(m_).(ownGrandpa_(m_))
END
