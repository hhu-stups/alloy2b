/*@ generated */
MACHINE alloytranslation
SETS
    Name_; Addr_
CONSTANTS
    Alias_, Group_, address_Name
DEFINITIONS
    show_ ==   1=1
PROPERTIES
    Alias_ <: Name_ &
    Group_ <: Name_ &
    Alias_ /\ Group_ = {} &
    Alias_ \/ Group_ = Name_ &
    address_Name : Name_ <-> (Addr_ \/ Name_)
OPERATIONS
    run_show = PRE show_ THEN skip END
END
