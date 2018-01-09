/*@ generated */
MACHINE alloytranslation
SETS
    Name_; Addr_; Book_
CONSTANTS
    addr_
DEFINITIONS
    show_ ==   1=1
PROPERTIES
    addr_ : Book_ --> Name_ +-> Addr_
OPERATIONS
    run_show = PRE show_ THEN skip END
END
