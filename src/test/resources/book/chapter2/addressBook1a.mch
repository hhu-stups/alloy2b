/*@ generated */
MACHINE alloytranslation
SETS
    Name_; Addr_; Book_
CONSTANTS
    addr_Book
DEFINITIONS
    show_ ==   1=1
PROPERTIES
    card(Book_) <= 1 &
    addr_Book : Book_ --> (Name_ +-> Addr_)
OPERATIONS
    run_show = PRE show_ THEN skip END
END
