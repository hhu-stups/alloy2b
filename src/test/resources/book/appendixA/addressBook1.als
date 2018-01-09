module appendixA/addressBook1 ----- Page 6

abstract sig Name {
	address: set Addr+Name
	}

sig Alias, Group extends Name { }

sig Addr { }

fact {
	// the invariants should go here
	}

pred show {
	// simulation constraints should go here
	}

run show for 3
