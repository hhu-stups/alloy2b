sig Name, Addr {}
sig Book {
	addr: Name -> Addr
	}

pred add (b, b': Book, n: Name, a: Addr) {
    no b.addr[n]
	b'.addr = b.addr + n-> a
	}

pred add_wrong (b, b': Book, n: Name, a: Addr) {
	b'.addr = b.addr + n-> a
	}

assert add_preserves_pfun {
	all b, b': Book, n: Name, a: Addr |
		 b.addr in Name -> lone Addr && add [b,b',n,a] implies b'.addr in Name -> lone Addr
	}

assert add_preserves_pfun_false {
	all b, b': Book, n: Name, a: Addr |
		 b.addr in Name -> lone Addr && add_wrong [b,b',n,a] implies b'.addr in Name -> lone Addr
	}


check add_preserves_pfun for 3 
