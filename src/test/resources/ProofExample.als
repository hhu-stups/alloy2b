sig Object {}
sig Vars {
	src,dst : Object
	}

pred move (v, v': Vars, n: Object) {
    v.src+v.dst = Object
    n in v.src
    v'.src = v.src - n
    v'.dst = v.dst + n
	}

assert add_preserves_inv {
	all v, v': Vars, n: Object |
		 move [v,v',n] implies  v'.src+v'.dst = Object
}


check add_preserves_inv for 3 
