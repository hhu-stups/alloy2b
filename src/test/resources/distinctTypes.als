abstract sig A {}
sig B {}
sig C { test : set A }

pred distinctTypes {
	some B + C
	no A + C
}

run distinctTypes