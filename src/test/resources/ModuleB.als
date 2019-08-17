module ModuleB

open util/integer
open ModuleA as mod1

sig foo {
 f1: Int
} {
 f1 != mod1/foo.f1
}

assert test { 
 this/foo.f1 = this/foo.f1
}

check test for 5 Int
