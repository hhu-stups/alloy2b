module Test1
// test that we find the right solutions to this
// Alloy finds one solution for 2 Person and 3 solutions for 3 Person
abstract sig Person {
    likes : one Person
}

fact NoOneLikesOnself {
    no p : Person | p in p.likes
}


pred likes2[m : Person] {
    m in m.likes.likes
}

run likes2 for 3 Person
