module FileSystem

sig FSObject {
    parent : lone Dir
}

sig Dir extends FSObject {
    contents : set FSObject
}

sig File extends FSObject {}

one sig Root extends Dir {}

fact {
    no Root.parent
    parent = ~contents
    File + Dir = FSObject
    FSObject in Root.*contents
}

assert acyclic {
    no d : Dir | d in d.^contents
}

assert oneRoot {
    one d : Dir | no d.parent
}

assert oneLocation {
    all f : FSObject | lone d : Dir | f in d.contents
}

check acyclic for 5

check oneRoot for 5

check oneLocation for 5
