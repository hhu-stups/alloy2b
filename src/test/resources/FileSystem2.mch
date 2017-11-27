MACHINE alloytranslation
SETS
    FSObject
CONSTANTS
    parent, contents, Dir, File, Root
PROPERTIES
    parent : FSObject +-> Dir &
    contents : Dir <-> FSObject &
    Dir <: FSObject &
    File <: FSObject &
    card(Root) = 1 &
    Root <: Dir &
    parent[{Root}] = {} & parent = contents~ & File \/ Dir = FSObject & FSObject : closure(contents)[{Root}] &
    Dir /\ File = {}
ASSERTIONS
    not(#(d).(d : Dir & d : closure1(contents)[{d}])) &
    card({d | d : Dir & parent[{d}] = {}}) = 1 &
    !(f).(f : FSObject => card({d | d : Dir & f : contents[{d}]}) <= 1)
END
