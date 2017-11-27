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
    Root[parent] = {} & parent = contents~ & File \/ Dir = FSObject & FSObject : (Root ; closure(contents)) &
    Dir /\ File = {}
ASSERTIONS
    not(#(d).(d : Dir & d : (d ; closure1(contents)))) &
    card({d | d : Dir & d[parent] = {}}) = 1 &
    !(f).(f : FSObject => card({d | d : Dir & f : d[contents]}) <= 1)
END
