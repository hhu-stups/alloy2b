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
    !(d, o).(d <: Dir & o <: contents[d] => parent[o] = d) &
    File \/ Dir = FSObject &
    card(Root) = 1 &
    Root <: Dir &
    FSObject <: closure(contents)[Root] &
    Dir /\ File = {}
ASSERTIONS
    not(#(d).(d <: Dir & d <: closure1(contents)[d])) &
    card({d | d <: Dir & parent[d] = {}}) = 1 &
    !(o).(o <: FSObject => card({d | d <: Dir & o <: contents[d]}) <= 1)
END
