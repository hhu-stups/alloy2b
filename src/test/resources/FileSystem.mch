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
    !(d, o).(d : Dir & o : (d ; contents) => (o ; parent) = d) &
    File \/ Dir = FSObject &
    card(Root) = 1 &
    Root <: Dir &
    FSObject : Root[closure(contents)] &
    Dir /\ File = {}
ASSERTIONS
    not(#(d).(d : Dir => d : (d ; closure1(contents)))) &
    card({d | d : Dir & (d ; parent) = {}}) = 1 &
    !(o).(o : FSObject => card({d | d : Dir & o : (d ; contents)}) =< 1)
