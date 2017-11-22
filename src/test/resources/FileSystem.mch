MACHINE alloytranslation
SETS
    FSObject
CONSTANTS
    parent, Dir, contents, File, Root
PROPERTIES
    parent : FSObject +-> Dir &
    Dir <: FSObject &
    contents : Dir <-> FSObject &
    File <: FSObject &
    !(d, o).(d : Dir & o : (d ; contents) => (o ; parent) = d) &
    File \/ Dir = FSObject &
    Root <: Dir &
    card(Root) = 1 &
    FSObject : Root[closure(contents)] &
    Dir /\ File = {}
ASSERTIONS
    not(#(d).(d : Dir => d : (d ; closure1(contents))) &
    card({d | d : Dir & (d = {} ; parent)}) = 1 &
    !(o).(o : FSObject => card({d | d : Dir & o : (d ; contents)}) =< 1)
