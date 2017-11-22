MACHINE alloytranslation
SETS
    FSObject
CONSTANTS
    Dir, File, Root
PROPERTIES
    Dir <: FSObject &
    File <: FSObject &
    Root <: Dir &
    card(Root) = 1 &
    Dir /\ File = {}
