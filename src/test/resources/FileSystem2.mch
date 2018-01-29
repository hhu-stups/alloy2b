/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_
CONSTANTS
    Dir_, File_, Root_, parent_FSObject, contents_FSObject
PROPERTIES
    Dir_ <: FSObject_ &
    File_ <: FSObject_ &
    {Root_} <: Dir_ &
    parent_FSObject[{Root_}] = {} & (parent_FSObject = contents_FSObject~) & ((File_ \/ Dir_) = FSObject_) & (FSObject_ <: closure(contents_FSObject)[{Root_}]) &
    Dir_ /\ File_ = {} &
    parent_FSObject : FSObject_ +-> Dir_ &
    contents_FSObject : Dir_ <-> FSObject_
OPERATIONS
    check_acyclic = PRE not(/* acyclic */ card({d_ | {d_} <: Dir_ & ({d_} <: closure1(contents_FSObject)[{d_}])}) = 0) THEN skip END;
    check_oneRoot = PRE not(/* oneRoot */ card({d_ | {d_} <: Dir_ & parent_FSObject[{d_}] = {}}) = 1) THEN skip END;
    check_oneLocation = PRE not(/* oneLocation */ !(f_).({f_} <: FSObject_ => card({d_ | {d_} <: Dir_ & ({f_} <: contents_FSObject[{d_}])}) < 2)) THEN skip END
END
