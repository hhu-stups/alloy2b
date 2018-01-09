/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_
CONSTANTS
    Dir_, File_, Root_, parent_, contents_
PROPERTIES
    Dir_ <: FSObject_ &
    File_ <: FSObject_ &
    {Root_} <: Dir_ &
    parent_[{Root_}] = {} & (parent_ = contents_~) & ((File_ \/ Dir_) = FSObject_) & (FSObject_ <: closure(contents_)[{Root_}]) &
    Dir_ /\ File_ = {} &
    parent_ : FSObject_ +-> Dir_ &
    contents_ : Dir_ <-> FSObject_
OPERATIONS
    check_acyclic = PRE not(/* acyclic */ card({d_ | {d_} <: Dir_ & ({d_} <: closure1(contents_)[{d_}])}) = 0) THEN skip END;
    check_oneRoot = PRE not(/* oneRoot */ card({d_ | {d_} <: Dir_ & parent_[{d_}] = {}}) = 1) THEN skip END;
    check_oneLocation = PRE not(/* oneLocation */ !(f_).({f_} <: FSObject_ => card({d_ | {d_} <: Dir_ & ({f_} <: contents_[{d_}])}) <= 1)) THEN skip END
END
