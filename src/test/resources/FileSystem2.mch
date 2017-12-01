/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_
CONSTANTS
    parent_, contents_, Dir_, File_, Root_
PROPERTIES
    parent_ : FSObject_ +-> Dir_ &
    contents_ : Dir_ <-> FSObject_ &
    Dir_ <: FSObject_ &
    File_ <: FSObject_ &
    card(Root_) = 1 &
    Root_ <: Dir_ &
    parent_[Root_] = {} & parent_ = contents_~ & File_ \/ Dir_ = FSObject_ & FSObject_ <: closure(contents_)[Root_] &
    Dir_ /\ File_ = {}
ASSERTIONS
    card({d_ | {d_} <: Dir_ & {d_} <: closure1(contents_)[{d_}]}) = 0 &
    card({d_ | {d_} <: Dir_ & parent_[{d_}] = {}}) = 1 &
    !(f_).({f_} <: FSObject_ => card({d_ | {d_} <: Dir_ & {f_} <: contents_[{d_}]}) <= 1)
END
