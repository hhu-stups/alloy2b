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
    !(d_, o_).(d_ <: Dir_ & o_ <: contents_[d_] => parent_[o_] = d_) &
    File_ \/ Dir_ = FSObject_ &
    card(Root_) = 1 &
    Root_ <: Dir_ &
    FSObject_ <: closure(contents_)[Root_] &
    Dir_ /\ File_ = {}
ASSERTIONS
    not(#(d_).(d_ <: Dir_ & d_ <: closure1(contents_)[d_])) &
    card({d_ | d_ <: Dir_ & parent_[d_] = {}}) = 1 &
    !(o_).(o_ <: FSObject_ => card({d_ | d_ <: Dir_ & o_ <: contents_[d_]}) <= 1)
END
