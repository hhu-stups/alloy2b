MACHINE alloytranslation
SETS
    Object_, Name_, DirEntry_
CONSTANTS
    File_, entries_, parent_, Dir_, Root_, Cur_, name_, contents_
DEFINITIONS
    OneParent_buggyVersion_ ==  & !(d_).(d_ <: Dir_ - Root_ => card(parent_[d_]) = 1) ;
    OneParent_correctVersion_ ==  & !(d_).(d_ <: Dir_ - Root_ => card(parent_[d_] & card(contents_~[d_]) = 1) = 1) ;
    NoDirAliases_ ==  & !(o_).(o_ <: Dir_ => card(contents_~[o_]) <= 1)
PROPERTIES
    File_ <: Object_ &
    entries_ : Dir_ <-> DirEntry_ &
    parent_ : Dir_ +-> Dir_ &
    Dir_ <: Object_ &
    card(Root_) = 1 &
    Root_ <: Dir_ &
    card(Cur_) <= 1 &
    Cur_ <: Dir_ &
    name_ : DirEntry_ <-> Name_ &
    contents_ : DirEntry_ <-> Object_ &
    File_ /\ Dir_ = {} &
    Root_ /\ Cur_ = {}
ASSERTIONS
    OneParent_buggyVersion_ => NoDirAliases_ &
    OneParent_correctVersion_ => NoDirAliases_
END
