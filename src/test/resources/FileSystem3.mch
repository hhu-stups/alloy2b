/*@ generated */
MACHINE alloytranslation
SETS
    Object_; Name_; DirEntry_
CONSTANTS
    File_, entries_, parent_, Dir_, Root_, Cur_, name_, contents_
DEFINITIONS
    OneParent_buggyVersion_ ==   !(d_).({d_} <: Dir_ - {Root_} => card(parent_[{d_}]) = 1) ;
    OneParent_correctVersion_ ==   !(d_).({d_} <: Dir_ - {Root_} => card(parent_[{d_}]) = 1 & card(contents_~[{d_}]) = 1) ;
    NoDirAliases_ ==   !(o_).({o_} <: Dir_ => card(contents_~[{o_}]) <= 1)
PROPERTIES
    File_ <: Object_ &
    entries_ : Dir_ <-> DirEntry_ &
    parent_ : Dir_ +-> Dir_ &
    Dir_ <: Object_ &
    {Root_} <: Dir_ &
    card(Cur_) <= 1 &
    Cur_ <: Dir_ &
    name_ : DirEntry_ --> Name_ &
    contents_ : DirEntry_ --> Object_ &
    File_ /\ Dir_ = {} &
    {Root_} /\ Cur_ = {} &
    File_ \/ Dir_ = Object_ &
    /* from signature declaration */ !(this_).({this_} <: File_ => card({d_ | {d_} <: Dir_ & {this_} <: contents_[entries_[{d_}]]}) > 0) &
    /* from signature declaration */ !(this_).({this_} <: Dir_ => parent_[{this_}] = entries_[{this_}]~[contents_~[{this_}]] & !(e1_, e2_).({e1_} <: entries_ & {e2_} <: entries_ => name_[{e1_}] = name_[{e2_}] => {e1_} = {e2_}) & {this_} <: closure1(parent_[{this_}])[{this_}] & {this_} = {Root_} => {Root_} <: closure1(parent_[{this_}])[{this_}]) &
    /* from signature declaration */ !(this_).({this_} <: {Root_} => parent_[{this_}] = {}) &
    /* from signature declaration */ !(this_).({this_} <: DirEntry_ => card(entries_~[{this_}]) = 1)
ASSERTIONS
    OneParent_buggyVersion_ => NoDirAliases_ &
    OneParent_correctVersion_ => NoDirAliases_
END
