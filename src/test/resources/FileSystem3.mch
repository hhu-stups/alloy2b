/*@ generated */
MACHINE alloytranslation
SETS
    Object_; Name_; DirEntry_
CONSTANTS
    File_, Dir_, Root_, Cur_, entries_Object, parent_Object, name_DirEntry, contents_DirEntry
DEFINITIONS
    OneParent_buggyVersion_ ==   (!(d_).({d_} <: (Dir_ - {Root_}) => (card(parent_Object[{d_}]) = 1))) ;
    OneParent_correctVersion_ ==   (!(d_).({d_} <: (Dir_ - {Root_}) => ((card(parent_Object[{d_}]) = 1 & card(contents_DirEntry~[{d_}]) = 1)))) ;
    NoDirAliases_ ==   (!(o_).({o_} <: Dir_ => (card(contents_DirEntry~[{o_}]) < 2)))
PROPERTIES
    File_ <: Object_ &
    Dir_ <: Object_ &
    {Root_} <: Dir_ &
    card(Cur_) <= 1 &
    Cur_ <: Dir_ &
    File_ /\ Dir_ = {} &
    {Root_} /\ Cur_ = {} &
    File_ \/ Dir_ = Object_ &
    /* from signature declaration */ !(this_).({this_} <: File_ => (#(d_).({d_} <: Dir_ => (({this_} <: contents_DirEntry[entries_Object[{d_}]]))))) &
    /* from signature declaration */ !(this_).({this_} <: Dir_ => ((parent_Object[{this_}] = entries_Object[{this_}]~[contents_DirEntry~[{this_}]])) & (!(e1_, e2_).({e1_} <: entries_Object & {e2_} <: entries_Object => ((name_DirEntry[{e1_}] = name_DirEntry[{e2_}]) => ({e1_} = {e2_})))) & (not(({this_} <: closure1(parent_Object[{this_}])[{this_}]))) & (not(({this_} = {Root_})) => ({Root_} <: closure1(parent_Object[{this_}])[{this_}]))) &
    /* from signature declaration */ !(this_).({this_} <: {Root_} => (parent_Object[{this_}] = {})) &
    /* from signature declaration */ !(this_).({this_} <: DirEntry_ => (card(entries_Object~[{this_}]) = 1)) &
    entries_Object : Dir_ <-> DirEntry_ &
    parent_Object : Dir_ +-> Dir_ &
    name_DirEntry : DirEntry_ --> Name_ &
    contents_DirEntry : DirEntry_ --> Object_
OPERATIONS
    check_2 = PRE not(OneParent_buggyVersion_ => NoDirAliases_) THEN skip END;
    check_3 = PRE not(OneParent_correctVersion_ => NoDirAliases_) THEN skip END
END
