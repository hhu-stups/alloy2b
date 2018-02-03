/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_
CONSTANTS
    Dir_, File_, Root_, parent_FSObject, contents_FSObject
PROPERTIES
    Dir_ <: FSObject_ &
    File_ <: FSObject_ &
    (!(d_, o_).({d_} <: Dir_ & {o_} <: contents_FSObject[{d_}] => ((parent_FSObject[{o_}] = {d_})))) &
    (((File_ \/ Dir_) = FSObject_)) &
    {Root_} <: Dir_ &
    ((FSObject_ <: closure(contents_FSObject)[{Root_}])) &
    Dir_ /\ File_ = {} &
    /* from signature declaration */ !(this_).({this_} <: {Root_} => (parent_FSObject[{this_}] = {})) &
    parent_FSObject : FSObject_ +-> Dir_ &
    contents_FSObject : Dir_ <-> FSObject_
OPERATIONS
    check_acyclic = PRE not(/* acyclic */ (not(#(d_).({d_} <: Dir_ =>(({d_} <: closure1(contents_FSObject)[{d_}])))))) THEN skip END;
    check_oneRoot = PRE not(/* oneRoot */ (card({d_ | {d_} <: Dir_ & (parent_FSObject[{d_}] = {})}) = 1)) THEN skip END;
    check_oneLocation = PRE not(/* oneLocation */ (!(o_).({o_} <: FSObject_ => (card({d_ | {d_} <: Dir_ & (({o_} <: contents_FSObject[{d_}]))}) < 2)))) THEN skip END
END
