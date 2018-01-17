/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_; FileSystem_
CONSTANTS
    File_, Dir_, live_FileSystem, root_FileSystem, parent_FileSystem, contents_FileSystem
DEFINITIONS
    example_ ==   1=1
PROPERTIES
    File_ <: FSObject_ &
    Dir_ <: FSObject_ &
    File_ /\ Dir_ = {} &
    File_ \/ Dir_ = FSObject_ &
    /* from signature declaration */ !(this_).({this_} <: FileSystem_ => (live_FileSystem[{this_}] <: (root_FileSystem[{this_}] ; closure(contents_FileSystem[{this_}]))) & (parent_FileSystem[{this_}] = contents_FileSystem[{this_}]~)) &
    live_FileSystem : FileSystem_ <-> FSObject_ &
    !(this_).({this_} <: FileSystem_ => root_FileSystem : FileSystem_ --> (Dir_ /\ live_FileSystem[{this_}])) &
    !(this_).({this_} <: FileSystem_ => parent_FileSystem : FileSystem_ --> ((live_FileSystem[{this_}] - root_FileSystem[{this_}]) --> (Dir_ /\ live_FileSystem[{this_}]))) &
    contents_FileSystem : FileSystem_ --> (Dir_ * FSObject_)
OPERATIONS
    run_example = PRE example_ THEN skip END
END
