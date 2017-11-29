/*@ generated */
MACHINE alloytranslation
SETS
    FSObject_, FileSystem_
CONSTANTS
    File_, Dir_, live_, root_, parent_, contents_
DEFINITIONS
    example_ == 
PROPERTIES
    File_ <: FSObject_ &
    Dir_ <: FSObject_ &
    live_ : FileSystem_ <-> FSObject_ &
    root_ : FileSystem_ <-> Dir_ /\ live_ &
    parent_ : FileSystem_ <-> live_ - root_ --> Dir_ /\ live_ &
    contents_ : FileSystem_ <-> Dir_ * FSObject_ &
    File_ /\ Dir_ = {}
ASSERTIONS
    example_
END
