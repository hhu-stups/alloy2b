/*@ generated */
MACHINE alloytranslation
SETS
    NaturalNumber_
CONSTANTS
    succ_, predc_
DEFINITIONS
    Ordering_(x_,y_) == {x_} <: NaturalNumber_ & {y_} <: NaturalNumber_  &  succ_[{x_}] = {y_} => predc_[{y_}] = {x_}
PROPERTIES
    succ_ : NaturalNumber_ --> NaturalNumber_ &
    predc_ : NaturalNumber_ +-> NaturalNumber_ &
    !(x_, y_).({x_} <: NaturalNumber_ & {y_} <: NaturalNumber_ => Ordering_(x_, y_))
ASSERTIONS
    /* PeanoAxiomAdapted */ !(x_, y_, z_).({x_} <: NaturalNumber_ & {y_} <: NaturalNumber_ & {z_} <: NaturalNumber_ => {x_} <: closure1(predc_)[{y_}] & {y_} <: closure1(predc_)[{z_}] => {z_} <: closure1(succ_)[{x_}])
END
