/*@ generated */
MACHINE alloytranslation
SETS
    NaturalNumber_
CONSTANTS
    succ_NaturalNumber, predc_NaturalNumber
DEFINITIONS
    Ordering_(x_,y_) == {x_} <: NaturalNumber_ & {y_} <: NaturalNumber_  &  (succ_NaturalNumber[{x_}] = {y_}) => (predc_NaturalNumber[{y_}] = {x_})
PROPERTIES
    !(x_, y_).({x_} <: NaturalNumber_ & {y_} <: NaturalNumber_ => Ordering_(x_, y_)) &
    succ_NaturalNumber : NaturalNumber_ --> NaturalNumber_ &
    predc_NaturalNumber : NaturalNumber_ +-> NaturalNumber_
OPERATIONS
    check_PeanoAxiomAdapted = PRE not(/* PeanoAxiomAdapted */ !(x_, y_, z_).({x_} <: NaturalNumber_ & {y_} <: NaturalNumber_ & {z_} <: NaturalNumber_ => (({x_} <: closure1(predc_NaturalNumber)[{y_}]) & ({y_} <: closure1(predc_NaturalNumber)[{z_}])) => ({z_} <: closure1(succ_NaturalNumber)[{x_}]))) THEN skip END
END
