/*@ generated */
MACHINE alloytranslation
SETS
    Object_; State_
CONSTANTS
    Farmer_, Fox_, Chicken_, Grain_, eats_, near_, far_
DEFINITIONS
    crossRiver_(from_,from__,to_,to__) == from_ <: Object_ & from__ <: Object_ & to_ <: Object_ & to__ <: Object_  &  card({x_ | {x_} <: from_ & from__ = from_ - {x_} - {Farmer_} - eats_[from__] & to__ = to_ \/ {x_} \/ {Farmer_}}) = 1
PROPERTIES
    {Farmer_} <: Object_ &
    {Fox_} <: Object_ &
    {Chicken_} <: Object_ &
    {Grain_} <: Object_ &
    eats_ = {Fox_} * {Chicken_} \/ {Chicken_} * {Grain_} &
    near_[{first_}] = Object_ & far_[{first_}] = {} &
    !(s_, s__).({s_} <: State_ & {s__} <: next_[{s_}] => ({Farmer_} <: near_[{s_}] => crossRiver_(near_[{s_}], near_[{s__}], far_[{s_}], far_[{s__}])) & (not({Farmer_} <: near_[{s_}]) => crossRiver_(far_[{s_}], far_[{s__}], near_[{s_}], near_[{s__}]))) &
    Farmer_ /= Fox_ &
    Farmer_ /= Chicken_ &
    Farmer_ /= Grain_ &
    Fox_ /= Chicken_ &
    Fox_ /= Grain_ &
    Chicken_ /= Grain_ &
    {Farmer_} \/ {Fox_} \/ {Chicken_} \/ {Grain_} = Object_ &
    eats_ : Object_ <-> Object_ &
    near_ : State_ <-> Object_ &
    far_ : State_ <-> Object_
ASSERTIONS
    far_[{last_}] = Object_
END
