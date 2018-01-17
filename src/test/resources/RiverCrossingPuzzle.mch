/*@ generated */
MACHINE alloytranslation
SETS
    Object_; State_
CONSTANTS
    ordering_State_, Farmer_, Fox_, Chicken_, Grain_, eats_Object, near_State, far_State
DEFINITIONS
    crossRiver_(from_,from__,to_,to__) == from_ <: Object_ & from__ <: Object_ & to_ <: Object_ & to__ <: Object_  &  card({x_ | {x_} <: from_ & (from__ = (((from_ - {x_}) - {Farmer_}) - eats_Object[from__])) & (to__ = ((to_ \/ {x_}) \/ {Farmer_}))}) = 1
PROPERTIES
    ordering_State_ : seq(State_) &
    {Farmer_} <: Object_ &
    {Fox_} <: Object_ &
    {Chicken_} <: Object_ &
    {Grain_} <: Object_ &
    (eats_Object = (({Fox_} * {Chicken_}) \/ ({Chicken_} * {Grain_}))) &
    ((near_State[first(ordering_UNTYPED_)] = Object_) & far_State[first(ordering_UNTYPED_)] = {}) &
    !(s_, s__).({s_} <: State_ & {s__} <: next_[{s_}] => (({Farmer_} <: near_State[{s_}]) => crossRiver_(near_State[{s_}], near_State[{s__}], far_State[{s_}], far_State[{s__}])) & (not(({Farmer_} <: near_State[{s_}])) => crossRiver_(far_State[{s_}], far_State[{s__}], near_State[{s_}], near_State[{s__}]))) &
    Farmer_ /= Fox_ &
    Farmer_ /= Chicken_ &
    Farmer_ /= Grain_ &
    Fox_ /= Chicken_ &
    Fox_ /= Grain_ &
    Chicken_ /= Grain_ &
    {Farmer_} \/ {Fox_} \/ {Chicken_} \/ {Grain_} = Object_ &
    eats_Object : Object_ <-> Object_ &
    near_State : State_ <-> Object_ &
    far_State : State_ <-> Object_
OPERATIONS
    run_2 = PRE (far_State[first(ordering_UNTYPED_)] = Object_) THEN skip END
END
