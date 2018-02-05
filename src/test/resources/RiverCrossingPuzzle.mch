/*@ generated */
MACHINE alloytranslation
SETS
    Object_
CONSTANTS
    Farmer_, Fox_, Chicken_, Grain_, eats_Object, near_State, far_State
DEFINITIONS
    crossRiver_(from_,from__,to_,to__) == from_ <: Object_ & from__ <: Object_ & to_ <: Object_ & to__ <: Object_  &  (card({x_ | {x_} <: from_ & (((from__ = (((from_ - {x_}) - {Farmer_}) - eats_Object[from__]))) & ((to__ = ((to_ \/ {x_}) \/ {Farmer_}))))}) = 1) ;
    next_State_(s) == {x|x=s+1 & x:State_} ;
    nexts_State_(s) == {x|x>s & x:State_} ;
    prev_State_(s) == {x|x=s-1 & x:State_} ;
    prevs_State_(s) == {x|x<s & x:State_} ;
    State_ == 0..7
PROPERTIES
    {Farmer_} <: Object_ &
    {Fox_} <: Object_ &
    {Chicken_} <: Object_ &
    {Grain_} <: Object_ &
    ((eats_Object = (({Fox_} <-> {Chicken_}) \/ ({Chicken_} <-> {Grain_})))) &
    (((near_State[{min(State_)}] = Object_) & far_State[{min(State_)}] = {})) &
    (!(s_, s__).({s_} <: State_ & {s__} <: next_State_(s_) => ((({Farmer_} <: near_State[{s_}]) => crossRiver_(near_State[{s_}], near_State[{s__}], far_State[{s_}], far_State[{s__}])) & (not(({Farmer_} <: near_State[{s_}])) => crossRiver_(far_State[{s_}], far_State[{s__}], near_State[{s_}], near_State[{s__}]))))) &
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
    run_2 = PRE (far_State[{max(State_)}] = Object_) THEN skip END
END
