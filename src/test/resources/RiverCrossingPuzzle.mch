MACHINE alloytranslation
DEFINITIONS
 crossRiver(from_,from__,to_,to__) == #(x_).(x_ <: from_ & from__ = from_ - x_ - {Farmer_} - eats_[from__] & to__ = to_ \/ x_ \/ {Farmer_});
 next(s) == {successor | #(i).(i:2..size(ordering_) & ordering_(i-1) = s & ordering_(i) = successor)}
SETS
 Object_; State_ = {s1,s2,s3,s4,s5,s6,s7,s8}
CONSTANTS
 Farmer_, Fox_, Chicken_, Grain_, eats_, near_, far_, ordering_
PROPERTIES
 // for exactly 8 state
 card(State_) = 8 &
 // currently, ProB assumes the wrong cardinality for Object
 card(Object_) = 4 &
 // field eats on signature object
 eats_ : POW(Object_ * Object_) & 
 // signatures extend Object
 {Farmer_} <: Object_ &
 {Fox_} <: Object_ &
 {Chicken_} <: Object_ &
 {Grain_} <: Object_ &
 // object is abstract
 {Farmer_} \/ {Fox_} \/ {Chicken_} \/ {Grain_} = Object_ & 
 // subsignatures are disjunct
 {Farmer_} /\ {Fox_} = {} & 
 {Farmer_} /\ {Chicken_} = {} &
 {Farmer_} /\ {Grain_} = {} &
 {Fox_} /\ {Chicken_} = {} &
 {Fox_} /\ {Grain_} = {} &
 {Chicken_} /\ {Grain_} = {} &
 // fact definition in line 9
 eats_ = {(Fox_,Chicken_)} \/ {(Chicken_,Grain_)} &
 // states are ordered
 ordering_ : seq(State_) & size(ordering_) = card(State_) & ordering_ = [s1,s2,s3,s4,s5,s6,s7,s8] &
 // fields near and far on state
 // seq of fields, since util/ordering was added to State
 // card von state ist länge (?)
 near_ : POW(State_ * Object_) &
 far_ : POW(State_ * Object_) &
 // fact definition in line 15
 near_[{first(ordering_)}] = Object_ &  
 far_[{first(ordering_)}] = {} &
 // fact definition in line 26
 !(s_,s__).({s_} <: State_ & {s__} <: next(s_) =>
             (({Farmer_} <: near_[{s_}] => crossRiver(near_[{s_}],near_[{s__}],far_[{s_}],far_[{s__}]) &
              (not({Farmer_} <: near_[{s_}]) => crossRiver(far_[{s_}],far_[{s__}],near_[{s_}],near_[{s__}])))))
END

