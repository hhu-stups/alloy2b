/*@ generated */
MACHINE alloytranslation
SETS
    Language_, Personell_, Flight_
CONSTANTS
    french_, german_, spanish_, speaks_, tom_, david_, jeremy_, carol_, janet_, tracy_, male_, assign_
DEFINITIONS
    allLanguages_ ==   !(f_).({f_} <: Flight_ => speaks_[assign_[{f_}]] = Language_) ;
    allSexes_ ==   !(f_).({f_} <: Flight_ => card(assign_[{f_}] - male_) >= 1 & card(assign_[{f_}] /\ male_) >= 1) ;
    scheduleOk_ ==   !(p_, f_).({p_} <: Personell_ & {f_} <: Flight_ => {p_} <: assign_[{f_}] & {p_} <: assign_[next_({f_})] => not({p_} <: assign_[next_(next_({f_}))])) ;
    everybodyInSchedule_ ==   dom(assign_) = Personell_ ;
    crewAlloc_ ==   allLanguages_ & allSexes_ & scheduleOk_ & everybodyInSchedule_
PROPERTIES
    card(french_) = 1 &
    french_ <: Language_ &
    card(german_) = 1 &
    german_ <: Language_ &
    card(spanish_) = 1 &
    spanish_ <: Language_ &
    speaks_ : Personell_ <-> Language_ &
    card(tom_) = 1 &
    tom_ <: Personell_ &
    card(david_) = 1 &
    david_ <: Personell_ &
    card(jeremy_) = 1 &
    jeremy_ <: Personell_ &
    card(carol_) = 1 &
    carol_ <: Personell_ &
    card(janet_) = 1 &
    janet_ <: Personell_ &
    card(tracy_) = 1 &
    tracy_ <: Personell_ &
    Personell_ <: male_ &
    assign_ : Flight_ <-> Personell_ &
    male_ = tom_ \/ david_ \/ jeremy_ &
    speaks_ = tom_ * german_ \/ david_ * french_ \/ jeremy_ * german_ \/ carol_ * spanish_ \/ janet_ * french_ \/ tracy_ * spanish_ &
    french_ /\ german_ = {} &
    french_ /\ spanish_ = {} &
    german_ /\ spanish_ = {} &
    tom_ /\ david_ = {} &
    tom_ /\ jeremy_ = {} &
    tom_ /\ carol_ = {} &
    tom_ /\ janet_ = {} &
    tom_ /\ tracy_ = {} &
    david_ /\ jeremy_ = {} &
    david_ /\ carol_ = {} &
    david_ /\ janet_ = {} &
    david_ /\ tracy_ = {} &
    jeremy_ /\ carol_ = {} &
    jeremy_ /\ janet_ = {} &
    jeremy_ /\ tracy_ = {} &
    carol_ /\ janet_ = {} &
    carol_ /\ tracy_ = {} &
    janet_ /\ tracy_ = {} &
    french_ \/ german_ \/ spanish_ = Language_ &
    tom_ \/ david_ \/ jeremy_ \/ carol_ \/ janet_ \/ tracy_ = Personell_
ASSERTIONS
    crewAlloc_
END
