/*@ generated */
MACHINE alloytranslation
SETS
    Language_; Personell_; Flight_
CONSTANTS
    ordering_Flight_, french_, german_, spanish_, tom_, david_, jeremy_, carol_, janet_, tracy_, male_, speaks_, assign_
DEFINITIONS
    allLanguages_ ==   !(f_).({f_} <: Flight_ => speaks_[assign_[{f_}]] = Language_) ;
    allSexes_ ==   !(f_).({f_} <: Flight_ => card(assign_[{f_}] - male_) >= 1 & card(assign_[{f_}] /\ male_) >= 1) ;
    scheduleOk_ ==   !(p_, f_).({p_} <: Personell_ & {f_} <: Flight_ => {p_} <: assign_[{f_}] & {p_} <: assign_[next_(f_)] => not({p_} <: assign_[next_(next_(f_))])) ;
    everybodyInSchedule_ ==   dom(assign_) = Personell_ ;
    crewAlloc_ ==   allLanguages_ & allSexes_ & scheduleOk_ & everybodyInSchedule_
PROPERTIES
    ordering_Flight_ : SEQ(Flight_) &
    {french_} <: Language_ &
    {german_} <: Language_ &
    {spanish_} <: Language_ &
    {tom_} <: Personell_ &
    {david_} <: Personell_ &
    {jeremy_} <: Personell_ &
    {carol_} <: Personell_ &
    {janet_} <: Personell_ &
    {tracy_} <: Personell_ &
    Personell_ <: male_ &
    male_ = {tom_} \/ {david_} \/ {jeremy_} &
    speaks_ = {tom_} * {german_} \/ {david_} * {french_} \/ {jeremy_} * {german_} \/ {carol_} * {spanish_} \/ {janet_} * {french_} \/ {tracy_} * {spanish_} &
    french_ /= german_ &
    french_ /= spanish_ &
    german_ /= spanish_ &
    tom_ /= david_ &
    tom_ /= jeremy_ &
    tom_ /= carol_ &
    tom_ /= janet_ &
    tom_ /= tracy_ &
    david_ /= jeremy_ &
    david_ /= carol_ &
    david_ /= janet_ &
    david_ /= tracy_ &
    jeremy_ /= carol_ &
    jeremy_ /= janet_ &
    jeremy_ /= tracy_ &
    carol_ /= janet_ &
    carol_ /= tracy_ &
    janet_ /= tracy_ &
    {french_} \/ {german_} \/ {spanish_} = Language_ &
    {tom_} \/ {david_} \/ {jeremy_} \/ {carol_} \/ {janet_} \/ {tracy_} = Personell_ &
    speaks_ : Personell_ <-> Language_ &
    assign_ : Flight_ <-> Personell_
OPERATIONS
    run_crewAlloc = PRE crewAlloc_ THEN skip END
END
