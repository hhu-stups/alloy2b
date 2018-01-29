/*@ generated */
MACHINE alloytranslation
SETS
    Language_; Personell_
CONSTANTS
    french_, german_, spanish_, tom_, david_, jeremy_, carol_, janet_, tracy_, male_, speaks_Personell, assign_Flight
DEFINITIONS
    allLanguages_ ==   !(f_).({f_} <: Flight_ => (speaks_Personell[assign_Flight[{f_}]] = Language_)) ;
    allSexes_ ==   !(f_).({f_} <: Flight_ => (card((assign_Flight[{f_}] - male_)) >= 1 & card((assign_Flight[{f_}] /\ male_)) >= 1)) ;
    scheduleOk_ ==   !(p_, f_).({p_} <: Personell_ & {f_} <: Flight_ => (({p_} <: assign_Flight[{f_}]) & ({p_} <: assign_Flight[next_Flight_(f_)])) => not(({p_} <: assign_Flight[next_Flight_(min(next_Flight_(f_)))]))) ;
    everybodyInSchedule_ ==   (ran(assign_Flight) = Personell_) ;
    crewAlloc_ ==   (((allLanguages_ & allSexes_) & scheduleOk_) & everybodyInSchedule_) ;
    Flight_ == 1..3 ;
    next_Flight_(s) == {x|x=s+1 & x:1..3} ;
    nexts_Flight_(s) == {x|x>s & x:1..3} ;
    prev_Flight_(s) == {x|x=s-1 & x:1..3} ;
    prevs_Flight_(s) == {x|x<s & x:1..3}
PROPERTIES
    {french_} <: Language_ &
    {german_} <: Language_ &
    {spanish_} <: Language_ &
    {tom_} <: Personell_ &
    {david_} <: Personell_ &
    {jeremy_} <: Personell_ &
    {carol_} <: Personell_ &
    {janet_} <: Personell_ &
    {tracy_} <: Personell_ &
    male_ <: Personell_ &
    (male_ = (({tom_} \/ {david_}) \/ {jeremy_})) &
    (speaks_Personell = (((((({tom_} * {german_}) \/ ({david_} * {french_})) \/ ({jeremy_} * {german_})) \/ ({carol_} * {spanish_})) \/ ({janet_} * {french_})) \/ ({tracy_} * {spanish_}))) &
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
    speaks_Personell : Personell_ <-> Language_ &
    assign_Flight : Flight_ <-> Personell_
OPERATIONS
    run_crewAlloc = PRE crewAlloc_ THEN skip END
END
