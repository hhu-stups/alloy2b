/*@ generated */
MACHINE alloytranslation
SETS
    setX_; V_
CONSTANTS
    SS_, TT_, VV_, xx_, yy_, EmptyS_, Empty_
PROPERTIES
    card(V_) = 1 &
    (EmptyS_[V_] = {}) &
    (Empty_[V_] = {}) &
    SS_ : V_ --> (setX_ <-> setX_) &
    TT_ : V_ --> (setX_ <-> setX_) &
    VV_ : V_ --> (setX_ <-> setX_) &
    xx_ : V_ <-> setX_ &
    yy_ : V_ <-> setX_ &
    EmptyS_ : V_ <-> setX_ &
    Empty_ : V_ --> (setX_ <-> setX_)
OPERATIONS
    check_SetLaws = PRE not(/* SetLaws */ (((SS_[V_] \/ SS_[V_]) = SS_[V_])) & ((SS_[V_] - SS_[V_]) = {}) & ((SS_[V_] = (SS_[V_] /\ SS_[V_]))) & (((SS_[V_] - Empty_[V_]) = SS_[V_])) & ((Empty_[V_] = (SS_[V_] - SS_[V_]))) & (((Empty_[V_] - SS_[V_]) = Empty_[V_])) & (((SS_[V_] \/ TT_[V_]) = (TT_[V_] \/ SS_[V_]))) & (((SS_[V_] /\ TT_[V_]) = (TT_[V_] /\ SS_[V_]))) & (((SS_[V_] \/ (TT_[V_] \/ VV_[V_])) = ((SS_[V_] \/ TT_[V_]) \/ VV_[V_]))) & (((SS_[V_] /\ (TT_[V_] /\ VV_[V_])) = ((SS_[V_] /\ TT_[V_]) /\ VV_[V_]))) & (((SS_[V_] \/ (TT_[V_] /\ VV_[V_])) = ((SS_[V_] \/ TT_[V_]) /\ (SS_[V_] \/ VV_[V_])))) & (((SS_[V_] /\ (TT_[V_] \/ VV_[V_])) = ((SS_[V_] /\ TT_[V_]) \/ (SS_[V_] /\ VV_[V_])))) & ((((SS_[V_] /\ TT_[V_]) \/ (SS_[V_] - TT_[V_])) = SS_[V_])) & ((((SS_[V_] - TT_[V_]) /\ TT_[V_]) = Empty_[V_])) & (((SS_[V_] - (TT_[V_] - VV_[V_])) = ((SS_[V_] - TT_[V_]) \/ (SS_[V_] /\ VV_[V_])))) & ((((SS_[V_] - TT_[V_]) - VV_[V_]) = (SS_[V_] - (TT_[V_] \/ VV_[V_])))) & (((SS_[V_] \/ (TT_[V_] - VV_[V_])) = ((SS_[V_] \/ TT_[V_]) - (VV_[V_] - SS_[V_])))) & (((SS_[V_] /\ (TT_[V_] - VV_[V_])) = ((SS_[V_] /\ TT_[V_]) - VV_[V_]))) & ((((SS_[V_] \/ TT_[V_]) - VV_[V_]) = ((SS_[V_] - VV_[V_]) \/ (TT_[V_] - VV_[V_])))) & (((SS_[V_] - (TT_[V_] /\ VV_[V_])) = ((SS_[V_] - TT_[V_]) \/ (SS_[V_] - VV_[V_])))) & ((SS_[V_] <: (SS_[V_] \/ TT_[V_]))) & ((TT_[V_] <: (SS_[V_] \/ TT_[V_]))) & (((SS_[V_] <: VV_[V_]) & (TT_[V_] <: VV_[V_])) => ((SS_[V_] \/ TT_[V_]) <: VV_[V_])) & (((SS_[V_] /\ TT_[V_]) <: SS_[V_])) & (((SS_[V_] /\ TT_[V_]) <: TT_[V_])) & (((VV_[V_] <: SS_[V_]) & (VV_[V_] <: TT_[V_])) => (VV_[V_] <: (SS_[V_] /\ TT_[V_]))) & (((SS_[V_] <: VV_[V_]) or not((SS_[V_] <: VV_[V_])))) & ((SS_[V_] <: VV_[V_]) => (card(SS_[V_]) <= card(VV_[V_]))) & ((card(SS_[V_]) < card(VV_[V_])) => not((VV_[V_] <: SS_[V_]))) & ((card((SS_[V_] \/ VV_[V_])) <= (card(SS_[V_]) + card(VV_[V_])))) & ((((SS_[V_] /\ VV_[V_]) = Empty_[V_]) <=> (SS_[V_] <: ((setX_ <-> setX_) - VV_[V_])))) & ((card(SS_[V_]) = 1 <=> (card(SS_[V_]) = 1))) & ((card(SS_[V_]) < 2 <=> (card(SS_[V_]) <= 1))) & ((card(SS_[V_]) > 0 <=> (card(SS_[V_]) > 0))) & (card((SS_[V_] \/ TT_[V_])) = 1 => (card(SS_[V_]) = 1 or card(TT_[V_]) = 1)) & ((card((SS_[V_] \/ TT_[V_])) > 0 <=> (card(SS_[V_]) > 0 or card(TT_[V_]) > 0))) & (card((SS_[V_] /\ TT_[V_])) > 0 => (card(SS_[V_]) > 0 & card(TT_[V_]) > 0)) & (card(SS_[V_]) < 2 => card((SS_[V_] /\ TT_[V_])) < 2) & ((card(SS_[V_]) < 2 & card(TT_[V_]) < 2) => card((SS_[V_] /\ TT_[V_])) < 2) & (card(SS_[V_]) < 2 => card((SS_[V_] - TT_[V_])) < 2) & (card(Empty_[V_]) < 2) & (not(card(Empty_[V_]) > 0)) & (card(SS_[V_]) = 1 => (card((SS_[V_] <-> SS_[V_])) = 1)) & (((card((SS_[V_] <-> SS_[V_])) = 0) <=> SS_[V_] = {})) & (((card((SS_[V_] <-> TT_[V_])) = 0) <=> (SS_[V_] = {} or TT_[V_] = {}))) & ((SS_[V_] <-> SS_[V_])~[EmptyS_[V_]] = {}) & ((SS_[V_] <: (setX_ --> setX_)) => (card(SS_[V_]) = card(setX_))) & ((SS_[V_] <: (setX_ --> setX_)) => (SS_[V_] <: (setX_ +-> setX_))) & (((SS_[V_] <: (setX_ --> setX_)) & (SS_[V_] <: (setX_ >+> setX_))) => (SS_[V_] <: (setX_ >-> setX_)))) THEN skip END
END
