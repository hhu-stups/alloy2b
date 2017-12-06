/*@ generated */
MACHINE alloytranslation
SETS
    Node_
CONSTANTS
    graph1_, graph2_, p_, a_, b_, c_, d_, e_, f_, g_, h_, i_
DEFINITIONS
    permutation_ ==   (p_ ; p_~) <: id(Node_) & dom(p_) = Node_ ;
    isomorph_ ==   permutation_ & !(n_).({n_} <: Node_ => p_[graph1_[{n_}]] = graph2_[p_[{n_}]])
PROPERTIES
    graph1_ : Node_ <-> Node_ &
    graph2_ : Node_ <-> Node_ &
    p_ : Node_ <-> Node_ &
    card(a_) = 1 &
    a_ <: Node_ &
    card(b_) = 1 &
    b_ <: Node_ &
    card(c_) = 1 &
    c_ <: Node_ &
    card(d_) = 1 &
    d_ <: Node_ &
    card(e_) = 1 &
    e_ <: Node_ &
    card(f_) = 1 &
    f_ <: Node_ &
    card(g_) = 1 &
    g_ <: Node_ &
    card(h_) = 1 &
    h_ <: Node_ &
    card(i_) = 1 &
    i_ <: Node_ &
    graph1_ = a_ * c_ \/ b_ * c_ \/ c_ * f_ \/ d_ * f_ \/ e_ * f_ \/ h_ * i_ \/ i_ * h_ \/ f_ * f_ \/ g_ * g_ & graph2_ = b_ * e_ \/ c_ * e_ \/ d_ * e_ \/ f_ * d_ \/ g_ * d_ \/ a_ * i_ \/ i_ * a_ \/ e_ * e_ \/ h_ * h_ &
    a_ /\ b_ = {} &
    a_ /\ c_ = {} &
    a_ /\ d_ = {} &
    a_ /\ e_ = {} &
    a_ /\ f_ = {} &
    a_ /\ g_ = {} &
    a_ /\ h_ = {} &
    a_ /\ i_ = {} &
    b_ /\ c_ = {} &
    b_ /\ d_ = {} &
    b_ /\ e_ = {} &
    b_ /\ f_ = {} &
    b_ /\ g_ = {} &
    b_ /\ h_ = {} &
    b_ /\ i_ = {} &
    c_ /\ d_ = {} &
    c_ /\ e_ = {} &
    c_ /\ f_ = {} &
    c_ /\ g_ = {} &
    c_ /\ h_ = {} &
    c_ /\ i_ = {} &
    d_ /\ e_ = {} &
    d_ /\ f_ = {} &
    d_ /\ g_ = {} &
    d_ /\ h_ = {} &
    d_ /\ i_ = {} &
    e_ /\ f_ = {} &
    e_ /\ g_ = {} &
    e_ /\ h_ = {} &
    e_ /\ i_ = {} &
    f_ /\ g_ = {} &
    f_ /\ h_ = {} &
    f_ /\ i_ = {} &
    g_ /\ h_ = {} &
    g_ /\ i_ = {} &
    h_ /\ i_ = {} &
    a_ \/ b_ \/ c_ \/ d_ \/ e_ \/ f_ \/ g_ \/ h_ \/ i_ = Node_
ASSERTIONS
    isomorph_
END
