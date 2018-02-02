/*@ generated */
MACHINE alloytranslation
SETS
    Node_
CONSTANTS
    a_, b_, c_, d_, e_, f_, g_, h_, i_, graph1_Node, graph2_Node, p_Node
DEFINITIONS
    permutation_ ==   (((p_Node ; p_Node~) <: id(Node_))) & ((ran(p_Node) = Node_)) ;
    isomorph_ ==   (permutation_) & (!(n_).({n_} <: Node_ => ((p_Node[graph1_Node[{n_}]] = graph2_Node[p_Node[{n_}]]))))
PROPERTIES
    {a_} <: Node_ &
    {b_} <: Node_ &
    {c_} <: Node_ &
    {d_} <: Node_ &
    {e_} <: Node_ &
    {f_} <: Node_ &
    {g_} <: Node_ &
    {h_} <: Node_ &
    {i_} <: Node_ &
    ((graph1_Node = ((((((((({a_} * {c_}) \/ ({b_} * {c_})) \/ ({c_} * {f_})) \/ ({d_} * {f_})) \/ ({e_} * {f_})) \/ ({h_} * {i_})) \/ ({i_} * {h_})) \/ ({f_} * {f_})) \/ ({g_} * {g_})))) & ((graph2_Node = ((((((((({b_} * {e_}) \/ ({c_} * {e_})) \/ ({d_} * {e_})) \/ ({f_} * {d_})) \/ ({g_} * {d_})) \/ ({a_} * {i_})) \/ ({i_} * {a_})) \/ ({e_} * {e_})) \/ ({h_} * {h_})))) &
    a_ /= b_ &
    a_ /= c_ &
    a_ /= d_ &
    a_ /= e_ &
    a_ /= f_ &
    a_ /= g_ &
    a_ /= h_ &
    a_ /= i_ &
    b_ /= c_ &
    b_ /= d_ &
    b_ /= e_ &
    b_ /= f_ &
    b_ /= g_ &
    b_ /= h_ &
    b_ /= i_ &
    c_ /= d_ &
    c_ /= e_ &
    c_ /= f_ &
    c_ /= g_ &
    c_ /= h_ &
    c_ /= i_ &
    d_ /= e_ &
    d_ /= f_ &
    d_ /= g_ &
    d_ /= h_ &
    d_ /= i_ &
    e_ /= f_ &
    e_ /= g_ &
    e_ /= h_ &
    e_ /= i_ &
    f_ /= g_ &
    f_ /= h_ &
    f_ /= i_ &
    g_ /= h_ &
    g_ /= i_ &
    h_ /= i_ &
    {a_} \/ {b_} \/ {c_} \/ {d_} \/ {e_} \/ {f_} \/ {g_} \/ {h_} \/ {i_} = Node_ &
    graph1_Node : Node_ <-> Node_ &
    graph2_Node : Node_ <-> Node_ &
    p_Node : Node_ --> Node_
OPERATIONS
    run_isomorph = PRE isomorph_ THEN skip END
END
