sig Node {adj: Node -> lone Int}

fact {
   all n:Node |
   let w = n.adj[n] |
     some w => int w = 0
}

