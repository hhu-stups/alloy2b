abstract sig Node {
  graph1 : set Node,
  graph2 : set Node,
  p : one Node
}
one sig a,b,c,d,e,f,g,h,i extends Node {}

fact graphdef {
  graph1 = a->c + b->c + c->f + d->f + e->f + h->i + i->h +  f->f + g->g
  graph2 = b->e + c->e + d->e + f->d + g->d + a->i + i->a +  e->e + h->h
}

pred permutation {
  // p is already defined as a total function on Node
  // p is injective:
  // p.~p in iden
  // p is surjective
  univ.p = Node
}

pred isomorph {
  permutation
  all n:Node | n.graph1.p = n.p.graph2
}

run isomorph for 9 Node
