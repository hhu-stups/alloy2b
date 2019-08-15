open util/ordering[nephews] as nephewsOrd

abstract sig nephews {}

one sig hughie extends nephews {}
one sig louis extends nephews {}
one sig dewey extends nephews {}

pred p {
 // larger, smaller accept both arguments empty sets
 nephewsOrd/larger[none,none] = nephewsOrd/larger[none,nephews-nephews]
 nephewsOrd/larger[louis-louis,louis-louis] = nephewsOrd/smaller[nephews-nephews,none]
 nephewsOrd/first = nephewsOrd/min[nephews]
 nephewsOrd/last = nephewsOrd/max[nephews]
 nephewsOrd/next[hughie] = louis
 nephewsOrd/next[hughie] != dewey
 nephewsOrd/next[hughie] != hughie
 nephewsOrd/next[louis] = dewey
 nephewsOrd/next[louis] != louis
 nephewsOrd/next[louis] != hughie
 nephewsOrd/next[dewey] = hughie-hughie
 nephewsOrd/prev[dewey] = louis
 nephewsOrd/prev[dewey] != hughie
 nephewsOrd/prev[louis] = hughie
 nephewsOrd/prev[louis] != dewey
 nephewsOrd/prev[hughie] = dewey-dewey
 // lt,gt,gte,lte for empty sets
 !nephewsOrd/lt[louis,hughie-hughie]
 !nephewsOrd/gt[louis,hughie-hughie]
 !nephewsOrd/gt[dewey,hughie-hughie]
 // lhs emptyset is always true
 nephewsOrd/lt[none,none]
 nephewsOrd/lt[none,louis]
 nephewsOrd/lt[louis-louis,louis]
 nephewsOrd/lte[dewey-dewey,hughie]
 nephewsOrd/lte[dewey-dewey,louis]
 nephewsOrd/lte[louis-louis,louis]
 nephewsOrd/gt[dewey-dewey,hughie]
 nephewsOrd/gt[dewey-dewey,louis]
 nephewsOrd/gt[louis-louis,louis]
 nephewsOrd/gte[dewey-dewey,hughie]
 nephewsOrd/gte[dewey-dewey,louis]
 nephewsOrd/gte[louis-louis,louis]
 // also true if rhs is empty
 nephewsOrd/gte[louis-louis,louis-louis]
 nephewsOrd/gte[louis-louis,louis-louis]
 // rhs emptyset is always false unless lhs is empty
 nephewsOrd/lt[dewey-dewey,hughie-hughie]
 nephewsOrd/lt[none,hughie-hughie]
 nephewsOrd/gt[dewey-dewey,hughie-hughie]
 nephewsOrd/gt[hughie-hughie,none]
 nephewsOrd/gte[dewey-dewey,hughie-hughie]
 nephewsOrd/gte[hughie-hughie,hughie-hughie]
 nephewsOrd/lte[dewey-dewey,hughie-hughie]
 nephewsOrd/lte[hughie-hughie,hughie-hughie]
 !nephewsOrd/lt[hughie,dewey-dewey]
 !nephewsOrd/lt[hughie,dewey-dewey]
 !nephewsOrd/lt[dewey,hughie-hughie]
 !nephewsOrd/lt[louis,dewey-dewey]
 !nephewsOrd/lt[louis,louis-louis]
 !nephewsOrd/lte[hughie,none]
 !nephewsOrd/lte[louis,dewey-dewey]
 !nephewsOrd/lte[louis,louis-louis]
 !nephewsOrd/gt[hughie,dewey-dewey]
 !nephewsOrd/gt[louis,none]
 !nephewsOrd/gt[louis,louis-louis]
 !nephewsOrd/gte[hughie,dewey-dewey]
 !nephewsOrd/gte[louis,dewey-dewey]
 !nephewsOrd/gte[louis,louis-louis]
}

run p for 3 nephews
