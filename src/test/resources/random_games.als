
// Alloy model from: https://www.hillelwayne.com/post/alloy-randomizer/

open util/ordering[Inventory]

sig Inventory {
  items: set Item
}

abstract sig Chest {
   contains: lone Item
}
one sig A, B, C extends Chest {}

abstract sig Item {}
one sig Bow, Glove, Boots, Sword extends Item {}

abstract sig Puzzle {
  solution: Chest -> set Item
}
one sig PA0, PA1, PB0, PC0 extends Puzzle {}


fact chestRequirements {
  PA0.solution = A -> (Boots + Bow)
  PA1.solution = A -> Sword
  PB0.solution = B -> Bow
  PC0.solution = C -> Glove
}

fun free: set Item {
  {i: Item | no contains.i}
}

fact Trace {
  first.items = free
  all inv: Inventory - last |
    let inv' = inv.next |
      inv'.items = inv.items + {i: Item | reachable[i, inv]}
}

pred reachable(i: Item, inv: Inventory) {
  i in inv.items or
    some p: Puzzle |
      let sol = p.solution[contains.i] |
        some sol and sol in inv.items
}

pred winnable {
  some inv : Inventory |
    reachable[Sword, inv] and reachable[Boots, inv]
}

fact chestsHoldUniqueItems {
  all i: Item | lone c: Chest | c.contains = i
}

pred restrictions {
  all c: Chest | some sol: Puzzle.solution[c] | c.contains not in sol
  Sword in free
}

check {restrictions implies winnable}