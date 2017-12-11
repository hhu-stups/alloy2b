sig Card {suit:Suit}
sig Suit {}
pred ThreeOfAKind (hand: set Card) {
  #hand.suit=1 and #hand=3
}


pred Test {}

run ThreeOfAKind  for 3
