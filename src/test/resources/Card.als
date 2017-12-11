sig Card {suit:Suit}
sig Suit {}
pred ThreeOfAKind (hand: set Card) {
  #hand.suit=1 and #hand=3
}

