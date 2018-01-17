/*@ generated */
MACHINE alloytranslation
SETS
    Card_; Suit_
CONSTANTS
    suit_Card
DEFINITIONS
    ThreeOfAKind_(hand_) == hand_ <: Card_  &  ((card(suit_Card[hand_]) = 1) & (card(hand_) = 3))
PROPERTIES
    suit_Card : Card_ --> Suit_
END
