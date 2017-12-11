/*@ generated */
MACHINE alloytranslation
SETS
    Card_; Suit_
CONSTANTS
    suit_
DEFINITIONS
    ThreeOfAKind_(hand_) == hand_ <: Card_  &  suit_[card(hand_)] = 1 & card(hand_) = 3
PROPERTIES
    suit_ : Card_ --> Suit_
END
