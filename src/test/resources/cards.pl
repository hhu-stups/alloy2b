alloy_model(facts([]),assertions([]),commands([]),functions([predicate('ThreeOfAKind_',[identifier('hand_',type('Card_'),pos(20,3))],[field('hand_',setof(identifier('Card_',type('Card_'),pos(5,1)),type('Card_'),pos(26,3)),type('Card_'),pos(20,3))],and([equal(card(join(identifier('hand_',type('Card_'),pos(20,3)),identifier('suit_',type('Card->this_''Suit_'),pos(11,1)),type('Suit_'),pos(8,4)),type('Int_'),pos(3,4)),integer(1,pos(14,4)),type(untyped),pos(13,4)), equal(card(identifier('hand_',type('Card_'),pos(20,3)),type('Int_'),pos(20,4)),integer(3,pos(26,4)),type(untyped),pos(25,4))],pos(16,4)),pos(1,3))]),signatures([signature('Card_',[field('suit_',oneof(identifier('Suit_',type('Suit_'),pos(5,2)),type('Suit_'),pos(16,1)),type('Suit_'),pos(11,1))],[],[],pos(5,1)),signature('Suit_',[],[],[],pos(5,2))])).