alloy_model(facts([]),assertions([]),commands([]),functions([predicate('this'/'ThreeOfAKind_',['hand_'],[field('hand_',setof('this'/'Card_',type(['this'/'Card_']),pos(26,3)),pos(20,3))],and([equal(card(join('hand_','suit_',type(['this'/'Suit_']),pos(8,4)),type(['Int_']),pos(3,4)),integer(1,pos(14,4)),type([]),pos(13,4)), equal(card('hand_',type(['Int_']),pos(20,4)),integer(3,pos(26,4)),type([]),pos(25,4))],pos(16,4)),pos(1,3))]),signatures([signature('this'/'Card_',[field('suit_',oneof('this'/'Suit_',type(['this'/'Suit_']),pos(16,1)),pos(11,1))],[],[],pos(5,1)),signature('this'/'Suit_',[],[],[],pos(5,2))])).