alloy_model(facts([]),assertions([]),commands([]),functions([predicate('this'/'ThreeOfAKind',['hand'],[field('hand',setof('this'/'Card',type(['this'/'Card']),pos(26,3)),pos(20,3))],and([equal(card(join('hand','suit',type(['this'/'Suit']),pos(8,4)),type(['Int']),pos(3,4)),integer(1,pos(14,4)),type([]),pos(13,4)), equal(card('hand',type(['Int']),pos(20,4)),integer(3,pos(26,4)),type([]),pos(25,4))],pos(16,4)),pos(1,3))]),signatures([signature('this'/'Card',[field('suit',oneof('this'/'Suit',type(['this'/'Suit']),pos(16,1)),pos(11,1))],[],[],pos(5,1)),signature('this'/'Suit',[],[],[],pos(5,2))])).