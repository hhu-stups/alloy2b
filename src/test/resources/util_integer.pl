alloy('unknown',[alloy_model(('unknown',alloy2b_none),facts([]),assertions([fact(pred_call('UtilInteger',[],type(['PrimitiveBoolean'],0),pos(3,80)),(1,79)),fact(pred_call('Comparison',[],type(['PrimitiveBoolean'],0),pos(3,84)),(1,83))]),commands([run('and'([pred_call('UtilInteger',[],type(['PrimitiveBoolean'],0),pos(3,88)), pred_call('Comparison',[],type(['PrimitiveBoolean'],0),pos(19,88))],pos(11,87)),global_scope(-1),exact_scopes([]),upper_bound_scopes([]),bitwidth(5),maxseq(-1),index(0),pos(1,91))]),functions([predicate('UtilInteger',[],[],'and'(['equal'('cast2sigint'('cast2int'('plus'(integer(2,pos(7,4)),integer(1,pos(11,4)),type(['Int'],1),pos(9,4)),type(['Int'],1),pos(2,4)),type(['Int'],1),pos(2,4)),integer(3,pos(17,4)),type(['PrimitiveBoolean'],0),pos(15,4)), 'equal'('cast2sigint'('cast2int'('plus'(integer(1,pos(7,5)),integer(1,pos(11,5)),type(['Int'],1),pos(9,5)),type(['Int'],1),pos(2,5)),type(['Int'],1),pos(2,5)),integer(1,pos(17,5)),type(['PrimitiveBoolean'],0),pos(15,5)), 'equal'(fun_call('integer''plus',['cast2sigint'('cast2int'('plus'(integer(1,pos(12,7)),integer(3,pos(16,7)),type(['Int'],1),pos(14,7)),type(['Int'],1),pos(7,7)),type(['Int'],1),pos(7,7)), 'cast2sigint'(integer(2,pos(21,7)),type(['Int'],1),pos(21,7))],type(['Int'],1),pos(2,7)),integer(6,pos(26,7)),type(['PrimitiveBoolean'],0),pos(24,7)), 'equal'(fun_call('integer''plus',['cast2sigint'(integer(1,pos(7,8)),type(['Int'],1),pos(7,8)), 'plus'(integer(2,pos(11,8)),integer(3,pos(15,8)),type(['Int'],1),pos(13,8))],type(['Int'],1),pos(2,8)),integer(6,pos(21,8)),type(['PrimitiveBoolean'],0),pos(19,8)), 'equal'(fun_call('integer''plus',['cast2sigint'(integer(1,pos(8,9)),type(['Int'],1),pos(7,9)), 'plus'(integer(2,pos(13,9)),integer(3,pos(17,9)),type(['Int'],1),pos(15,9))],type(['Int'],1),pos(2,9)),integer(6,pos(23,9)),type(['PrimitiveBoolean'],0),pos(21,9)), 'equal'(fun_call('integer''plus',['cast2sigint'(integer(1,pos(8,10)),type(['Int'],1),pos(7,10)), 'plus'('plus'(integer(2,pos(13,10)),integer(3,pos(17,10)),type(['Int'],1),pos(15,10)),integer(4,pos(21,10)),type(['Int'],1),pos(19,10))],type(['Int'],1),pos(2,10)),integer(10,pos(27,10)),type(['PrimitiveBoolean'],0),pos(25,10)), 'equal'(fun_call('integer''plus',['plus'(integer(1,pos(8,11)),integer(5,pos(12,11)),type(['Int'],1),pos(10,11)), 'plus'('plus'(integer(2,pos(17,11)),integer(3,pos(21,11)),type(['Int'],1),pos(19,11)),integer(4,pos(25,11)),type(['Int'],1),pos(23,11))],type(['Int'],1),pos(2,11)),integer(15,pos(31,11)),type(['PrimitiveBoolean'],0),pos(29,11)), 'equal'(fun_call('integer''plus',['plus'(integer(1,pos(4,12)),integer(5,pos(8,12)),type(['Int'],1),pos(6,12)), 'plus'('plus'(integer(2,pos(18,12)),integer(3,pos(22,12)),type(['Int'],1),pos(20,12)),integer(4,pos(26,12)),type(['Int'],1),pos(24,12))],type(['Int'],1),pos(12,12)),integer(15,pos(32,12)),type(['PrimitiveBoolean'],0),pos(30,12)), 'equal'(fun_call('integer''add',['plus'(integer(1,pos(7,15)),integer(3,pos(11,15)),type(['Int'],1),pos(9,15)), 'cast2sigint'(integer(2,pos(15,15)),type(['Int'],1),pos(15,15))],type(['Int'],1),pos(2,15)),integer(6,pos(20,15)),type(['PrimitiveBoolean'],0),pos(18,15)), 'equal'(fun_call('integer''add',['cast2sigint'(integer(1,pos(6,16)),type(['Int'],1),pos(6,16)), 'plus'(integer(2,pos(10,16)),integer(3,pos(14,16)),type(['Int'],1),pos(12,16))],type(['Int'],1),pos(2,16)),integer(6,pos(20,16)),type(['PrimitiveBoolean'],0),pos(18,16)), 'equal'(fun_call('integer''add',['cast2sigint'(integer(1,pos(7,17)),type(['Int'],1),pos(6,17)), 'plus'(integer(2,pos(12,17)),integer(3,pos(16,17)),type(['Int'],1),pos(14,17))],type(['Int'],1),pos(2,17)),integer(6,pos(22,17)),type(['PrimitiveBoolean'],0),pos(20,17)), 'equal'(fun_call('integer''add',['cast2sigint'(integer(1,pos(7,18)),type(['Int'],1),pos(6,18)), 'plus'('plus'(integer(2,pos(12,18)),integer(3,pos(16,18)),type(['Int'],1),pos(14,18)),integer(4,pos(20,18)),type(['Int'],1),pos(18,18))],type(['Int'],1),pos(2,18)),integer(10,pos(26,18)),type(['PrimitiveBoolean'],0),pos(24,18)), 'equal'(fun_call('integer''add',['plus'(integer(1,pos(7,19)),integer(5,pos(11,19)),type(['Int'],1),pos(9,19)), 'plus'('plus'(integer(2,pos(16,19)),integer(3,pos(20,19)),type(['Int'],1),pos(18,19)),integer(4,pos(24,19)),type(['Int'],1),pos(22,19))],type(['Int'],1),pos(2,19)),integer(15,pos(30,19)),type(['PrimitiveBoolean'],0),pos(28,19)), 'equal'(fun_call('integer''add',['plus'(integer(1,pos(4,20)),integer(5,pos(8,20)),type(['Int'],1),pos(6,20)), 'plus'('plus'(integer(2,pos(17,20)),integer(3,pos(21,20)),type(['Int'],1),pos(19,20)),integer(4,pos(25,20)),type(['Int'],1),pos(23,20))],type(['Int'],1),pos(12,20)),integer(15,pos(31,20)),type(['PrimitiveBoolean'],0),pos(29,20)), 'not_equal'(fun_call('integer''sub',['plus'(integer(1,pos(7,22)),integer(2,pos(11,22)),type(['Int'],1),pos(9,22)), 'cast2sigint'(integer(2,pos(14,22)),type(['Int'],1),pos(14,22))],type(['Int'],1),pos(2,22)),fun_call('integer''min',[],type(['Int'],1),pos(20,22)),type(['PrimitiveBoolean'],0),pos(17,22)), 'equal'(fun_call('integer''sub',['plus'(integer(1,pos(7,23)),integer(3,pos(11,23)),type(['Int'],1),pos(9,23)), 'cast2sigint'(integer(2,pos(14,23)),type(['Int'],1),pos(14,23))],type(['Int'],1),pos(2,23)),integer(2,pos(19,23)),type(['PrimitiveBoolean'],0),pos(17,23)), 'equal'(fun_call('integer''sub',['plus'(integer(1,pos(7,24)),integer(5,pos(11,24)),type(['Int'],1),pos(9,24)), 'plus'(integer(2,pos(15,24)),integer(1,pos(19,24)),type(['Int'],1),pos(17,24))],type(['Int'],1),pos(2,24)),integer(3,pos(25,24)),type(['PrimitiveBoolean'],0),pos(23,24)), 'equal'(fun_call('integer''mul',['cast2sigint'(integer(3,pos(6,27)),type(['Int'],1),pos(6,27)), 'cast2sigint'(integer(2,pos(8,27)),type(['Int'],1),pos(8,27))],type(['Int'],1),pos(2,27)),integer(6,pos(13,27)),type(['PrimitiveBoolean'],0),pos(11,27)), 'equal'(fun_call('integer''mul',['plus'(integer(3,pos(6,28)),integer(1,pos(8,28)),type(['Int'],1),pos(7,28)), 'cast2sigint'(integer(2,pos(10,28)),type(['Int'],1),pos(10,28))],type(['Int'],1),pos(2,28)),integer(8,pos(15,28)),type(['PrimitiveBoolean'],0),pos(13,28)), 'equal'(fun_call('integer''mul',['plus'(integer(3,pos(7,29)),integer(1,pos(11,29)),type(['Int'],1),pos(9,29)), 'cast2sigint'(integer(2,pos(15,29)),type(['Int'],1),pos(15,29))],type(['Int'],1),pos(2,29)),integer(8,pos(20,29)),type(['PrimitiveBoolean'],0),pos(18,29)), 'equal'(fun_call('integer''mul',['plus'(integer(3,pos(7,30)),integer(2,pos(11,30)),type(['Int'],1),pos(9,30)), 'plus'(integer(1,pos(16,30)),integer(2,pos(20,30)),type(['Int'],1),pos(18,30))],type(['Int'],1),pos(2,30)),integer(15,pos(26,30)),type(['PrimitiveBoolean'],0),pos(24,30)), 'equal'(fun_call('integer''div',['cast2sigint'(integer(4,pos(6,32)),type(['Int'],1),pos(6,32)), 'cast2sigint'(integer(2,pos(9,32)),type(['Int'],1),pos(9,32))],type(['Int'],1),pos(2,32)),integer(2,pos(14,32)),type(['PrimitiveBoolean'],0),pos(12,32)), 'equal'(fun_call('integer''div',['plus'(integer(1,pos(7,33)),integer(3,pos(11,33)),type(['Int'],1),pos(9,33)), 'cast2sigint'(integer(2,pos(15,33)),type(['Int'],1),pos(15,33))],type(['Int'],1),pos(2,33)),integer(2,pos(20,33)),type(['PrimitiveBoolean'],0),pos(18,33)), 'equal'(fun_call('integer''div',['plus'(integer(1,pos(7,34)),integer(3,pos(11,34)),type(['Int'],1),pos(9,34)), 'plus'(integer(1,pos(16,34)),integer(1,pos(20,34)),type(['Int'],1),pos(18,34))],type(['Int'],1),pos(2,34)),integer(4,pos(26,34)),type(['PrimitiveBoolean'],0),pos(24,34)), 'equal'(fun_call('integer''div',['plus'(integer(1,pos(7,35)),integer(3,pos(11,35)),type(['Int'],1),pos(9,35)), 'plus'(integer(2,pos(16,35)),integer(0,pos(20,35)),type(['Int'],1),pos(18,35))],type(['Int'],1),pos(2,35)),integer(2,pos(26,35)),type(['PrimitiveBoolean'],0),pos(24,35)), 'equal'(fun_call('integer''rem',['cast2sigint'(integer(4,pos(6,37)),type(['Int'],1),pos(6,37)), 'cast2sigint'(integer(2,pos(9,37)),type(['Int'],1),pos(9,37))],type(['Int'],1),pos(2,37)),integer(0,pos(14,37)),type(['PrimitiveBoolean'],0),pos(12,37)), 'equal'(fun_call('integer''rem',['cast2sigint'(integer(4,pos(6,38)),type(['Int'],1),pos(6,38)), 'cast2sigint'(integer(3,pos(9,38)),type(['Int'],1),pos(9,38))],type(['Int'],1),pos(2,38)),integer(1,pos(14,38)),type(['PrimitiveBoolean'],0),pos(12,38)), 'equal'(fun_call('integer''rem',['plus'(integer(1,pos(7,39)),integer(3,pos(11,39)),type(['Int'],1),pos(9,39)), 'cast2sigint'(integer(2,pos(15,39)),type(['Int'],1),pos(15,39))],type(['Int'],1),pos(2,39)),integer(0,pos(20,39)),type(['PrimitiveBoolean'],0),pos(18,39)), 'equal'(fun_call('integer''rem',['plus'(integer(4,pos(7,40)),integer(3,pos(11,40)),type(['Int'],1),pos(9,40)), 'plus'(integer(1,pos(16,40)),integer(2,pos(20,40)),type(['Int'],1),pos(18,40))],type(['Int'],1),pos(2,40)),integer(1,pos(26,40)),type(['PrimitiveBoolean'],0),pos(24,40))],pos(1,1)),pos(1,3)),predicate('Comparison',[],[],'and'([pred_call('integer''lt',['cast2sigint'(integer(1,pos(6,45)),type(['Int'],1),pos(5,45)), 'cast2sigint'(integer(2,pos(10,45)),type(['Int'],1),pos(9,45))],type(['PrimitiveBoolean'],0),pos(2,45)), pred_call('integer''lt',['cast2sigint'(integer(2,pos(6,46)),type(['Int'],1),pos(5,46)), 'plus'(integer(1,pos(10,46)),integer(3,pos(14,46)),type(['Int'],1),pos(12,46))],type(['PrimitiveBoolean'],0),pos(2,46)), 'not'(pred_call('integer''gt',['cast2sigint'(integer(4,pos(10,47)),type(['Int'],1),pos(9,47)), 'plus'(integer(1,pos(14,47)),integer(3,pos(18,47)),type(['Int'],1),pos(16,47))],type(['PrimitiveBoolean'],0),pos(6,47)),type(['PrimitiveBoolean'],0),pos(2,47)), 'not'(pred_call('integer''lt',['plus'('plus'(integer(2,pos(10,48)),integer(3,pos(14,48)),type(['Int'],1),pos(12,48)),integer(4,pos(18,48)),type(['Int'],1),pos(16,48)), 'plus'('plus'(integer(1,pos(22,48)),integer(3,pos(26,48)),type(['Int'],1),pos(24,48)),integer(1,pos(30,48)),type(['Int'],1),pos(28,48))],type(['PrimitiveBoolean'],0),pos(6,48)),type(['PrimitiveBoolean'],0),pos(2,48)), pred_call('integer''lte',['plus'('plus'(integer(2,pos(7,49)),integer(3,pos(11,49)),type(['Int'],1),pos(9,49)),integer(4,pos(15,49)),type(['Int'],1),pos(13,49)), 'plus'('plus'(integer(1,pos(19,49)),integer(6,pos(23,49)),type(['Int'],1),pos(21,49)),integer(2,pos(27,49)),type(['Int'],1),pos(25,49))],type(['PrimitiveBoolean'],0),pos(2,49)), pred_call('integer''gte',['plus'('plus'(integer(2,pos(7,50)),integer(3,pos(11,50)),type(['Int'],1),pos(9,50)),integer(4,pos(15,50)),type(['Int'],1),pos(13,50)), 'plus'('plus'(integer(1,pos(19,50)),integer(7,pos(23,50)),type(['Int'],1),pos(21,50)),integer(1,pos(27,50)),type(['Int'],1),pos(25,50))],type(['PrimitiveBoolean'],0),pos(2,50)), pred_call('integer''eq',['plus'('plus'(integer(2,pos(6,51)),integer(3,pos(10,51)),type(['Int'],1),pos(8,51)),integer(4,pos(14,51)),type(['Int'],1),pos(12,51)), 'plus'('plus'(integer(1,pos(18,51)),integer(6,pos(22,51)),type(['Int'],1),pos(20,51)),integer(2,pos(26,51)),type(['Int'],1),pos(24,51))],type(['PrimitiveBoolean'],0),pos(2,51)), pred_call('integer''lt',['intersection'('cast2sigint'(integer(1,pos(6,53)),type(['Int'],1),pos(5,53)),'cast2sigint'(integer(2,pos(10,53)),type(['Int'],1),pos(9,53)),type(['Int'],1),pos(8,53)), 'cast2sigint'(integer(1,pos(14,53)),type(['Int'],1),pos(13,53))],type(['PrimitiveBoolean'],0),pos(2,53)), pred_call('integer''lte',['intersection'('cast2sigint'(integer(1,pos(7,54)),type(['Int'],1),pos(6,54)),'cast2sigint'(integer(2,pos(11,54)),type(['Int'],1),pos(10,54)),type(['Int'],1),pos(9,54)), 'cast2sigint'(integer(0,pos(15,54)),type(['Int'],1),pos(14,54))],type(['PrimitiveBoolean'],0),pos(2,54)), pred_call('integer''gte',['intersection'('cast2sigint'(integer(1,pos(7,55)),type(['Int'],1),pos(6,55)),'cast2sigint'(integer(2,pos(11,55)),type(['Int'],1),pos(10,55)),type(['Int'],1),pos(9,55)), 'cast2sigint'(integer(0,pos(15,55)),type(['Int'],1),pos(14,55))],type(['PrimitiveBoolean'],0),pos(2,55)), pred_call('integer''eq',['intersection'('cast2sigint'(integer(1,pos(6,56)),type(['Int'],1),pos(5,56)),'cast2sigint'(integer(2,pos(10,56)),type(['Int'],1),pos(9,56)),type(['Int'],1),pos(8,56)), 'cast2sigint'(integer(0,pos(14,56)),type(['Int'],1),pos(13,56))],type(['PrimitiveBoolean'],0),pos(2,56)), 'equal'('plus'('intersection'('cast2sigint'(integer(1,pos(3,57)),type(['Int'],1),pos(2,57)),'cast2sigint'(integer(2,pos(7,57)),type(['Int'],1),pos(6,57)),type(['Int'],1),pos(5,57)),integer(3,pos(13,57)),type(['Int'],1),pos(10,57)),integer(3,pos(18,57)),type(['PrimitiveBoolean'],0),pos(16,57)), 'equal'(fun_call('integer''minus',['intersection'('cast2sigint'(integer(1,pos(9,58)),type(['Int'],1),pos(8,58)),'cast2sigint'(integer(2,pos(13,58)),type(['Int'],1),pos(12,58)),type(['Int'],1),pos(11,58)), 'intersection'('cast2sigint'(integer(2,pos(17,58)),type(['Int'],1),pos(16,58)),'cast2sigint'(integer(3,pos(21,58)),type(['Int'],1),pos(20,58)),type(['Int'],1),pos(19,58))],type(['Int'],1),pos(2,58)),integer(0,pos(27,58)),type(['PrimitiveBoolean'],0),pos(25,58)), 'equal'(fun_call('integer''plus',['intersection'('cast2sigint'(integer(1,pos(8,59)),type(['Int'],1),pos(7,59)),'cast2sigint'(integer(2,pos(12,59)),type(['Int'],1),pos(11,59)),type(['Int'],1),pos(10,59)), 'intersection'('cast2sigint'(integer(2,pos(16,59)),type(['Int'],1),pos(15,59)),'cast2sigint'(integer(3,pos(20,59)),type(['Int'],1),pos(19,59)),type(['Int'],1),pos(18,59))],type(['Int'],1),pos(2,59)),integer(0,pos(26,59)),type(['PrimitiveBoolean'],0),pos(24,59)), 'equal'(fun_call('integer''plus',[identifier('none',type(['none'],1),pos(1,1)), identifier('none',type(['none'],1),pos(1,1))],type(['Int'],1),pos(2,60)),integer(0,pos(20,60)),type(['PrimitiveBoolean'],0),pos(18,60)), 'equal'(fun_call('integer''signum',['cast2sigint'(integer(1,pos(9,62)),type(['Int'],1),pos(9,62))],type(['Int'],1),pos(2,62)),integer(1,pos(14,62)),type(['PrimitiveBoolean'],0),pos(12,62)), 'equal'(fun_call('integer''signum',['cast2sigint'(integer(7,pos(9,63)),type(['Int'],1),pos(9,63))],type(['Int'],1),pos(2,63)),integer(1,pos(14,63)),type(['PrimitiveBoolean'],0),pos(12,63)), 'equal'(fun_call('integer''signum',['cast2sigint'(integer(-1,pos(9,64)),type(['Int'],1),pos(9,64))],type(['Int'],1),pos(2,64)),integer(-1,pos(15,64)),type(['PrimitiveBoolean'],0),pos(13,64)), 'equal'(fun_call('integer''signum',['cast2sigint'(integer(-5,pos(9,65)),type(['Int'],1),pos(9,65))],type(['Int'],1),pos(2,65)),integer(-1,pos(15,65)),type(['PrimitiveBoolean'],0),pos(13,65)), 'equal'(fun_call('integer''signum',['cast2sigint'(integer(0,pos(9,66)),type(['Int'],1),pos(9,66))],type(['Int'],1),pos(2,66)),integer(0,pos(14,66)),type(['PrimitiveBoolean'],0),pos(12,66)), 'equal'(fun_call('integer''signum',[fun_call('integer''minus',['cast2sigint'(integer(0,pos(15,67)),type(['Int'],1),pos(15,67)), 'cast2sigint'(integer(1,pos(17,67)),type(['Int'],1),pos(17,67))],type(['Int'],1),pos(9,67))],type(['Int'],1),pos(2,67)),integer(-1,pos(23,67)),type(['PrimitiveBoolean'],0),pos(21,67)), 'greater'('cast2int'('intersection'('cast2sigint'(integer(1,pos(3,69)),type(['Int'],1),pos(2,69)),'cast2sigint'(integer(2,pos(7,69)),type(['Int'],1),pos(6,69)),type(['Int'],1),pos(5,69)),type(['Int'],1),pos(2,69)),integer(-1,pos(12,69)),type(['PrimitiveBoolean'],0),pos(10,69)), pred_call('integer''gt',['cast2sigint'(integer(2,pos(5,70)),type(['Int'],1),pos(5,70)), 'cast2sigint'(integer(1,pos(8,70)),type(['Int'],1),pos(8,70))],type(['PrimitiveBoolean'],0),pos(2,70)), pred_call('integer''gt',['plus'(integer(4,pos(6,71)),integer(3,pos(10,71)),type(['Int'],1),pos(8,71)), 'plus'('plus'(integer(3,pos(15,71)),integer(2,pos(19,71)),type(['Int'],1),pos(17,71)),integer(1,pos(23,71)),type(['Int'],1),pos(21,71))],type(['PrimitiveBoolean'],0),pos(2,71)), 'greater'('cast2int'('plus'(integer(4,pos(3,72)),integer(3,pos(7,72)),type(['Int'],1),pos(5,72)),type(['Int'],1),pos(2,72)),'cast2int'('plus'('plus'(integer(3,pos(13,72)),integer(2,pos(17,72)),type(['Int'],1),pos(15,72)),integer(1,pos(21,72)),type(['Int'],1),pos(19,72)),type(['Int'],1),pos(12,72)),type(['PrimitiveBoolean'],0),pos(10,72)), 'equal'(fun_call('integer''smaller',['cast2sigint'(integer(1,pos(10,74)),type(['Int'],1),pos(10,74)), 'cast2sigint'(integer(2,pos(12,74)),type(['Int'],1),pos(12,74))],type(['Int'],1),pos(2,74)),integer(1,pos(17,74)),type(['PrimitiveBoolean'],0),pos(15,74)), 'equal'(fun_call('integer''smaller',['plus'(integer(1,pos(11,75)),integer(2,pos(15,75)),type(['Int'],1),pos(13,75)), 'cast2sigint'(integer(2,pos(18,75)),type(['Int'],1),pos(18,75))],type(['Int'],1),pos(2,75)),integer(2,pos(23,75)),type(['PrimitiveBoolean'],0),pos(21,75)), 'equal'(fun_call('integer''smaller',['plus'(integer(1,pos(11,76)),integer(2,pos(15,76)),type(['Int'],1),pos(13,76)), 'plus'(integer(2,pos(19,76)),integer(3,pos(23,76)),type(['Int'],1),pos(21,76))],type(['Int'],1),pos(2,76)),integer(3,pos(29,76)),type(['PrimitiveBoolean'],0),pos(27,76))],pos(1,1)),pos(1,43)),predicate('eval',[],[],'and'([pred_call('UtilInteger',[],type(['PrimitiveBoolean'],0),pos(3,88)), pred_call('Comparison',[],type(['PrimitiveBoolean'],0),pos(19,88))],pos(15,88)),pos(1,87))]),signatures([]),ordered_signatures([]),[sequences:false]),alloy_model(('util''integer','integer'),facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field(identifier('n1',type(['Int'],1),pos(11,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field(identifier('n2',type(['Int'],1),pos(15,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field(identifier('n1',type(['Int'],1),pos(11,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field(identifier('n2',type(['Int'],1),pos(15,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'cast2sigint'('integer''plus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,12)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,12)),type(['Int'],1),pos(30,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field(identifier('n1',type(['Int'],1),pos(12,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field(identifier('n2',type(['Int'],1),pos(16,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field(identifier('n1',type(['Int'],1),pos(12,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field(identifier('n2',type(['Int'],1),pos(16,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'cast2sigint'('integer''minus'('cast2int'(identifier('n1',type(['Int'],1),pos(12,15)),type(['Int'],1),pos(33,15)),'cast2int'(identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(44,15)),type(['Int'],1),pos(36,15)),type(['Int'],1),pos(31,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field(identifier('n1',type(['Int'],1),pos(10,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field(identifier('n2',type(['Int'],1),pos(14,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'cast2sigint'('closure'('cast2int'(identifier('n1',type(['Int'],1),pos(10,17)),type(['Int'],1),pos(31,17)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(42,17)),type(['Int'],1),pos(34,17)),type(['Int'],1),pos(29,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field(identifier('n1',type(['Int'],1),pos(10,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field(identifier('n2',type(['Int'],1),pos(14,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'cast2sigint'('integer''div'('cast2int'(identifier('n1',type(['Int'],1),pos(10,25)),type(['Int'],1),pos(31,25)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(42,25)),type(['Int'],1),pos(34,25)),type(['Int'],1),pos(29,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field(identifier('n1',type(['Int'],1),pos(10,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field(identifier('n2',type(['Int'],1),pos(14,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'cast2sigint'('integer''rem'('cast2int'(identifier('n1',type(['Int'],1),pos(10,28)),type(['Int'],1),pos(31,28)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(42,28)),type(['Int'],1),pos(34,28)),type(['Int'],1),pos(29,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field(identifier('n',type(['Int'],1),pos(13,31)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'cast2sigint'('integer''minus'(integer(0,pos(29,31)),'cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),type(['Int'],1),pos(31,31)),type(['Int'],1),pos(27,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field(identifier('n1',type(['Int'],1),pos(10,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field(identifier('n2',type(['Int'],1),pos(14,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'('cast2sigint'('cast2int'(identifier('n1',type(['Int'],1),pos(10,34)),type(['Int'],1),pos(25,34)),type(['Int'],1),pos(25,34)),'cast2sigint'('cast2int'(identifier('n2',type(['Int'],1),pos(14,34)),type(['Int'],1),pos(35,34)),type(['Int'],1),pos(35,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field(identifier('n1',type(['Int'],1),pos(10,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field(identifier('n2',type(['Int'],1),pos(14,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'('cast2int'(identifier('n1',type(['Int'],1),pos(10,37)),type(['Int'],1),pos(25,37)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,37)),type(['Int'],1),pos(30,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field(identifier('n1',type(['Int'],1),pos(10,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field(identifier('n2',type(['Int'],1),pos(14,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'('cast2int'(identifier('n1',type(['Int'],1),pos(10,40)),type(['Int'],1),pos(25,40)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,40)),type(['Int'],1),pos(30,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field(identifier('n1',type(['Int'],1),pos(11,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field(identifier('n2',type(['Int'],1),pos(15,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,43)),type(['Int'],1),pos(26,43)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,43)),type(['Int'],1),pos(32,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field(identifier('n1',type(['Int'],1),pos(11,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field(identifier('n2',type(['Int'],1),pos(15,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,46)),type(['Int'],1),pos(26,46)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,46)),type(['Int'],1),pos(32,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field(identifier('n',type(['Int'],1),pos(12,49)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field(identifier('n',type(['Int'],1),pos(12,52)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'('cast2int'(identifier('n',type(['Int'],1),pos(12,52)),type(['Int'],1),pos(22,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field(identifier('n',type(['Int'],1),pos(12,55)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'('cast2int'(identifier('n',type(['Int'],1),pos(12,55)),type(['Int'],1),pos(22,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field(identifier('n',type(['Int'],1),pos(14,58)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,58)),type(['Int'],1),pos(24,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field(identifier('n',type(['Int'],1),pos(14,61)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,61)),type(['Int'],1),pos(24,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field(identifier('n',type(['Int'],1),pos(13,64)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(29,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'integer''minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(56,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field(identifier('i',type(['Int'],1),pos(14,71)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field(identifier('next',type(['univ', 'univ'],2),pos(22,71)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field(identifier('s',type(['univ'],1),pos(40,71)),'setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'([identifier('e',type(['univ'],1),pos(3,72))],[field(identifier('e',type(['univ'],1),pos(4,72)),'oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),'cast2sigint'('cast2int'(identifier('i',type(['Int'],1),pos(14,71)),type(['Int'],1),pos(22,72)),type(['Int'],1),pos(22,72)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field(identifier('e',type(['univ'],1),pos(14,80)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field(identifier('next',type(['univ', 'univ'],2),pos(23,80)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'cast2sigint'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),type(['Int'],1),pos(52,80)),pos(1,80)),function('integer''max',[],[],'cast2sigint'(integer(max,pos(19,85)),type(['Int'],1),pos(17,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field(identifier('es',type(['Int'],1),pos(10,97)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],'cast2sigint'(integer(min,pos(19,88)),type(['Int'],1),pos(17,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field(identifier('es',type(['Int'],1),pos(10,100)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field(identifier('e',type(['Int'],1),pos(12,103)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field(identifier('e',type(['Int'],1),pos(12,106)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field(identifier('e1',type(['Int'],1),pos(13,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field(identifier('e2',type(['Int'],1),pos(17,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let(identifier('a',type(['Int'],1),pos(37,109)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let(identifier('b',type(['Int'],1),pos(48,109)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field(identifier('e1',type(['Int'],1),pos(14,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field(identifier('e2',type(['Int'],1),pos(18,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let(identifier('a',type(['Int'],1),pos(38,112)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(14,112)),type(['Int'],1),pos(40,112)),type(['Int'],1),pos(40,112)),let(identifier('b',type(['Int'],1),pos(49,112)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(18,112)),type(['Int'],1),pos(51,112)),type(['Int'],1),pos(51,112)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(38,112)),type(['Int'],1),pos(62,112)),'cast2int'(identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(64,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false])]).
