alloy('unknown',[alloy_model('unknown',facts([fact(pred_call('Arithmetic',[],type(['PrimitiveBoolean'],0),pos(3,69)),(1,68))]),assertions([fact(pred_call('Arithmetic',[],type(['PrimitiveBoolean'],0),pos(3,73)),(1,72))]),commands([run('and'([pred_call('Arithmetic',[],type(['PrimitiveBoolean'],0),pos(3,69)), 'greater'(integer(3,pos(3,4)),integer(2,pos(7,4)),type(['PrimitiveBoolean'],0),pos(5,4)), 'greater_equal'(integer(3,pos(3,5)),integer(2,pos(8,5)),type(['PrimitiveBoolean'],0),pos(5,5)), 'less'(integer(2,pos(3,6)),integer(4,pos(7,6)),type(['PrimitiveBoolean'],0),pos(5,6)), 'less_equal'(integer(4,pos(3,7)),integer(5,pos(8,7)),type(['PrimitiveBoolean'],0),pos(5,7)), 'equal'(fun_call('integer''plus',[integer(1,pos(8,8)), integer(1,pos(10,8))],type(['Int'],1),pos(3,8)),integer(2,pos(15,8)),type(['PrimitiveBoolean'],0),pos(13,8)), 'equal'(fun_call('integer''mul',[integer(2,pos(7,9)), integer(3,pos(9,9))],type(['Int'],1),pos(3,9)),integer(6,pos(14,9)),type(['PrimitiveBoolean'],0),pos(12,9)), 'equal'(fun_call('integer''mul',[integer(3,pos(7,10)), integer(2,pos(9,10))],type(['Int'],1),pos(3,10)),integer(6,pos(14,10)),type(['PrimitiveBoolean'],0),pos(12,10)), 'equal'(fun_call('integer''minus',[integer(3,pos(9,11)), integer(2,pos(11,11))],type(['Int'],1),pos(3,11)),integer(1,pos(16,11)),type(['PrimitiveBoolean'],0),pos(14,11)), 'equal'(fun_call('integer''minus',[integer(10,pos(9,12)), integer(10,pos(12,12))],type(['Int'],1),pos(3,12)),integer(0,pos(18,12)),type(['PrimitiveBoolean'],0),pos(16,12)), 'equal'(fun_call('integer''div',[integer(10,pos(7,13)), integer(5,pos(10,13))],type(['Int'],1),pos(3,13)),integer(2,pos(15,13)),type(['PrimitiveBoolean'],0),pos(13,13)), 'equal'(fun_call('integer''div',[integer(11,pos(7,14)), integer(5,pos(10,14))],type(['Int'],1),pos(3,14)),integer(2,pos(15,14)),type(['PrimitiveBoolean'],0),pos(13,14)), 'equal'(fun_call('integer''rem',[integer(10,pos(7,15)), integer(5,pos(10,15))],type(['Int'],1),pos(3,15)),integer(0,pos(15,15)),type(['PrimitiveBoolean'],0),pos(13,15)), 'equal'(fun_call('integer''rem',[integer(11,pos(7,16)), integer(5,pos(10,16))],type(['Int'],1),pos(3,16)),integer(1,pos(15,16)),type(['PrimitiveBoolean'],0),pos(13,16)), 'equal'(integer(2,pos(7,17)),integer(2,pos(12,17)),type(['PrimitiveBoolean'],0),pos(10,17)), 'greater'(fun_call('integer''plus',[integer(1,pos(8,18)), integer(1,pos(10,18))],type(['Int'],1),pos(3,18)),integer(1,pos(15,18)),type(['PrimitiveBoolean'],0),pos(13,18)), 'equal'(fun_call('integer''mul',[fun_call('integer''plus',[integer(2,pos(12,19)), integer(2,pos(14,19))],type(['Int'],1),pos(7,19)), fun_call('integer''mul',[integer(2,pos(21,19)), integer(3,pos(23,19))],type(['Int'],1),pos(17,19))],type(['Int'],1),pos(3,19)),integer(24,pos(29,19)),type(['PrimitiveBoolean'],0),pos(27,19)), 'equal'('cartesian'(integer(2,pos(3,20)),integer(3,pos(6,20)),type(['Int', 'Int'],2),pos(4,20)),'cartesian'(fun_call('integer''plus',[integer(1,pos(15,20)), integer(1,pos(17,20))],type(['Int'],1),pos(10,20)),fun_call('integer''plus',[integer(1,pos(26,20)), integer(2,pos(28,20))],type(['Int'],1),pos(21,20)),type(['Int', 'Int'],2),pos(19,20)),type(['PrimitiveBoolean'],0),pos(8,20)), 'equal'('cartesian'(integer(2,pos(3,21)),'plus'(integer(3,pos(7,21)),integer(7,pos(9,21)),type(['Int'],1),pos(8,21)),type(['Int', 'Int'],2),pos(4,21)),'plus'('cartesian'(fun_call('integer''plus',[integer(1,pos(19,21)), integer(1,pos(21,21))],type(['Int'],1),pos(14,21)),fun_call('integer''plus',[integer(1,pos(30,21)), integer(2,pos(32,21))],type(['Int'],1),pos(25,21)),type(['Int', 'Int'],2),pos(23,21)),'cartesian'(integer(2,pos(37,21)),fun_call('integer''minus',[integer(8,pos(46,21)), integer(1,pos(48,21))],type(['Int'],1),pos(40,21)),type(['Int', 'Int'],2),pos(38,21)),type(['Int', 'Int'],2),pos(35,21)),type(['PrimitiveBoolean'],0),pos(12,21)), 'equal'('plus'(integer(1,pos(3,24)),integer(2,pos(7,24)),type(['Int'],1),pos(5,24)),'plus'(integer(1,pos(12,24)),integer(2,pos(18,24)),type(['Int'],1),pos(15,24)),type(['PrimitiveBoolean'],0),pos(9,24)), 'equal'(fun_call('integer''plus',['plus'(integer(1,pos(9,25)),fun_call('integer''plus',[integer(1,pos(16,25)), integer(0,pos(18,25))],type(['Int'],1),pos(11,25)),type(['Int'],1),pos(10,25)), integer(2,pos(22,25))],type(['Int'],1),pos(3,25)),integer(3,pos(27,25)),type(['PrimitiveBoolean'],0),pos(25,25)), 'equal'(fun_call('integer''minus',[integer(2,pos(9,26)), integer(3,pos(11,26))],type(['Int'],1),pos(3,26)),integer(-1,pos(16,26)),type(['PrimitiveBoolean'],0),pos(14,26)), 'equal'(fun_call('integer''add',['plus'(integer(1,pos(8,33)),fun_call('integer''sub',[integer(1,pos(14,33)), integer(0,pos(16,33))],type(['Int'],1),pos(10,33)),type(['Int'],1),pos(9,33)), integer(2,pos(20,33))],type(['Int'],1),pos(3,33)),integer(3,pos(25,33)),type(['PrimitiveBoolean'],0),pos(23,33)), pred_call('integer''gt',[integer(3,pos(6,35)), integer(2,pos(8,35))],type(['PrimitiveBoolean'],0),pos(3,35)), pred_call('integer''gt',[fun_call('integer''plus',[integer(1,pos(11,36)), integer(1,pos(13,36))],type(['Int'],1),pos(6,36)), integer(1,pos(16,36))],type(['PrimitiveBoolean'],0),pos(3,36)), pred_call('integer''gte',[fun_call('integer''plus',[integer(1,pos(12,37)), integer(1,pos(14,37))],type(['Int'],1),pos(7,37)), integer(1,pos(17,37))],type(['PrimitiveBoolean'],0),pos(3,37)), pred_call('integer''lt',[fun_call('integer''minus',[integer(1,pos(12,38)), integer(1,pos(14,38))],type(['Int'],1),pos(6,38)), integer(1,pos(17,38))],type(['PrimitiveBoolean'],0),pos(3,38)), pred_call('integer''lte',[fun_call('integer''minus',[integer(1,pos(13,39)), integer(1,pos(15,39))],type(['Int'],1),pos(7,39)), integer(1,pos(18,39))],type(['PrimitiveBoolean'],0),pos(3,39)), 'equal'(fun_call('integer''min',['plus'('plus'(integer(1,pos(7,40)),integer(2,pos(9,40)),type(['Int'],1),pos(8,40)),integer(3,pos(11,40)),type(['Int'],1),pos(10,40))],type(['Int'],1),pos(3,40)),integer(1,pos(16,40)),type(['PrimitiveBoolean'],0),pos(14,40)), 'equal'(fun_call('integer''max',['plus'('plus'(integer(1,pos(7,41)),integer(2,pos(9,41)),type(['Int'],1),pos(8,41)),integer(3,pos(11,41)),type(['Int'],1),pos(10,41))],type(['Int'],1),pos(3,41)),integer(3,pos(16,41)),type(['PrimitiveBoolean'],0),pos(14,41)), 'equal'(fun_call('integer''min',[],type(['Int'],1),pos(3,42)),integer(-32,pos(9,42)),type(['PrimitiveBoolean'],0),pos(7,42)), 'equal'(fun_call('integer''max',[],type(['Int'],1),pos(3,43)),integer(31,pos(9,43)),type(['PrimitiveBoolean'],0),pos(7,43)), 'equal'(fun_call('integer''div',[integer(3,pos(7,46)), integer(-2,pos(9,46))],type(['Int'],1),pos(3,46)),integer(-1,pos(15,46)),type(['PrimitiveBoolean'],0),pos(13,46)), 'equal'(fun_call('integer''div',[integer(-3,pos(7,47)), integer(-2,pos(10,47))],type(['Int'],1),pos(3,47)),integer(1,pos(16,47)),type(['PrimitiveBoolean'],0),pos(14,47)), 'equal'(fun_call('integer''div',[integer(-3,pos(7,48)), integer(2,pos(10,48))],type(['Int'],1),pos(3,48)),integer(-1,pos(15,48)),type(['PrimitiveBoolean'],0),pos(13,48)), 'equal'(fun_call('integer''negate',[integer(-2,pos(10,53))],type(['Int'],1),pos(3,53)),integer(2,pos(16,53)),type(['PrimitiveBoolean'],0),pos(14,53)), 'equal'('join'(integer(1,pos(8,54)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(3,54)),type(['Int'],1),pos(3,54)),integer(2,pos(13,54)),type(['PrimitiveBoolean'],0),pos(11,54)), 'equal'('join'(integer(1,pos(8,55)),fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(3,55)),type(['Int'],1),pos(3,55)),integer(0,pos(13,55)),type(['PrimitiveBoolean'],0),pos(11,55)), 'equal'(fun_call('integer''add',['join'('join'(integer(3,pos(17,56)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(12,56)),type(['Int'],1),pos(12,56)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(7,56)),type(['Int'],1),pos(7,56)), integer(3,pos(21,56))],type(['Int'],1),pos(3,56)),integer(8,pos(26,56)),type(['PrimitiveBoolean'],0),pos(24,56)), 'equal'(fun_call('integer''add',[integer(3,pos(3,58)), integer(2,pos(9,58))],type(['Int'],1),pos(5,58)),integer(5,pos(14,58)),type(['PrimitiveBoolean'],0),pos(12,58)), 'equal'(fun_call('integer''add',[integer(1,pos(3,59)), fun_call('integer''mul',[integer(4,pos(9,59)), integer(2,pos(15,59))],type(['Int'],1),pos(11,59))],type(['Int'],1),pos(5,59)),integer(9,pos(21,59)),type(['PrimitiveBoolean'],0),pos(19,59)), 'equal'(fun_call('integer''minus',[integer(1,pos(3,60)), fun_call('integer''mul',[integer(4,pos(11,60)), integer(2,pos(17,60))],type(['Int'],1),pos(13,60))],type(['Int'],1),pos(5,60)),integer(-7,pos(23,60)),type(['PrimitiveBoolean'],0),pos(21,60)), if_then_else('greater'(integer(3,pos(4,62)),integer(2,pos(6,62)),type(['PrimitiveBoolean'],0),pos(5,62)),'greater'(integer(7,pos(11,62)),integer(1,pos(13,62)),type(['PrimitiveBoolean'],0),pos(12,62)),'greater'(integer(8,pos(20,62)),integer(9,pos(22,62)),type(['PrimitiveBoolean'],0),pos(21,62)),type(['PrimitiveBoolean'],0),pos(8,62)), 'equal'(if_then_else('greater'(integer(3,pos(4,63)),integer(2,pos(6,63)),type(['PrimitiveBoolean'],0),pos(5,63)),integer(7,pos(11,63)),integer(8,pos(18,63)),type(['Int'],1),pos(8,63)),integer(7,pos(23,63)),type(['PrimitiveBoolean'],0),pos(21,63))],pos(17,3)),global_scope(-1),exact_scopes([]),upper_bound_scopes([]),bitwidth(6),maxseq(-1),index(0),pos(1,76))]),functions([predicate('Arithmetic',[],[],'and'(['greater'(integer(3,pos(3,4)),integer(2,pos(7,4)),type(['PrimitiveBoolean'],0),pos(5,4)), 'greater_equal'(integer(3,pos(3,5)),integer(2,pos(8,5)),type(['PrimitiveBoolean'],0),pos(5,5)), 'less'(integer(2,pos(3,6)),integer(4,pos(7,6)),type(['PrimitiveBoolean'],0),pos(5,6)), 'less_equal'(integer(4,pos(3,7)),integer(5,pos(8,7)),type(['PrimitiveBoolean'],0),pos(5,7)), 'equal'(fun_call('integer''plus',[integer(1,pos(8,8)), integer(1,pos(10,8))],type(['Int'],1),pos(3,8)),integer(2,pos(15,8)),type(['PrimitiveBoolean'],0),pos(13,8)), 'equal'(fun_call('integer''mul',[integer(2,pos(7,9)), integer(3,pos(9,9))],type(['Int'],1),pos(3,9)),integer(6,pos(14,9)),type(['PrimitiveBoolean'],0),pos(12,9)), 'equal'(fun_call('integer''mul',[integer(3,pos(7,10)), integer(2,pos(9,10))],type(['Int'],1),pos(3,10)),integer(6,pos(14,10)),type(['PrimitiveBoolean'],0),pos(12,10)), 'equal'(fun_call('integer''minus',[integer(3,pos(9,11)), integer(2,pos(11,11))],type(['Int'],1),pos(3,11)),integer(1,pos(16,11)),type(['PrimitiveBoolean'],0),pos(14,11)), 'equal'(fun_call('integer''minus',[integer(10,pos(9,12)), integer(10,pos(12,12))],type(['Int'],1),pos(3,12)),integer(0,pos(18,12)),type(['PrimitiveBoolean'],0),pos(16,12)), 'equal'(fun_call('integer''div',[integer(10,pos(7,13)), integer(5,pos(10,13))],type(['Int'],1),pos(3,13)),integer(2,pos(15,13)),type(['PrimitiveBoolean'],0),pos(13,13)), 'equal'(fun_call('integer''div',[integer(11,pos(7,14)), integer(5,pos(10,14))],type(['Int'],1),pos(3,14)),integer(2,pos(15,14)),type(['PrimitiveBoolean'],0),pos(13,14)), 'equal'(fun_call('integer''rem',[integer(10,pos(7,15)), integer(5,pos(10,15))],type(['Int'],1),pos(3,15)),integer(0,pos(15,15)),type(['PrimitiveBoolean'],0),pos(13,15)), 'equal'(fun_call('integer''rem',[integer(11,pos(7,16)), integer(5,pos(10,16))],type(['Int'],1),pos(3,16)),integer(1,pos(15,16)),type(['PrimitiveBoolean'],0),pos(13,16)), 'equal'(integer(2,pos(7,17)),integer(2,pos(12,17)),type(['PrimitiveBoolean'],0),pos(10,17)), 'greater'(fun_call('integer''plus',[integer(1,pos(8,18)), integer(1,pos(10,18))],type(['Int'],1),pos(3,18)),integer(1,pos(15,18)),type(['PrimitiveBoolean'],0),pos(13,18)), 'equal'(fun_call('integer''mul',[fun_call('integer''plus',[integer(2,pos(12,19)), integer(2,pos(14,19))],type(['Int'],1),pos(7,19)), fun_call('integer''mul',[integer(2,pos(21,19)), integer(3,pos(23,19))],type(['Int'],1),pos(17,19))],type(['Int'],1),pos(3,19)),integer(24,pos(29,19)),type(['PrimitiveBoolean'],0),pos(27,19)), 'equal'('cartesian'(integer(2,pos(3,20)),integer(3,pos(6,20)),type(['Int', 'Int'],2),pos(4,20)),'cartesian'(fun_call('integer''plus',[integer(1,pos(15,20)), integer(1,pos(17,20))],type(['Int'],1),pos(10,20)),fun_call('integer''plus',[integer(1,pos(26,20)), integer(2,pos(28,20))],type(['Int'],1),pos(21,20)),type(['Int', 'Int'],2),pos(19,20)),type(['PrimitiveBoolean'],0),pos(8,20)), 'equal'('cartesian'(integer(2,pos(3,21)),'plus'(integer(3,pos(7,21)),integer(7,pos(9,21)),type(['Int'],1),pos(8,21)),type(['Int', 'Int'],2),pos(4,21)),'plus'('cartesian'(fun_call('integer''plus',[integer(1,pos(19,21)), integer(1,pos(21,21))],type(['Int'],1),pos(14,21)),fun_call('integer''plus',[integer(1,pos(30,21)), integer(2,pos(32,21))],type(['Int'],1),pos(25,21)),type(['Int', 'Int'],2),pos(23,21)),'cartesian'(integer(2,pos(37,21)),fun_call('integer''minus',[integer(8,pos(46,21)), integer(1,pos(48,21))],type(['Int'],1),pos(40,21)),type(['Int', 'Int'],2),pos(38,21)),type(['Int', 'Int'],2),pos(35,21)),type(['PrimitiveBoolean'],0),pos(12,21)), 'equal'('plus'(integer(1,pos(3,24)),integer(2,pos(7,24)),type(['Int'],1),pos(5,24)),'plus'(integer(1,pos(12,24)),integer(2,pos(18,24)),type(['Int'],1),pos(15,24)),type(['PrimitiveBoolean'],0),pos(9,24)), 'equal'(fun_call('integer''plus',['plus'(integer(1,pos(9,25)),fun_call('integer''plus',[integer(1,pos(16,25)), integer(0,pos(18,25))],type(['Int'],1),pos(11,25)),type(['Int'],1),pos(10,25)), integer(2,pos(22,25))],type(['Int'],1),pos(3,25)),integer(3,pos(27,25)),type(['PrimitiveBoolean'],0),pos(25,25)), 'equal'(fun_call('integer''minus',[integer(2,pos(9,26)), integer(3,pos(11,26))],type(['Int'],1),pos(3,26)),integer(-1,pos(16,26)),type(['PrimitiveBoolean'],0),pos(14,26)), 'equal'(fun_call('integer''add',['plus'(integer(1,pos(8,33)),fun_call('integer''sub',[integer(1,pos(14,33)), integer(0,pos(16,33))],type(['Int'],1),pos(10,33)),type(['Int'],1),pos(9,33)), integer(2,pos(20,33))],type(['Int'],1),pos(3,33)),integer(3,pos(25,33)),type(['PrimitiveBoolean'],0),pos(23,33)), pred_call('integer''gt',[integer(3,pos(6,35)), integer(2,pos(8,35))],type(['PrimitiveBoolean'],0),pos(3,35)), pred_call('integer''gt',[fun_call('integer''plus',[integer(1,pos(11,36)), integer(1,pos(13,36))],type(['Int'],1),pos(6,36)), integer(1,pos(16,36))],type(['PrimitiveBoolean'],0),pos(3,36)), pred_call('integer''gte',[fun_call('integer''plus',[integer(1,pos(12,37)), integer(1,pos(14,37))],type(['Int'],1),pos(7,37)), integer(1,pos(17,37))],type(['PrimitiveBoolean'],0),pos(3,37)), pred_call('integer''lt',[fun_call('integer''minus',[integer(1,pos(12,38)), integer(1,pos(14,38))],type(['Int'],1),pos(6,38)), integer(1,pos(17,38))],type(['PrimitiveBoolean'],0),pos(3,38)), pred_call('integer''lte',[fun_call('integer''minus',[integer(1,pos(13,39)), integer(1,pos(15,39))],type(['Int'],1),pos(7,39)), integer(1,pos(18,39))],type(['PrimitiveBoolean'],0),pos(3,39)), 'equal'(fun_call('integer''min',['plus'('plus'(integer(1,pos(7,40)),integer(2,pos(9,40)),type(['Int'],1),pos(8,40)),integer(3,pos(11,40)),type(['Int'],1),pos(10,40))],type(['Int'],1),pos(3,40)),integer(1,pos(16,40)),type(['PrimitiveBoolean'],0),pos(14,40)), 'equal'(fun_call('integer''max',['plus'('plus'(integer(1,pos(7,41)),integer(2,pos(9,41)),type(['Int'],1),pos(8,41)),integer(3,pos(11,41)),type(['Int'],1),pos(10,41))],type(['Int'],1),pos(3,41)),integer(3,pos(16,41)),type(['PrimitiveBoolean'],0),pos(14,41)), 'equal'(fun_call('integer''min',[],type(['Int'],1),pos(3,42)),integer(-32,pos(9,42)),type(['PrimitiveBoolean'],0),pos(7,42)), 'equal'(fun_call('integer''max',[],type(['Int'],1),pos(3,43)),integer(31,pos(9,43)),type(['PrimitiveBoolean'],0),pos(7,43)), 'equal'(fun_call('integer''div',[integer(3,pos(7,46)), integer(-2,pos(9,46))],type(['Int'],1),pos(3,46)),integer(-1,pos(15,46)),type(['PrimitiveBoolean'],0),pos(13,46)), 'equal'(fun_call('integer''div',[integer(-3,pos(7,47)), integer(-2,pos(10,47))],type(['Int'],1),pos(3,47)),integer(1,pos(16,47)),type(['PrimitiveBoolean'],0),pos(14,47)), 'equal'(fun_call('integer''div',[integer(-3,pos(7,48)), integer(2,pos(10,48))],type(['Int'],1),pos(3,48)),integer(-1,pos(15,48)),type(['PrimitiveBoolean'],0),pos(13,48)), 'equal'(fun_call('integer''negate',[integer(-2,pos(10,53))],type(['Int'],1),pos(3,53)),integer(2,pos(16,53)),type(['PrimitiveBoolean'],0),pos(14,53)), 'equal'('join'(integer(1,pos(8,54)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(3,54)),type(['Int'],1),pos(3,54)),integer(2,pos(13,54)),type(['PrimitiveBoolean'],0),pos(11,54)), 'equal'('join'(integer(1,pos(8,55)),fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(3,55)),type(['Int'],1),pos(3,55)),integer(0,pos(13,55)),type(['PrimitiveBoolean'],0),pos(11,55)), 'equal'(fun_call('integer''add',['join'('join'(integer(3,pos(17,56)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(12,56)),type(['Int'],1),pos(12,56)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(7,56)),type(['Int'],1),pos(7,56)), integer(3,pos(21,56))],type(['Int'],1),pos(3,56)),integer(8,pos(26,56)),type(['PrimitiveBoolean'],0),pos(24,56)), 'equal'(fun_call('integer''add',[integer(3,pos(3,58)), integer(2,pos(9,58))],type(['Int'],1),pos(5,58)),integer(5,pos(14,58)),type(['PrimitiveBoolean'],0),pos(12,58)), 'equal'(fun_call('integer''add',[integer(1,pos(3,59)), fun_call('integer''mul',[integer(4,pos(9,59)), integer(2,pos(15,59))],type(['Int'],1),pos(11,59))],type(['Int'],1),pos(5,59)),integer(9,pos(21,59)),type(['PrimitiveBoolean'],0),pos(19,59)), 'equal'(fun_call('integer''minus',[integer(1,pos(3,60)), fun_call('integer''mul',[integer(4,pos(11,60)), integer(2,pos(17,60))],type(['Int'],1),pos(13,60))],type(['Int'],1),pos(5,60)),integer(-7,pos(23,60)),type(['PrimitiveBoolean'],0),pos(21,60)), if_then_else('greater'(integer(3,pos(4,62)),integer(2,pos(6,62)),type(['PrimitiveBoolean'],0),pos(5,62)),'greater'(integer(7,pos(11,62)),integer(1,pos(13,62)),type(['PrimitiveBoolean'],0),pos(12,62)),'greater'(integer(8,pos(20,62)),integer(9,pos(22,62)),type(['PrimitiveBoolean'],0),pos(21,62)),type(['PrimitiveBoolean'],0),pos(8,62)), 'equal'(if_then_else('greater'(integer(3,pos(4,63)),integer(2,pos(6,63)),type(['PrimitiveBoolean'],0),pos(5,63)),integer(7,pos(11,63)),integer(8,pos(18,63)),type(['Int'],1),pos(8,63)),integer(7,pos(23,63)),type(['PrimitiveBoolean'],0),pos(21,63))],pos(1,1)),pos(1,3))]),signatures([]),ordered_signatures([]),[sequences:false]),alloy_model('util''integer',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'@+'(identifier('n1',type(['Int'],1),pos(11,12)),identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(35,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'@-'(identifier('n1',type(['Int'],1),pos(12,15)),identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(36,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'closure'(identifier('n1',type(['Int'],1),pos(10,17)),identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(34,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'/'(identifier('n1',type(['Int'],1),pos(10,25)),identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(34,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'%'(identifier('n1',type(['Int'],1),pos(10,28)),identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(34,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'@-'(integer(0,pos(29,31)),identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(31,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'(identifier('n1',type(['Int'],1),pos(10,34)),identifier('n2',type(['Int'],1),pos(14,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'(identifier('n1',type(['Int'],1),pos(10,37)),identifier('n2',type(['Int'],1),pos(14,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'(identifier('n1',type(['Int'],1),pos(10,40)),identifier('n2',type(['Int'],1),pos(14,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'(identifier('n1',type(['Int'],1),pos(11,43)),identifier('n2',type(['Int'],1),pos(15,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'(identifier('n1',type(['Int'],1),pos(11,46)),identifier('n2',type(['Int'],1),pos(15,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'(identifier('n',type(['Int'],1),pos(12,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'(identifier('n',type(['Int'],1),pos(12,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'(identifier('n',type(['Int'],1),pos(14,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'(identifier('n',type(['Int'],1),pos(14,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'(identifier('n',type(['Int'],1),pos(13,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'@-'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'(identifier('n',type(['Int'],1),pos(13,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field('s','setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'(['e'],[field('e','oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),identifier('i',type(['Int'],1),pos(14,71)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),pos(1,80)),function('integer''max',[],[],integer(max,pos(19,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],integer(min,pos(19,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let('a',identifier('e1',type(['Int'],1),pos(13,109)),let('b',identifier('e2',type(['Int'],1),pos(17,109)),if_then_else('less'(identifier('a',type(['Int'],1),pos(37,109)),identifier('b',type(['Int'],1),pos(48,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let('a',identifier('e1',type(['Int'],1),pos(14,112)),let('b',identifier('e2',type(['Int'],1),pos(18,112)),if_then_else('less'(identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false])]).