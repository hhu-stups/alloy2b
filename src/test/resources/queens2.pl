alloy('queens2',[alloy_model('queens2',facts([fact('all'(['q', 'q_'],[field('q','oneof'(identifier('queen',type(['queen'],1),pos(5,7)),type(['queen'],1),pos(14,17)),type(['queen'],1),[],pos(12,17)), field('q_','oneof'('minus'(identifier('queen',type(['queen'],1),pos(5,7)),identifier('q',type(['queen'],1),pos(12,17)),type(['queen'],1),pos(30,17)),type(['queen'],1),pos(25,17)),type(['queen'],1),[],pos(21,17))],'and'(['not'('equal'('join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(8,18)),'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(15,18)),type(['PrimitiveBoolean'],0),pos(11,18)),type(['PrimitiveBoolean'],0),pos(5,18)), 'not'('equal'('join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(8,19)),'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(15,19)),type(['PrimitiveBoolean'],0),pos(11,19)),type(['PrimitiveBoolean'],0),pos(5,19)), 'not'('equal'(fun_call('integer''plus',['join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(13,20)), 'join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(17,20))],type(['Int'],1),pos(7,20)),fun_call('integer''plus',['join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(30,20)), 'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(35,20))],type(['Int'],1),pos(23,20)),type(['PrimitiveBoolean'],0),pos(21,20)),type(['PrimitiveBoolean'],0),pos(5,20)), 'not'('equal'(fun_call('integer''plus',['join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(13,21)), 'join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(18,21))],type(['Int'],1),pos(7,21)),fun_call('integer''plus',['join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(31,21)), 'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(37,21))],type(['Int'],1),pos(24,21)),type(['PrimitiveBoolean'],0),pos(22,21)),type(['PrimitiveBoolean'],0),pos(5,21))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(8,17)),(1,17))]),assertions([]),commands([run('and'(['all'(['q', 'q_'],[field('q','oneof'(identifier('queen',type(['queen'],1),pos(5,7)),type(['queen'],1),pos(14,17)),type(['queen'],1),[],pos(12,17)), field('q_','oneof'('minus'(identifier('queen',type(['queen'],1),pos(5,7)),identifier('q',type(['queen'],1),pos(12,17)),type(['queen'],1),pos(30,17)),type(['queen'],1),pos(25,17)),type(['queen'],1),[],pos(21,17))],'and'(['not'('equal'('join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(8,18)),'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(15,18)),type(['PrimitiveBoolean'],0),pos(11,18)),type(['PrimitiveBoolean'],0),pos(5,18)), 'not'('equal'('join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(8,19)),'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(15,19)),type(['PrimitiveBoolean'],0),pos(11,19)),type(['PrimitiveBoolean'],0),pos(5,19)), 'not'('equal'(fun_call('integer''plus',['join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(13,20)), 'join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(17,20))],type(['Int'],1),pos(7,20)),fun_call('integer''plus',['join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(30,20)), 'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(35,20))],type(['Int'],1),pos(23,20)),type(['PrimitiveBoolean'],0),pos(21,20)),type(['PrimitiveBoolean'],0),pos(5,20)), 'not'('equal'(fun_call('integer''plus',['join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(13,21)), 'join'(identifier('q',type(['queen'],1),pos(12,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(18,21))],type(['Int'],1),pos(7,21)),fun_call('integer''plus',['join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(31,21)), 'join'(identifier('q_',type(['queen'],1),pos(21,17)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(37,21))],type(['Int'],1),pos(24,21)),type(['PrimitiveBoolean'],0),pos(22,21)),type(['PrimitiveBoolean'],0),pos(5,21))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(8,17))],pos(1,17)),global_scope(-1),exact_scopes([('queen',4)]),upper_bound_scopes([]),bitwidth(5),maxseq(-1),index(0),pos(1,26))]),functions([predicate('show',[],[],boolean(true,pos(11,24)),pos(1,24))]),signatures([signature('queen',[field('x','oneof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(15,7)),type(['Int'],1),[],pos(13,7)),field('x_','oneof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(23,7)),type(['Int'],1),[],pos(20,7)),field('y','oneof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(30,7)),type(['Int'],1),[],pos(28,7))],['and'(['greater_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(5,8)),integer(1,pos(10,8)),type(['PrimitiveBoolean'],0),pos(7,8)), 'greater_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(5,9)),integer(1,pos(10,9)),type(['PrimitiveBoolean'],0),pos(7,9)), 'less_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(5,10)),'card'(identifier('queen',type(['queen'],1),pos(5,7)),type(['Int'],1),pos(10,10)),type(['PrimitiveBoolean'],0),pos(7,10)), 'less_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('y',type(['queen', 'Int'],2),pos(28,7)),type(['Int'],1),pos(5,11)),'card'(identifier('queen',type(['queen'],1),pos(5,7)),type(['Int'],1),pos(10,11)),type(['PrimitiveBoolean'],0),pos(7,11)), 'greater_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(5,12)),integer(1,pos(10,12)),type(['PrimitiveBoolean'],0),pos(8,12)), 'less_equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(5,13)),'card'(identifier('queen',type(['queen'],1),pos(5,7)),type(['Int'],1),pos(11,13)),type(['PrimitiveBoolean'],0),pos(8,13)), 'equal'('join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x_',type(['queen', 'Int'],2),pos(20,7)),type(['Int'],1),pos(5,14)),fun_call('integer''minus',[fun_call('integer''plus',['card'(identifier('queen',type(['queen'],1),pos(5,7)),type(['Int'],1),pos(21,14)), integer(1,pos(28,14))],type(['Int'],1),pos(16,14)), 'join'(identifier(this,type(['queen'],1),pos(1,1)),identifier('x',type(['queen', 'Int'],2),pos(13,7)),type(['Int'],1),pos(31,14))],type(['Int'],1),pos(10,14)),type(['PrimitiveBoolean'],0),pos(8,14))],pos(1,1))],[],pos(5,7))]),ordered_signatures([]),[sequences:false]),alloy_model('util''integer',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'@+'(identifier('n1',type(['Int'],1),pos(11,12)),identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(35,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'@-'(identifier('n1',type(['Int'],1),pos(12,15)),identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(36,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'closure'(identifier('n1',type(['Int'],1),pos(10,17)),identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(34,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'/'(identifier('n1',type(['Int'],1),pos(10,25)),identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(34,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'%'(identifier('n1',type(['Int'],1),pos(10,28)),identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(34,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'@-'(integer(0,pos(29,31)),identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(31,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'(identifier('n1',type(['Int'],1),pos(10,34)),identifier('n2',type(['Int'],1),pos(14,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'(identifier('n1',type(['Int'],1),pos(10,37)),identifier('n2',type(['Int'],1),pos(14,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'(identifier('n1',type(['Int'],1),pos(10,40)),identifier('n2',type(['Int'],1),pos(14,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'(identifier('n1',type(['Int'],1),pos(11,43)),identifier('n2',type(['Int'],1),pos(15,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'(identifier('n1',type(['Int'],1),pos(11,46)),identifier('n2',type(['Int'],1),pos(15,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'(identifier('n',type(['Int'],1),pos(12,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'(identifier('n',type(['Int'],1),pos(12,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'(identifier('n',type(['Int'],1),pos(14,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'(identifier('n',type(['Int'],1),pos(14,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'(identifier('n',type(['Int'],1),pos(13,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'@-'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'(identifier('n',type(['Int'],1),pos(13,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field('s','setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'(['e'],[field('e','oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),identifier('i',type(['Int'],1),pos(14,71)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),pos(1,80)),function('integer''max',[],[],integer(max,pos(19,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],integer(min,pos(19,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let('a',identifier('e1',type(['Int'],1),pos(13,109)),let('b',identifier('e2',type(['Int'],1),pos(17,109)),if_then_else('less'(identifier('a',type(['Int'],1),pos(37,109)),identifier('b',type(['Int'],1),pos(48,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let('a',identifier('e1',type(['Int'],1),pos(14,112)),let('b',identifier('e2',type(['Int'],1),pos(18,112)),if_then_else('less'(identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false])]).
