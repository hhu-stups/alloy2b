alloy('unknown',[alloy_model('(unknown,alloy2b_none)',facts([]),assertions([fact('and'(['equal'('card'('join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(4,26)),type(['Int'],1),pos(2,26)),integer(7,pos(15,26)),type(['PrimitiveBoolean'],0),pos(13,26)), 'equal'(fun_call('seq''first',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,27))],type(['univ'],1),pos(12,27)),integer(12,pos(20,27)),type(['PrimitiveBoolean'],0),pos(18,27)), 'equal'(fun_call('seq''last',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,28))],type(['univ'],1),pos(12,28)),integer(12,pos(19,28)),type(['PrimitiveBoolean'],0),pos(17,28)), 'equal'(fun_call('seq''lastIdx',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,29))],type(['Int'],1),pos(12,29)),integer(6,pos(22,29)),type(['PrimitiveBoolean'],0),pos(20,29)), 'equal'(fun_call('seq''idxOf',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,31)), 'cast2sigint'(integer(12,pos(18,31)),type(['Int'],1),pos(18,31))],type(['Int'],1),pos(12,31)),integer(0,pos(24,31)),type(['PrimitiveBoolean'],0),pos(22,31)), 'equal'(fun_call('seq''lastIdxOf',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,32)), 'cast2sigint'(integer(12,pos(23,32)),type(['Int'],1),pos(23,32))],type(['Int'],1),pos(12,32)),integer(6,pos(29,32)),type(['PrimitiveBoolean'],0),pos(27,32))],pos(1,1)),(1,25))]),commands([check('and'(['not'('and'(['equal'('card'('join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(4,26)),type(['Int'],1),pos(2,26)),integer(7,pos(15,26)),type(['PrimitiveBoolean'],0),pos(13,26)), 'equal'(fun_call('seq''first',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,27))],type(['univ'],1),pos(12,27)),integer(12,pos(20,27)),type(['PrimitiveBoolean'],0),pos(18,27)), 'equal'(fun_call('seq''last',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,28))],type(['univ'],1),pos(12,28)),integer(12,pos(19,28)),type(['PrimitiveBoolean'],0),pos(17,28)), 'equal'(fun_call('seq''lastIdx',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,29))],type(['Int'],1),pos(12,29)),integer(6,pos(22,29)),type(['PrimitiveBoolean'],0),pos(20,29)), 'equal'(fun_call('seq''idxOf',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,31)), 'cast2sigint'(integer(12,pos(18,31)),type(['Int'],1),pos(18,31))],type(['Int'],1),pos(12,31)),integer(0,pos(24,31)),type(['PrimitiveBoolean'],0),pos(22,31)), 'equal'(fun_call('seq''lastIdxOf',['join'(identifier('C',type(['C'],1),pos(14,8)),identifier('numbers',type(['C', 'seq''Int', 'Int'],3),pos(2,11)),type(['seq''Int', 'Int'],2),pos(3,32)), 'cast2sigint'(integer(12,pos(23,32)),type(['Int'],1),pos(23,32))],type(['Int'],1),pos(12,32)),integer(6,pos(29,32)),type(['PrimitiveBoolean'],0),pos(27,32))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(1,25))],pos(1,25)),global_scope(-1),exact_scopes([]),upper_bound_scopes([]),bitwidth(6),maxseq(10),index(0),pos(1,35))]),functions([]),signatures([signature('Parent',[],[],[abstract],pos(14,2)),signature('A',[],[],[subsig('Parent')],pos(5,4)),signature('B',[],[],[subsig('Parent')],pos(5,6)),signature('C',[field('vertices','is_seq'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),identifier('A',type(['Parent'],1),pos(5,4)),type(['seq''Int', 'Parent'],2),pos(12,9)),type(['seq''Int', 'Parent'],2),[],pos(2,9)),field('transitions','is_seq'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),identifier('A',type(['Parent'],1),pos(5,4)),type(['seq''Int', 'Parent'],2),pos(15,10)),type(['seq''Int', 'Parent'],2),[],pos(2,10)),field('numbers','is_seq'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),identifier('Int',type(['Int'],1),pos(1,1)),type(['seq''Int', 'Int'],2),pos(12,11)),type(['seq''Int', 'Int'],2),[],pos(2,11)),field('setA','setof'(identifier('A',type(['Parent'],1),pos(5,4)),type(['Parent'],1),pos(9,12)),type(['Parent'],1),[],pos(2,12)),field('setB','setof'(identifier('B',type(['Parent'],1),pos(5,6)),type(['Parent'],1),pos(9,13)),type(['Parent'],1),[],pos(2,13)),field('anything','setof'('cartesian'(identifier('B',type(['Parent'],1),pos(5,6)),'cartesian'(identifier('A',type(['Parent'],1),pos(5,4)),identifier('B',type(['Parent'],1),pos(5,6)),type(['Parent', 'Parent'],2),pos(25,14)),type(['Parent', 'Parent', 'Parent'],3),pos(19,14)),type(['Parent', 'Parent', 'Parent'],3),pos(12,14)),type(['Parent', 'Parent', 'Parent'],3),[],pos(2,14))],['and'(['equal'(fun_call('seq''first',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('vertices',type(['C', 'seq''Int', 'Parent'],3),pos(2,9)),type(['seq''Int', 'Parent'],2),pos(2,16))],type(['Parent'],1),pos(11,16)),fun_call('seq''last',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('transitions',type(['C', 'seq''Int', 'Parent'],3),pos(2,10)),type(['seq''Int', 'Parent'],2),pos(19,16))],type(['Parent'],1),pos(31,16)),type(['PrimitiveBoolean'],0),pos(17,16)), 'not_equal'(fun_call('seq''butlast',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('vertices',type(['C', 'seq''Int', 'Parent'],3),pos(2,9)),type(['seq''Int', 'Parent'],2),pos(2,17))],type(['seq''Int', 'Parent'],2),pos(11,17)),fun_call('seq''butlast',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('transitions',type(['C', 'seq''Int', 'Parent'],3),pos(2,10)),type(['seq''Int', 'Parent'],2),pos(22,17))],type(['seq''Int', 'Parent'],2),pos(34,17)),type(['PrimitiveBoolean'],0),pos(19,17)), 'not'(pred_call('seq''isEmpty',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('vertices',type(['C', 'seq''Int', 'Parent'],3),pos(2,9)),type(['seq''Int', 'Parent'],2),pos(3,18))],type(['PrimitiveBoolean'],0),pos(12,18)),type(['PrimitiveBoolean'],0),pos(2,18)), 'not'(pred_call('seq''hasDups',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('transitions',type(['C', 'seq''Int', 'Parent'],3),pos(2,10)),type(['seq''Int', 'Parent'],2),pos(3,19))],type(['PrimitiveBoolean'],0),pos(15,19)),type(['PrimitiveBoolean'],0),pos(2,19)), 'not_equal'(fun_call('seq''inds',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('transitions',type(['C', 'seq''Int', 'Parent'],3),pos(2,10)),type(['seq''Int', 'Parent'],2),pos(2,20))],type(['Int'],1),pos(14,20)),fun_call('seq''inds',['join'(identifier(this,type(['C'],1),pos(1,1)),identifier('vertices',type(['C', 'seq''Int', 'Parent'],3),pos(2,9)),type(['seq''Int', 'Parent'],2),pos(22,20))],type(['Int'],1),pos(31,20)),type(['PrimitiveBoolean'],0),pos(19,20)), 'in'('join'('join'('join'(identifier(this,type(['C'],1),pos(1,1)),identifier('anything',type(['C', 'Parent', 'Parent', 'Parent'],4),pos(2,14)),type(['Parent', 'Parent', 'Parent'],3),pos(3,21)),'join'(identifier(this,type(['C'],1),pos(1,1)),identifier('setA',type(['C', 'Parent'],2),pos(2,12)),type(['Parent'],1),pos(12,21)),type(['Parent', 'Parent'],2),pos(11,21)),'join'(identifier(this,type(['C'],1),pos(1,1)),identifier('setB',type(['C', 'Parent'],2),pos(2,13)),type(['Parent'],1),pos(18,21)),type(['Parent'],1),pos(17,21)),'join'(identifier(this,type(['C'],1),pos(1,1)),identifier('setB',type(['C', 'Parent'],2),pos(2,13)),type(['Parent'],1),pos(26,21)),type(['PrimitiveBoolean'],0),pos(23,21))],pos(1,1))],[abstract],pos(14,8))]),ordered_signatures([]),[sequences:true]),alloy_model('(util''integer,integer)',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'cast2sigint'('integer''plus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,12)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,12)),type(['Int'],1),pos(30,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'cast2sigint'('integer''minus'('cast2int'(identifier('n1',type(['Int'],1),pos(12,15)),type(['Int'],1),pos(33,15)),'cast2int'(identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(44,15)),type(['Int'],1),pos(36,15)),type(['Int'],1),pos(31,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'cast2sigint'('closure'('cast2int'(identifier('n1',type(['Int'],1),pos(10,17)),type(['Int'],1),pos(31,17)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(42,17)),type(['Int'],1),pos(34,17)),type(['Int'],1),pos(29,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'cast2sigint'('integer''div'('cast2int'(identifier('n1',type(['Int'],1),pos(10,25)),type(['Int'],1),pos(31,25)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(42,25)),type(['Int'],1),pos(34,25)),type(['Int'],1),pos(29,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'cast2sigint'('integer''rem'('cast2int'(identifier('n1',type(['Int'],1),pos(10,28)),type(['Int'],1),pos(31,28)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(42,28)),type(['Int'],1),pos(34,28)),type(['Int'],1),pos(29,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'cast2sigint'('integer''minus'(integer(0,pos(29,31)),'cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),type(['Int'],1),pos(31,31)),type(['Int'],1),pos(27,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'('cast2sigint'('cast2int'(identifier('n1',type(['Int'],1),pos(10,34)),type(['Int'],1),pos(25,34)),type(['Int'],1),pos(25,34)),'cast2sigint'('cast2int'(identifier('n2',type(['Int'],1),pos(14,34)),type(['Int'],1),pos(35,34)),type(['Int'],1),pos(35,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'('cast2int'(identifier('n1',type(['Int'],1),pos(10,37)),type(['Int'],1),pos(25,37)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,37)),type(['Int'],1),pos(30,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'('cast2int'(identifier('n1',type(['Int'],1),pos(10,40)),type(['Int'],1),pos(25,40)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,40)),type(['Int'],1),pos(30,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,43)),type(['Int'],1),pos(26,43)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,43)),type(['Int'],1),pos(32,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,46)),type(['Int'],1),pos(26,46)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,46)),type(['Int'],1),pos(32,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'('cast2int'(identifier('n',type(['Int'],1),pos(12,52)),type(['Int'],1),pos(22,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'('cast2int'(identifier('n',type(['Int'],1),pos(12,55)),type(['Int'],1),pos(22,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,58)),type(['Int'],1),pos(24,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,61)),type(['Int'],1),pos(24,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(29,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'integer''minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(56,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field('s','setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'(['e'],[field('e','oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),'cast2sigint'('cast2int'(identifier('i',type(['Int'],1),pos(14,71)),type(['Int'],1),pos(22,72)),type(['Int'],1),pos(22,72)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'cast2sigint'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),type(['Int'],1),pos(52,80)),pos(1,80)),function('integer''max',[],[],'cast2sigint'(integer(max,pos(19,85)),type(['Int'],1),pos(17,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],'cast2sigint'(integer(min,pos(19,88)),type(['Int'],1),pos(17,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let('a','cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let('b','cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let('a','cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(14,112)),type(['Int'],1),pos(40,112)),type(['Int'],1),pos(40,112)),let('b','cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(18,112)),type(['Int'],1),pos(51,112)),type(['Int'],1),pos(51,112)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(38,112)),type(['Int'],1),pos(62,112)),'cast2int'(identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(64,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:true]),alloy_model('(util''sequniv,seq)',facts([]),assertions([]),commands([]),functions([predicate('seq''isSeq',[identifier('s',type(['Int', 'univ'],2),pos(12,26))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,26)),type(['Int', 'univ'],2),[],pos(12,26))],'and'(['in'(identifier('s',type(['Int', 'univ'],2),pos(12,26)),'partial_function'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['seq''Int', 'univ'],2),pos(16,27)),type(['PrimitiveBoolean'],0),pos(5,27)), 'in'('minus'(fun_call('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(12,26))],type(['Int'],1),pos(5,28)),'join'(fun_call('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(12,26))],type(['Int'],1),pos(22,28)),fun_call('integer''next',[],type(['Int', 'Int'],2),pos(12,28)),type(['Int'],1),pos(12,28)),type(['Int'],1),pos(10,28)),'cast2sigint'(integer(0,pos(31,28)),type(['Int'],1),pos(31,28)),type(['PrimitiveBoolean'],0),pos(28,28))],pos(1,1)),pos(1,26)),function('seq''elems',[identifier('s',type(['Int', 'univ'],2),pos(12,32))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,32)),type(['Int', 'univ'],2),[],pos(12,32))],'join'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),identifier('s',type(['Int', 'univ'],2),pos(12,32)),type(['univ'],1),pos(51,32)),pos(1,32)),function('seq''first',[identifier('s',type(['Int', 'univ'],2),pos(12,38))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,38)),type(['Int', 'univ'],2),[],pos(12,38))],'join'('cast2sigint'(integer(0,pos(46,38)),type(['Int'],1),pos(46,38)),identifier('s',type(['Int', 'univ'],2),pos(12,38)),type(['univ'],1),pos(44,38)),pos(1,38)),function('seq''last',[identifier('s',type(['Int', 'univ'],2),pos(11,44))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(18,44)),type(['Int', 'univ'],2),[],pos(11,44))],'join'(fun_call('seq''lastIdx',[identifier('s',type(['Int', 'univ'],2),pos(11,44))],type(['Int'],1),pos(45,44)),identifier('s',type(['Int', 'univ'],2),pos(11,44)),type(['univ'],1),pos(43,44)),pos(1,44)),function('seq''rest',[identifier('s',type(['Int', 'univ'],2),pos(11,50))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(18,50)),type(['Int', 'univ'],2),[],pos(11,50))],'dom_restr'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),'join'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(46,50)),identifier('s',type(['Int', 'univ'],2),pos(11,50)),type(['Int', 'univ'],2),pos(54,50)),type(['seq''Int', 'univ'],2),pos(41,50)),pos(1,50)),function('seq''butlast',[identifier('s',type(['Int', 'univ'],2),pos(14,53))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(21,53)),type(['Int', 'univ'],2),[],pos(14,53))],'dom_restr'('minus'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),fun_call('seq''lastIdx',[identifier('s',type(['Int', 'univ'],2),pos(14,53))],type(['Int'],1),pos(14,54)),type(['seq''Int'],1),pos(12,54)),identifier('s',type(['Int', 'univ'],2),pos(14,53)),type(['seq''Int', 'univ'],2),pos(26,54)),pos(1,53)),predicate('seq''isEmpty',[identifier('s',type(['Int', 'univ'],2),pos(15,58))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(22,58)),type(['Int', 'univ'],2),[],pos(15,58))],'no'(identifier('s',type(['Int', 'univ'],2),pos(15,58)),type(['PrimitiveBoolean'],0),pos(33,58)),pos(1,58)),predicate('seq''hasDups',[identifier('s',type(['Int', 'univ'],2),pos(15,61))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(22,61)),type(['Int', 'univ'],2),[],pos(15,61))],'less'('card'(fun_call('seq''elems',[identifier('s',type(['Int', 'univ'],2),pos(15,61))],type(['univ'],1),pos(35,61)),type(['Int'],1),pos(33,61)),'card'(fun_call('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(15,61))],type(['Int'],1),pos(48,61)),type(['Int'],1),pos(46,61)),type(['PrimitiveBoolean'],0),pos(44,61)),pos(1,61)),function('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(11,64))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(18,64)),type(['Int', 'univ'],2),[],pos(11,64))],'join'(identifier('s',type(['Int', 'univ'],2),pos(11,64)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int'],1),pos(39,64)),pos(1,64)),function('seq''lastIdx',[identifier('s',type(['Int', 'univ'],2),pos(14,70))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(21,70)),type(['Int', 'univ'],2),[],pos(14,70))],fun_call('integer''max',[fun_call('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(14,70))],type(['Int'],1),pos(49,70))],type(['Int'],1),pos(42,70)),pos(1,70)),function('seq''afterLastIdx',[identifier('s',type(['Int', 'univ'],2),pos(19,77))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(26,77)),type(['Int', 'univ'],2),[],pos(19,77))],fun_call('integer''min',['minus'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),fun_call('seq''inds',[identifier('s',type(['Int', 'univ'],2),pos(19,77))],type(['Int'],1),pos(65,77)),type(['seq''Int'],1),pos(63,77))],type(['Int'],1),pos(48,77)),pos(1,77)),function('seq''idxOf',[identifier('s',type(['Int', 'univ'],2),pos(12,80)), identifier('e',type(['univ'],1),pos(28,80))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,80)),type(['Int', 'univ'],2),[],pos(12,80)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(28,80))],fun_call('integer''min',[fun_call('seq''indsOf',[identifier('s',type(['Int', 'univ'],2),pos(12,80)), identifier('e',type(['univ'],1),pos(28,80))],type(['Int'],1),pos(57,80))],type(['Int'],1),pos(50,80)),pos(1,80)),function('seq''lastIdxOf',[identifier('s',type(['Int', 'univ'],2),pos(16,83)), identifier('e',type(['univ'],1),pos(32,83))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(23,83)),type(['Int', 'univ'],2),[],pos(16,83)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(32,83))],fun_call('integer''max',[fun_call('seq''indsOf',[identifier('s',type(['Int', 'univ'],2),pos(16,83)), identifier('e',type(['univ'],1),pos(32,83))],type(['Int'],1),pos(61,83))],type(['Int'],1),pos(54,83)),pos(1,83)),function('seq''indsOf',[identifier('s',type(['Int', 'univ'],2),pos(13,86)), identifier('e',type(['univ'],1),pos(29,86))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(20,86)),type(['Int', 'univ'],2),[],pos(13,86)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(29,86))],'join'(identifier('s',type(['Int', 'univ'],2),pos(13,86)),identifier('e',type(['univ'],1),pos(29,86)),type(['Int'],1),pos(51,86)),pos(1,86)),function('seq''add',[identifier('s',type(['Int', 'univ'],2),pos(10,92)), identifier('e',type(['univ'],1),pos(26,92))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(17,92)),type(['Int', 'univ'],2),[],pos(10,92)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(26,92))],fun_call('seq''setAt',[identifier('s',type(['Int', 'univ'],2),pos(10,92)), fun_call('seq''afterLastIdx',[identifier('s',type(['Int', 'univ'],2),pos(10,92))],type(['Int'],1),pos(12,93)), identifier('e',type(['univ'],1),pos(26,92))],type(['Int', 'univ'],2),pos(3,93)),pos(1,92)),function('seq''setAt',[identifier('s',type(['Int', 'univ'],2),pos(12,100)), identifier('i',type(['Int'],1),pos(28,100)), identifier('e',type(['univ'],1),pos(36,100))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,100)),type(['Int', 'univ'],2),[],pos(12,100)), field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(28,100)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(36,100))],'override'(identifier('s',type(['Int', 'univ'],2),pos(12,100)),'cartesian'(identifier('i',type(['Int'],1),pos(28,100)),identifier('e',type(['univ'],1),pos(36,100)),type(['Int', 'univ'],2),pos(10,101)),type(['Int', 'univ'],2),pos(5,101)),pos(1,100)),function('seq''insert',[identifier('s',type(['Int', 'univ'],2),pos(13,109)), identifier('i',type(['Int'],1),pos(29,109)), identifier('e',type(['univ'],1),pos(37,109))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(20,109)),type(['Int', 'univ'],2),[],pos(13,109)), field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(29,109)), field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(37,109))],'dom_restr'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),'plus'('plus'('dom_restr'(fun_call('integer''prevs',[identifier('i',type(['Int'],1),pos(29,109))],type(['Int'],1),pos(16,110)),identifier('s',type(['Int', 'univ'],2),pos(13,109)),type(['Int', 'univ'],2),pos(28,110)),'cartesian'(identifier('i',type(['Int'],1),pos(29,109)),identifier('e',type(['univ'],1),pos(37,109)),type(['Int', 'univ'],2),pos(38,110)),type(['Int', 'univ'],2),pos(34,110)),'join'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(45,110)),'dom_restr'('plus'(fun_call('integer''nexts',[identifier('i',type(['Int'],1),pos(29,109))],type(['Int'],1),pos(55,110)),identifier('i',type(['Int'],1),pos(29,109)),type(['Int'],1),pos(67,110)),identifier('s',type(['Int', 'univ'],2),pos(13,109)),type(['Int', 'univ'],2),pos(72,110)),type(['Int', 'univ'],2),pos(52,110)),type(['Int', 'univ'],2),pos(43,110)),type(['seq''Int', 'univ'],2),pos(11,110)),pos(1,109)),function('seq''delete',[identifier('s',type(['Int', 'univ'],2),pos(12,117)), identifier('i',type(['Int'],1),pos(28,117))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(19,117)),type(['Int', 'univ'],2),[],pos(12,117)), field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(28,117))],'plus'('dom_restr'(fun_call('integer''prevs',[identifier('i',type(['Int'],1),pos(28,117))],type(['Int'],1),pos(4,118)),identifier('s',type(['Int', 'univ'],2),pos(12,117)),type(['Int', 'univ'],2),pos(16,118)),'join'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(25,118)),'dom_restr'(fun_call('integer''nexts',[identifier('i',type(['Int'],1),pos(28,117))],type(['Int'],1),pos(35,118)),identifier('s',type(['Int', 'univ'],2),pos(12,117)),type(['Int', 'univ'],2),pos(47,118)),type(['Int', 'univ'],2),pos(33,118)),type(['Int', 'univ'],2),pos(22,118)),pos(1,117)),function('seq''append',[identifier('s1',type(['Int', 'univ'],2),pos(13,125)), identifier('s2',type(['Int', 'univ'],2),pos(17,125))],[field('s1','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(25,125)),type(['Int', 'univ'],2),[],pos(13,125)),field('s2','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(25,125)),type(['Int', 'univ'],2),[],pos(13,125))],let('shift','comprehension'(['i_','i'],[field('i_','oneof'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),type(['seq''Int'],1),pos(23,126)),type(['seq''Int'],1),[],pos(16,126)),field('i','oneof'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),type(['seq''Int'],1),pos(23,126)),type(['seq''Int'],1),[],pos(16,126))],'equal'('cast2sigint'('cast2int'(identifier('i_',type(['seq''Int'],1),pos(16,126)),type(['Int'],1),pos(33,126)),type(['Int'],1),pos(33,126)),fun_call('integer''add',['cast2sigint'('cast2int'(identifier('i',type(['seq''Int'],1),pos(20,126)),type(['Int'],1),pos(50,126)),type(['Int'],1),pos(50,126)), fun_call('integer''add',['cast2sigint'('cast2int'(fun_call('seq''lastIdx',[identifier('s1',type(['Int', 'univ'],2),pos(13,125))],type(['Int'],1),pos(69,126)),type(['Int'],1),pos(65,126)),type(['Int'],1),pos(65,126)), 'cast2sigint'(integer(1,pos(83,126)),type(['Int'],1),pos(83,126))],type(['Int'],1),pos(58,126))],type(['Int'],1),pos(43,126)),type(['PrimitiveBoolean'],0),pos(41,126)),type(['seq''Int', 'seq''Int'],2),pos(15,126)),if_then_else('no'(identifier('s1',type(['Int', 'univ'],2),pos(13,125)),type(['PrimitiveBoolean'],0),pos(5,127)),identifier('s2',type(['Int', 'univ'],2),pos(17,125)),'plus'(identifier('s1',type(['Int', 'univ'],2),pos(13,125)),'join'(identifier('shift',type(['seq''Int', 'seq''Int'],2),pos(7,126)),identifier('s2',type(['Int', 'univ'],2),pos(17,125)),type(['seq''Int', 'univ'],2),pos(33,127)),type(['Int', 'univ'],2),pos(26,127)),type(['Int', 'univ'],2),pos(11,127)),type(['Int', 'univ'],2),pos(13,126)),pos(1,125)),function('seq''subseq',[identifier('s',type(['Int', 'univ'],2),pos(13,134)), identifier('from',type(['Int'],1),pos(29,134)), identifier('to',type(['Int'],1),pos(35,134))],[field('s','cartesian'(identifier('Int',type(['Int'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['Int', 'univ'],2),pos(20,134)),type(['Int', 'univ'],2),[],pos(13,134)), field('from',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(29,134)),field('to',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(29,134))],let('shift','comprehension'(['i_','i'],[field('i_','oneof'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),type(['seq''Int'],1),pos(23,135)),type(['seq''Int'],1),[],pos(16,135)),field('i','oneof'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),type(['seq''Int'],1),pos(23,135)),type(['seq''Int'],1),[],pos(16,135))],'equal'('cast2sigint'('cast2int'(identifier('i_',type(['seq''Int'],1),pos(16,135)),type(['Int'],1),pos(33,135)),type(['Int'],1),pos(33,135)),fun_call('integer''sub',['cast2sigint'('cast2int'(identifier('i',type(['seq''Int'],1),pos(20,135)),type(['Int'],1),pos(50,135)),type(['Int'],1),pos(50,135)), 'cast2sigint'('cast2int'(identifier('from',type(['Int'],1),pos(29,134)),type(['Int'],1),pos(58,135)),type(['Int'],1),pos(58,135))],type(['Int'],1),pos(43,135)),type(['PrimitiveBoolean'],0),pos(41,135)),type(['seq''Int', 'seq''Int'],2),pos(15,135)),'join'(identifier('shift',type(['seq''Int', 'seq''Int'],2),pos(7,135)),'dom_restr'('minus'(identifier('seq''Int',type(['seq''Int'],1),pos(1,1)),fun_call('integer''nexts',[identifier('to',type(['Int'],1),pos(35,134))],type(['Int'],1),pos(23,136)),type(['seq''Int'],1),pos(21,136)),identifier('s',type(['Int', 'univ'],2),pos(13,134)),type(['seq''Int', 'univ'],2),pos(37,136)),type(['seq''Int', 'univ'],2),pos(10,136)),type(['seq''Int', 'univ'],2),pos(13,135)),pos(1,134))]),signatures([]),ordered_signatures([]),[sequences:true])]).
