alloy('unknown',[alloy_model(('unknown',alloy2b_none),facts([]),assertions([]),commands([run('and'([],pos(14,3)),global_scope(-1),exact_scopes([]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(0),pos(1,5))]),functions([predicate('nothing',[],[],boolean(true,pos(14,3)),pos(1,3))]),signatures([signature('E',[],[],[enum, abstract],pos(6,1)),signature('E0',[],[],[one,subsig('E')],pos(9,1)),signature('E1',[],[],[one,subsig('E')],pos(12,1)),signature('E2',[],[],[one,subsig('E')],pos(15,1))]),ordered_signatures(['ordering']),[sequences:false,parent_types:[],ordering_successors_only:true]),alloy_model(('util''integer','integer'),facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field(identifier('n1',type(['Int'],1),pos(11,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field(identifier('n2',type(['Int'],1),pos(15,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field(identifier('n1',type(['Int'],1),pos(11,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field(identifier('n2',type(['Int'],1),pos(15,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'cast2sigint'('integer''plus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,12)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,12)),type(['Int'],1),pos(30,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field(identifier('n1',type(['Int'],1),pos(12,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field(identifier('n2',type(['Int'],1),pos(16,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field(identifier('n1',type(['Int'],1),pos(12,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field(identifier('n2',type(['Int'],1),pos(16,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'cast2sigint'('integer''minus'('cast2int'(identifier('n1',type(['Int'],1),pos(12,15)),type(['Int'],1),pos(33,15)),'cast2int'(identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(44,15)),type(['Int'],1),pos(36,15)),type(['Int'],1),pos(31,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field(identifier('n1',type(['Int'],1),pos(10,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field(identifier('n2',type(['Int'],1),pos(14,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'cast2sigint'('closure'('cast2int'(identifier('n1',type(['Int'],1),pos(10,17)),type(['Int'],1),pos(31,17)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(42,17)),type(['Int'],1),pos(34,17)),type(['Int'],1),pos(29,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field(identifier('n1',type(['Int'],1),pos(10,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field(identifier('n2',type(['Int'],1),pos(14,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'cast2sigint'('integer''div'('cast2int'(identifier('n1',type(['Int'],1),pos(10,25)),type(['Int'],1),pos(31,25)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(42,25)),type(['Int'],1),pos(34,25)),type(['Int'],1),pos(29,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field(identifier('n1',type(['Int'],1),pos(10,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field(identifier('n2',type(['Int'],1),pos(14,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'cast2sigint'('integer''rem'('cast2int'(identifier('n1',type(['Int'],1),pos(10,28)),type(['Int'],1),pos(31,28)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(42,28)),type(['Int'],1),pos(34,28)),type(['Int'],1),pos(29,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field(identifier('n',type(['Int'],1),pos(13,31)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'cast2sigint'('integer''minus'(integer(0,pos(29,31)),'cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),type(['Int'],1),pos(31,31)),type(['Int'],1),pos(27,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field(identifier('n1',type(['Int'],1),pos(10,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field(identifier('n2',type(['Int'],1),pos(14,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'('cast2sigint'('cast2int'(identifier('n1',type(['Int'],1),pos(10,34)),type(['Int'],1),pos(25,34)),type(['Int'],1),pos(25,34)),'cast2sigint'('cast2int'(identifier('n2',type(['Int'],1),pos(14,34)),type(['Int'],1),pos(35,34)),type(['Int'],1),pos(35,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field(identifier('n1',type(['Int'],1),pos(10,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field(identifier('n2',type(['Int'],1),pos(14,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'('cast2int'(identifier('n1',type(['Int'],1),pos(10,37)),type(['Int'],1),pos(25,37)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,37)),type(['Int'],1),pos(30,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field(identifier('n1',type(['Int'],1),pos(10,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field(identifier('n2',type(['Int'],1),pos(14,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'('cast2int'(identifier('n1',type(['Int'],1),pos(10,40)),type(['Int'],1),pos(25,40)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,40)),type(['Int'],1),pos(30,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field(identifier('n1',type(['Int'],1),pos(11,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field(identifier('n2',type(['Int'],1),pos(15,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,43)),type(['Int'],1),pos(26,43)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,43)),type(['Int'],1),pos(32,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field(identifier('n1',type(['Int'],1),pos(11,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field(identifier('n2',type(['Int'],1),pos(15,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,46)),type(['Int'],1),pos(26,46)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,46)),type(['Int'],1),pos(32,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field(identifier('n',type(['Int'],1),pos(12,49)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field(identifier('n',type(['Int'],1),pos(12,52)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'('cast2int'(identifier('n',type(['Int'],1),pos(12,52)),type(['Int'],1),pos(22,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field(identifier('n',type(['Int'],1),pos(12,55)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'('cast2int'(identifier('n',type(['Int'],1),pos(12,55)),type(['Int'],1),pos(22,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field(identifier('n',type(['Int'],1),pos(14,58)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,58)),type(['Int'],1),pos(24,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field(identifier('n',type(['Int'],1),pos(14,61)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,61)),type(['Int'],1),pos(24,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field(identifier('n',type(['Int'],1),pos(13,64)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(29,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'integer''minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(56,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field(identifier('i',type(['Int'],1),pos(14,71)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field(identifier('next',type(['univ', 'univ'],2),pos(22,71)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field(identifier('s',type(['univ'],1),pos(40,71)),'setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'([identifier('e',type(['univ'],1),pos(3,72))],[field(identifier('e',type(['univ'],1),pos(4,72)),'oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),'cast2sigint'('cast2int'(identifier('i',type(['Int'],1),pos(14,71)),type(['Int'],1),pos(22,72)),type(['Int'],1),pos(22,72)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field(identifier('e',type(['univ'],1),pos(14,80)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field(identifier('next',type(['univ', 'univ'],2),pos(23,80)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'cast2sigint'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),type(['Int'],1),pos(52,80)),pos(1,80)),function('integer''max',[],[],'cast2sigint'(integer(max,pos(19,85)),type(['Int'],1),pos(17,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field(identifier('es',type(['Int'],1),pos(10,97)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],'cast2sigint'(integer(min,pos(19,88)),type(['Int'],1),pos(17,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field(identifier('es',type(['Int'],1),pos(10,100)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field(identifier('e',type(['Int'],1),pos(12,103)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field(identifier('e',type(['Int'],1),pos(12,106)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field(identifier('e1',type(['Int'],1),pos(13,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field(identifier('e2',type(['Int'],1),pos(17,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let(identifier('a',type(['Int'],1),pos(37,109)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let(identifier('b',type(['Int'],1),pos(48,109)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field(identifier('e1',type(['Int'],1),pos(14,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field(identifier('e2',type(['Int'],1),pos(18,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let(identifier('a',type(['Int'],1),pos(38,112)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(14,112)),type(['Int'],1),pos(40,112)),type(['Int'],1),pos(40,112)),let(identifier('b',type(['Int'],1),pos(49,112)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(18,112)),type(['Int'],1),pos(51,112)),type(['Int'],1),pos(51,112)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(38,112)),type(['Int'],1),pos(62,112)),'cast2int'(identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(64,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures(['ordering']),[sequences:false,parent_types:[],ordering_successors_only:true]),alloy_model(('util''ordering','ordering'),facts([]),assertions([fact(let(identifier('mynext',type(['E', 'E'],2),pos(7,81)),'join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(19,81)),let(identifier('myprev',type(['E', 'E'],2),pos(7,82)),'inverse'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(16,82)),'and'(['all'([identifier('b',type(['E'],1),pos(8,83))],[field(identifier('b',type(['E'],1),pos(12,83)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(14,83)),type(['E'],1),[],pos(12,83))],'and'(['lone'('join'(identifier('b',type(['E'],1),pos(12,83)),fun_call('ordering''next',[],type(['E', 'E'],2),pos(29,83)),type(['E'],1),pos(28,83)),type(['PrimitiveBoolean'],0),pos(22,83)), 'lone'('join'(identifier('b',type(['E'],1),pos(12,83)),fun_call('ordering''prev',[],type(['E', 'E'],2),pos(46,83)),type(['E'],1),pos(45,83)),type(['PrimitiveBoolean'],0),pos(39,83)), 'not_in'(identifier('b',type(['E'],1),pos(12,83)),'join'(identifier('b',type(['E'],1),pos(12,83)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(64,83)),type(['E'],1),pos(63,83)),type(['PrimitiveBoolean'],0),pos(58,83))],pos(52,83)),type(['PrimitiveBoolean'],0),pos(8,83)), 'no'('join'(fun_call('ordering''first',[],type(['E'],1),pos(12,84)),fun_call('ordering''prev',[],type(['E', 'E'],2),pos(18,84)),type(['E'],1),pos(17,84)),type(['PrimitiveBoolean'],0),pos(9,84)), 'no'('join'(fun_call('ordering''last',[],type(['E'],1),pos(31,84)),fun_call('ordering''next',[],type(['E', 'E'],2),pos(36,84)),type(['E'],1),pos(35,84)),type(['PrimitiveBoolean'],0),pos(28,84)), 'all'([identifier('b',type(['E'],1),pos(8,85))],[field(identifier('b',type(['E'],1),pos(12,85)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(14,85)),type(['E'],1),[],pos(12,85))],'implication'('and'(['not_equal'(identifier('b',type(['E'],1),pos(12,85)),fun_call('ordering''first',[],type(['E'],1),pos(25,85)),type(['PrimitiveBoolean'],0),pos(23,85)), 'not_equal'(identifier('b',type(['E'],1),pos(12,85)),fun_call('ordering''last',[],type(['E'],1),pos(37,85)),type(['PrimitiveBoolean'],0),pos(35,85))],pos(31,85)),'and'(['one'('join'(identifier('b',type(['E'],1),pos(12,85)),fun_call('ordering''prev',[],type(['E', 'E'],2),pos(53,85)),type(['E'],1),pos(52,85)),type(['PrimitiveBoolean'],0),pos(47,85)), 'one'('join'(identifier('b',type(['E'],1),pos(12,85)),fun_call('ordering''next',[],type(['E', 'E'],2),pos(67,85)),type(['E'],1),pos(66,85)),type(['PrimitiveBoolean'],0),pos(61,85))],pos(58,85)),type(['PrimitiveBoolean'],0),pos(43,85)),type(['PrimitiveBoolean'],0),pos(8,85)), 'implication'('not'('one'(identifier('E',type(['E'],1),pos(6,1)),type(['PrimitiveBoolean'],0),pos(9,86)),type(['PrimitiveBoolean'],0),pos(8,86)),'and'(['one'(fun_call('ordering''first',[],type(['E'],1),pos(26,86)),type(['PrimitiveBoolean'],0),pos(22,86)), 'one'(fun_call('ordering''last',[],type(['E'],1),pos(39,86)),type(['PrimitiveBoolean'],0),pos(35,86)), 'not_equal'(fun_call('ordering''first',[],type(['E'],1),pos(47,86)),fun_call('ordering''last',[],type(['E'],1),pos(54,86)),type(['PrimitiveBoolean'],0),pos(52,86)), 'one'('join'(fun_call('ordering''first',[],type(['E'],1),pos(66,86)),fun_call('ordering''next',[],type(['E', 'E'],2),pos(72,86)),type(['E'],1),pos(71,86)),type(['PrimitiveBoolean'],0),pos(62,86)), 'one'('join'(fun_call('ordering''last',[],type(['E'],1),pos(84,86)),fun_call('ordering''prev',[],type(['E', 'E'],2),pos(89,86)),type(['E'],1),pos(88,86)),type(['PrimitiveBoolean'],0),pos(80,86))],pos(77,86)),type(['PrimitiveBoolean'],0),pos(18,86)), 'implication'('one'(identifier('E',type(['E'],1),pos(6,1)),type(['PrimitiveBoolean'],0),pos(8,87)),'and'(['equal'(fun_call('ordering''first',[],type(['E'],1),pos(21,87)),identifier('E',type(['E'],1),pos(6,1)),type(['PrimitiveBoolean'],0),pos(26,87)), 'equal'(fun_call('ordering''last',[],type(['E'],1),pos(35,87)),identifier('E',type(['E'],1),pos(6,1)),type(['PrimitiveBoolean'],0),pos(39,87)), 'no'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['PrimitiveBoolean'],0),pos(48,87)), 'no'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['PrimitiveBoolean'],0),pos(61,87))],pos(58,87)),type(['PrimitiveBoolean'],0),pos(17,87)), 'equal'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),'inverse'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(15,88)),type(['PrimitiveBoolean'],0),pos(14,88)), 'equal'(identifier('E',type(['E'],1),pos(6,1)),'join'(fun_call('ordering''first',[],type(['E'],1),pos(15,89)),'closure'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['univ', 'univ'],2),pos(21,89)),type(['univ'],1),pos(20,89)),type(['PrimitiveBoolean'],0),pos(13,89)), 'all'([identifier('a',type(['E'],1),pos(7,90)), identifier('b',type(['E'],1),pos(7,90))],[field(identifier('a',type(['E'],1),pos(16,90)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(20,90)),type(['E'],1),[disj],pos(16,90)),field(identifier('b',type(['E'],1),pos(18,90)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(20,90)),type(['E'],1),[disj],pos(16,90))],'or'(['in'(identifier('a',type(['E'],1),pos(16,90)),'join'(identifier('b',type(['E'],1),pos(18,90)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(34,90)),type(['E'],1),pos(33,90)),type(['PrimitiveBoolean'],0),pos(29,90)), 'in'(identifier('a',type(['E'],1),pos(16,90)),'join'(identifier('b',type(['E'],1),pos(18,90)),'closure1'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['E', 'E'],2),pos(52,90)),type(['E'],1),pos(51,90)),type(['PrimitiveBoolean'],0),pos(47,90))],pos(42,90)),type(['PrimitiveBoolean'],0),pos(7,90)), 'no'([identifier('a',type(['E'],1),pos(7,91)), identifier('b',type(['E'],1),pos(7,91))],[field(identifier('a',type(['E'],1),pos(15,91)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(19,91)),type(['E'],1),[disj],pos(15,91)),field(identifier('b',type(['E'],1),pos(17,91)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(19,91)),type(['E'],1),[disj],pos(15,91))],'and'(['in'(identifier('a',type(['E'],1),pos(15,91)),'join'(identifier('b',type(['E'],1),pos(17,91)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(33,91)),type(['E'],1),pos(32,91)),type(['PrimitiveBoolean'],0),pos(28,91)), 'in'(identifier('a',type(['E'],1),pos(15,91)),'join'(identifier('b',type(['E'],1),pos(17,91)),'closure1'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['E', 'E'],2),pos(52,91)),type(['E'],1),pos(51,91)),type(['PrimitiveBoolean'],0),pos(47,91))],pos(41,91)),type(['PrimitiveBoolean'],0),pos(7,91)), 'all'([identifier('a',type(['E'],1),pos(7,92)), identifier('b',type(['E'],1),pos(7,92)), identifier('c',type(['E'],1),pos(7,92))],[field(identifier('a',type(['E'],1),pos(16,92)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,92)),type(['E'],1),[disj],pos(16,92)),field(identifier('b',type(['E'],1),pos(18,92)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,92)),type(['E'],1),[disj],pos(16,92)),field(identifier('c',type(['E'],1),pos(20,92)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,92)),type(['E'],1),[disj],pos(16,92))],'implication'('and'(['in'(identifier('b',type(['E'],1),pos(18,92)),'join'(identifier('a',type(['E'],1),pos(16,92)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(37,92)),type(['E'],1),pos(36,92)),type(['PrimitiveBoolean'],0),pos(32,92)), 'in'(identifier('c',type(['E'],1),pos(20,92)),'join'(identifier('b',type(['E'],1),pos(18,92)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(56,92)),type(['E'],1),pos(55,92)),type(['PrimitiveBoolean'],0),pos(51,92))],pos(45,92)),'in'(identifier('c',type(['E'],1),pos(20,92)),'join'(identifier('a',type(['E'],1),pos(16,92)),'closure1'(identifier('mynext',type(['E', 'E'],2),pos(7,81)),type(['E', 'E'],2),pos(75,92)),type(['E'],1),pos(74,92)),type(['PrimitiveBoolean'],0),pos(70,92)),type(['PrimitiveBoolean'],0),pos(65,92)),type(['PrimitiveBoolean'],0),pos(7,92)), 'all'([identifier('a',type(['E'],1),pos(7,93)), identifier('b',type(['E'],1),pos(7,93)), identifier('c',type(['E'],1),pos(7,93))],[field(identifier('a',type(['E'],1),pos(16,93)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,93)),type(['E'],1),[disj],pos(16,93)),field(identifier('b',type(['E'],1),pos(18,93)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,93)),type(['E'],1),[disj],pos(16,93)),field(identifier('c',type(['E'],1),pos(20,93)),'oneof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(22,93)),type(['E'],1),[disj],pos(16,93))],'implication'('and'(['in'(identifier('b',type(['E'],1),pos(18,93)),'join'(identifier('a',type(['E'],1),pos(16,93)),'closure1'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['E', 'E'],2),pos(37,93)),type(['E'],1),pos(36,93)),type(['PrimitiveBoolean'],0),pos(32,93)), 'in'(identifier('c',type(['E'],1),pos(20,93)),'join'(identifier('b',type(['E'],1),pos(18,93)),'closure1'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['E', 'E'],2),pos(56,93)),type(['E'],1),pos(55,93)),type(['PrimitiveBoolean'],0),pos(51,93))],pos(45,93)),'in'(identifier('c',type(['E'],1),pos(20,93)),'join'(identifier('a',type(['E'],1),pos(16,93)),'closure1'(identifier('myprev',type(['E', 'E'],2),pos(7,82)),type(['E', 'E'],2),pos(75,93)),type(['E'],1),pos(74,93)),type(['PrimitiveBoolean'],0),pos(70,93)),type(['PrimitiveBoolean'],0),pos(65,93)),type(['PrimitiveBoolean'],0),pos(7,93))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(14,82)),type(['PrimitiveBoolean'],0),pos(14,81)),(1,80))]),commands([run(identifier('run$1',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',0)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(0),pos(1,96)),run(identifier('run$2',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',1)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(1),pos(1,97)),run(identifier('run$3',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',2)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(2),pos(1,98)),run(identifier('run$4',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',3)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(3),pos(1,99)),run(identifier('run$5',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',4)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(4),pos(1,100)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',0)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(5),pos(1,101)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',1)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(6),pos(1,102)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',2)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(7),pos(1,103)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',3)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(8),pos(1,104)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',4)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(9),pos(1,105)),check(identifier('correct',type([''],0),pos(1,1)),global_scope(-1),exact_scopes([('elem',5)]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(10),pos(1,106))]),functions([function('ordering''first',[],[],'join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('First',type(['Ordering', 'E'],2),pos(4,26)),type(['E'],1),pos(26,33)),pos(1,33)),function('ordering''last',[],[],'minus'(identifier('E',type(['E'],1),pos(6,1)),'join'(fun_call('ordering''next',[],type(['E', 'E'],2),pos(30,36)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(34,36)),type(['E'],1),pos(27,36)),pos(1,36)),function('ordering''prev',[],[],'inverse'('join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(30,39)),type(['E', 'E'],2),pos(25,39)),pos(1,39)),function('ordering''next',[],[],'join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(28,42)),pos(1,42)),function('ordering''prevs',[identifier('e',type(['E'],1),pos(12,45))],[field(identifier('e',type(['E'],1),pos(12,45)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(12,45))],'join'(identifier('e',type(['E'],1),pos(12,45)),'closure1'('inverse'('join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(42,45)),type(['E', 'E'],2),pos(37,45)),type(['E', 'E'],2),pos(35,45)),type(['E'],1),pos(34,45)),pos(1,45)),function('ordering''nexts',[identifier('e',type(['E'],1),pos(12,48))],[field(identifier('e',type(['E'],1),pos(12,48)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(12,48))],'join'(identifier('e',type(['E'],1),pos(12,48)),'closure1'('join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(40,48)),type(['E', 'E'],2),pos(35,48)),type(['E'],1),pos(34,48)),pos(1,48)),predicate('ordering''lt',[identifier('e1',type(['E'],1),pos(10,51)), identifier('e2',type(['E'],1),pos(14,51))],[field(identifier('e1',type(['E'],1),pos(10,51)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(10,51)),field(identifier('e2',type(['E'],1),pos(14,51)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(10,51))],'in'(identifier('e1',type(['E'],1),pos(10,51)),fun_call('ordering''prevs',[identifier('e2',type(['E'],1),pos(14,51))],type(['E'],1),pos(32,51)),type(['PrimitiveBoolean'],0),pos(29,51)),pos(1,51)),predicate('ordering''gt',[identifier('e1',type(['E'],1),pos(10,54)), identifier('e2',type(['E'],1),pos(14,54))],[field(identifier('e1',type(['E'],1),pos(10,54)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(10,54)),field(identifier('e2',type(['E'],1),pos(14,54)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(10,54))],'in'(identifier('e1',type(['E'],1),pos(10,54)),fun_call('ordering''nexts',[identifier('e2',type(['E'],1),pos(14,54))],type(['E'],1),pos(32,54)),type(['PrimitiveBoolean'],0),pos(29,54)),pos(1,54)),predicate('ordering''lte',[identifier('e1',type(['E'],1),pos(11,57)), identifier('e2',type(['E'],1),pos(15,57))],[field(identifier('e1',type(['E'],1),pos(11,57)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(11,57)),field(identifier('e2',type(['E'],1),pos(15,57)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(11,57))],'or'(['equal'(identifier('e1',type(['E'],1),pos(11,57)),identifier('e2',type(['E'],1),pos(15,57)),type(['PrimitiveBoolean'],0),pos(29,57)), pred_call('ordering''lt',[identifier('e1',type(['E'],1),pos(11,57)), identifier('e2',type(['E'],1),pos(15,57))],type(['PrimitiveBoolean'],0),pos(36,57))],pos(33,57)),pos(1,57)),predicate('ordering''gte',[identifier('e1',type(['E'],1),pos(11,60)), identifier('e2',type(['E'],1),pos(15,60))],[field(identifier('e1',type(['E'],1),pos(11,60)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(11,60)),field(identifier('e2',type(['E'],1),pos(15,60)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(11,60))],'or'(['equal'(identifier('e1',type(['E'],1),pos(11,60)),identifier('e2',type(['E'],1),pos(15,60)),type(['PrimitiveBoolean'],0),pos(29,60)), pred_call('ordering''gt',[identifier('e1',type(['E'],1),pos(11,60)), identifier('e2',type(['E'],1),pos(15,60))],type(['PrimitiveBoolean'],0),pos(36,60))],pos(33,60)),pos(1,60)),function('ordering''larger',[identifier('e1',type(['E'],1),pos(13,63)), identifier('e2',type(['E'],1),pos(17,63))],[field(identifier('e1',type(['E'],1),pos(13,63)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(13,63)),field(identifier('e2',type(['E'],1),pos(17,63)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(13,63))],if_then_else(pred_call('ordering''lt',[identifier('e1',type(['E'],1),pos(13,63)), identifier('e2',type(['E'],1),pos(17,63))],type(['PrimitiveBoolean'],0),pos(35,63)),identifier('e2',type(['E'],1),pos(17,63)),identifier('e1',type(['E'],1),pos(13,63)),type(['E'],1),pos(45,63)),pos(1,63)),function('ordering''smaller',[identifier('e1',type(['E'],1),pos(14,66)), identifier('e2',type(['E'],1),pos(18,66))],[field(identifier('e1',type(['E'],1),pos(14,66)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(14,66)),field(identifier('e2',type(['E'],1),pos(18,66)),identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),[],pos(14,66))],if_then_else(pred_call('ordering''lt',[identifier('e1',type(['E'],1),pos(14,66)), identifier('e2',type(['E'],1),pos(18,66))],type(['PrimitiveBoolean'],0),pos(36,66)),identifier('e1',type(['E'],1),pos(14,66)),identifier('e2',type(['E'],1),pos(18,66)),type(['E'],1),pos(46,66)),pos(1,66)),function('ordering''max',[identifier('es',type(['E'],1),pos(10,72))],[field(identifier('es',type(['E'],1),pos(10,72)),'setof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(14,72)),type(['E'],1),[],pos(10,72))],'minus'(identifier('es',type(['E'],1),pos(10,72)),'join'(identifier('es',type(['E'],1),pos(10,72)),'closure1'('inverse'('join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(52,72)),type(['E', 'E'],2),pos(47,72)),type(['E', 'E'],2),pos(45,72)),type(['E'],1),pos(44,72)),type(['E'],1),pos(40,72)),pos(1,72)),function('ordering''min',[identifier('es',type(['E'],1),pos(10,78))],[field(identifier('es',type(['E'],1),pos(10,78)),'setof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(14,78)),type(['E'],1),[],pos(10,78))],'minus'(identifier('es',type(['E'],1),pos(10,78)),'join'(identifier('es',type(['E'],1),pos(10,78)),'closure1'('join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(50,78)),type(['E', 'E'],2),pos(45,78)),type(['E'],1),pos(44,78)),type(['E'],1),pos(40,78)),pos(1,78)),predicate('ordering''run$1',[],[],boolean(true,pos(5,96)),pos(1,96)),predicate('ordering''run$2',[],[],boolean(true,pos(5,97)),pos(1,97)),predicate('ordering''run$3',[],[],boolean(true,pos(5,98)),pos(1,98)),predicate('ordering''run$4',[],[],boolean(true,pos(5,99)),pos(1,99)),predicate('ordering''run$5',[],[],boolean(true,pos(5,100)),pos(1,100))]),signatures([signature('ordering''Ord',[field(identifier('First',type(['Ordering', 'E'],2),pos(4,26)),'setof'(identifier('E',type(['E'],1),pos(6,1)),type(['E'],1),pos(11,26)),type(['E'],1),[],pos(4,26)),field(identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),'cartesian'(identifier('E',type(['E'],1),pos(6,1)),identifier('E',type(['E'],1),pos(6,1)),type(['E', 'E'],2),pos(15,27)),type(['E', 'E'],2),[],pos(4,27))],['totalorder'([identifier('E',type(['E'],1),pos(6,1)), 'join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('First',type(['Ordering', 'E'],2),pos(4,26)),type(['E'],1),pos(25,29)), 'join'(identifier('ordering''Ord',type(['Ordering'],1),pos(17,25)),identifier('Next',type(['Ordering', 'E', 'E'],3),pos(4,27)),type(['E', 'E'],2),pos(31,29))],pos(4,29))],[one, private],pos(17,25))]),ordered_signatures(['ordering']),[sequences:false,parent_types:[],ordering_successors_only:true])]).
