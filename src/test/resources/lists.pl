alloy('list',[alloy_model(('list',alloy2b_none),facts([fact('in'(identifier('Thing',type(['Thing'],1),pos(5,11)),'join'(identifier('List',type(['List'],1),pos(14,14)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(34,12)),type(['PrimitiveBoolean'],0),pos(27,12)),(1,12)),fact('all'([identifier('L',type(['List'],1),pos(14,25))],[field(identifier('L',type(['List'],1),pos(18,25)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(21,25)),type(['List'],1),[],pos(18,25))],pred_call('isFinite',[identifier('L',type(['List'],1),pos(18,25))],type(['PrimitiveBoolean'],0),pos(28,25)),type(['PrimitiveBoolean'],0),pos(14,25)),(1,25)),fact('all'([identifier('a',type(['List'],1),pos(5,28)), identifier('b',type(['List'],1),pos(5,28))],[field(identifier('a',type(['List'],1),pos(9,28)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(14,28)),type(['List'],1),[],pos(9,28)),field(identifier('b',type(['List'],1),pos(11,28)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(14,28)),type(['List'],1),[],pos(9,28))],'iff'('in'(identifier('a',type(['List'],1),pos(9,28)),'join'(identifier('b',type(['List'],1),pos(11,28)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(28,28)),type(['PrimitiveBoolean'],0),pos(24,28)),'and'(['equal'('join'(identifier('a',type(['List'],1),pos(9,28)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(45,28)),'join'(identifier('b',type(['List'],1),pos(11,28)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(53,28)),type(['PrimitiveBoolean'],0),pos(50,28)), 'in'('join'(identifier('b',type(['List'],1),pos(11,28)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(63,28)),'join'('join'(identifier('a',type(['List'],1),pos(9,28)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(72,28)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(76,28)),type(['PrimitiveBoolean'],0),pos(68,28)), 'equal'('card'('join'(identifier('a',type(['List'],1),pos(9,28)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(94,28)),type(['univ'],1),pos(93,28)),type(['Int'],1),pos(91,28)),'card'('join'(identifier('b',type(['List'],1),pos(11,28)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(104,28)),type(['univ'],1),pos(103,28)),type(['Int'],1),pos(101,28)),type(['PrimitiveBoolean'],0),pos(99,28))],pos(86,28)),type(['PrimitiveBoolean'],0),pos(38,28)),type(['PrimitiveBoolean'],0),pos(5,28)),(1,27)),fact('and'(['all'([identifier('e',type(['List'],1),pos(5,38)), identifier('L',type(['List'],1),pos(5,38))],[field(identifier('e',type(['List'],1),pos(9,38)),'oneof'(identifier('EmptyList',type(['List'],1),pos(5,22)),type(['List'],1),pos(12,38)),type(['List'],1),[],pos(9,38)), field(identifier('L',type(['List'],1),pos(23,38)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(25,38)),type(['List'],1),[],pos(23,38))],'in'(identifier('e',type(['List'],1),pos(9,38)),'join'(identifier('L',type(['List'],1),pos(23,38)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(38,38)),type(['PrimitiveBoolean'],0),pos(34,38)),type(['PrimitiveBoolean'],0),pos(5,38)), 'all'([identifier('a',type(['List'],1),pos(5,39)), identifier('b',type(['List'],1),pos(5,39))],[field(identifier('a',type(['List'],1),pos(9,39)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(14,39)),type(['List'],1),[],pos(9,39)),field(identifier('b',type(['List'],1),pos(11,39)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(14,39)),type(['List'],1),[],pos(9,39))],'iff'('in'(identifier('a',type(['List'],1),pos(9,39)),'join'(identifier('b',type(['List'],1),pos(11,39)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(36,39)),type(['PrimitiveBoolean'],0),pos(32,39)),'and'(['equal'('join'(identifier('a',type(['List'],1),pos(9,39)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(53,39)),'join'(identifier('b',type(['List'],1),pos(11,39)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(61,39)),type(['PrimitiveBoolean'],0),pos(58,39)), 'in'('join'(identifier('a',type(['List'],1),pos(9,39)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(54,40)),'join'('join'(identifier('b',type(['List'],1),pos(11,39)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(63,40)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(67,40)),type(['PrimitiveBoolean'],0),pos(59,40)), 'less'('card'('join'(identifier('a',type(['List'],1),pos(9,39)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(56,41)),type(['univ'],1),pos(55,41)),type(['Int'],1),pos(53,41)),'card'('join'(identifier('b',type(['List'],1),pos(11,39)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(66,41)),type(['univ'],1),pos(65,41)),type(['Int'],1),pos(63,41)),type(['PrimitiveBoolean'],0),pos(61,41))],pos(49,41)),type(['PrimitiveBoolean'],0),pos(47,39)),type(['PrimitiveBoolean'],0),pos(5,39))],pos(1,1)),(1,37))]),assertions([fact('all'([identifier('L',type(['List'],1),pos(19,30))],[field(identifier('L',type(['List'],1),pos(23,30)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(26,30)),type(['List'],1),[],pos(23,30))],'in'(identifier('L',type(['List'],1),pos(23,30)),'join'(identifier('L',type(['List'],1),pos(23,30)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(39,30)),type(['PrimitiveBoolean'],0),pos(35,30)),type(['PrimitiveBoolean'],0),pos(19,30)),(1,30)),fact('all'([identifier('a',type(['List'],1),pos(19,32)), identifier('b',type(['List'],1),pos(19,32))],[field(identifier('a',type(['List'],1),pos(23,32)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(28,32)),type(['List'],1),[],pos(23,32)),field(identifier('b',type(['List'],1),pos(25,32)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(28,32)),type(['List'],1),[],pos(23,32))],'iff'('in'(identifier('a',type(['List'],1),pos(23,32)),'join'(identifier('b',type(['List'],1),pos(25,32)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(41,32)),type(['PrimitiveBoolean'],0),pos(37,32)),'in'(identifier('b',type(['List'],1),pos(25,32)),'join'(identifier('a',type(['List'],1),pos(23,32)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(60,32)),type(['PrimitiveBoolean'],0),pos(56,32)),type(['PrimitiveBoolean'],0),pos(50,32)),type(['PrimitiveBoolean'],0),pos(19,32)),(1,32)),fact('all'([identifier('a',type(['List'],1),pos(17,34)), identifier('b',type(['List'],1),pos(17,34))],[field(identifier('a',type(['List'],1),pos(21,34)),'oneof'(identifier('EmptyList',type(['List'],1),pos(5,22)),type(['List'],1),pos(26,34)),type(['List'],1),[],pos(21,34)),field(identifier('b',type(['List'],1),pos(23,34)),'oneof'(identifier('EmptyList',type(['List'],1),pos(5,22)),type(['List'],1),pos(26,34)),type(['List'],1),[],pos(21,34))],'in'(identifier('a',type(['List'],1),pos(21,34)),'join'(identifier('b',type(['List'],1),pos(23,34)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(44,34)),type(['PrimitiveBoolean'],0),pos(40,34)),type(['PrimitiveBoolean'],0),pos(17,34)),(1,34))]),commands([run('and'(['in'(identifier('Thing',type(['Thing'],1),pos(5,11)),'join'(identifier('List',type(['List'],1),pos(14,14)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(34,12)),type(['PrimitiveBoolean'],0),pos(27,12)), 'all'([identifier('L',type(['List'],1),pos(14,25))],[field(identifier('L',type(['List'],1),pos(18,25)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(21,25)),type(['List'],1),[],pos(18,25))],pred_call('isFinite',[identifier('L',type(['List'],1),pos(18,25))],type(['PrimitiveBoolean'],0),pos(28,25)),type(['PrimitiveBoolean'],0),pos(14,25)), 'all'([identifier('a',type(['List'],1),pos(5,28)), identifier('b',type(['List'],1),pos(5,28))],[field(identifier('a',type(['List'],1),pos(9,28)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(14,28)),type(['List'],1),[],pos(9,28)),field(identifier('b',type(['List'],1),pos(11,28)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(14,28)),type(['List'],1),[],pos(9,28))],'iff'('in'(identifier('a',type(['List'],1),pos(9,28)),'join'(identifier('b',type(['List'],1),pos(11,28)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(28,28)),type(['PrimitiveBoolean'],0),pos(24,28)),'and'(['equal'('join'(identifier('a',type(['List'],1),pos(9,28)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(45,28)),'join'(identifier('b',type(['List'],1),pos(11,28)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(53,28)),type(['PrimitiveBoolean'],0),pos(50,28)), 'in'('join'(identifier('b',type(['List'],1),pos(11,28)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(63,28)),'join'('join'(identifier('a',type(['List'],1),pos(9,28)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(72,28)),identifier('equivTo',type(['List', 'List'],2),pos(5,15)),type(['List'],1),pos(76,28)),type(['PrimitiveBoolean'],0),pos(68,28)), 'equal'('card'('join'(identifier('a',type(['List'],1),pos(9,28)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(94,28)),type(['univ'],1),pos(93,28)),type(['Int'],1),pos(91,28)),'card'('join'(identifier('b',type(['List'],1),pos(11,28)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(104,28)),type(['univ'],1),pos(103,28)),type(['Int'],1),pos(101,28)),type(['PrimitiveBoolean'],0),pos(99,28))],pos(86,28)),type(['PrimitiveBoolean'],0),pos(38,28)),type(['PrimitiveBoolean'],0),pos(5,28)), 'all'([identifier('e',type(['List'],1),pos(5,38)), identifier('L',type(['List'],1),pos(5,38))],[field(identifier('e',type(['List'],1),pos(9,38)),'oneof'(identifier('EmptyList',type(['List'],1),pos(5,22)),type(['List'],1),pos(12,38)),type(['List'],1),[],pos(9,38)), field(identifier('L',type(['List'],1),pos(23,38)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(25,38)),type(['List'],1),[],pos(23,38))],'in'(identifier('e',type(['List'],1),pos(9,38)),'join'(identifier('L',type(['List'],1),pos(23,38)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(38,38)),type(['PrimitiveBoolean'],0),pos(34,38)),type(['PrimitiveBoolean'],0),pos(5,38)), 'all'([identifier('a',type(['List'],1),pos(5,39)), identifier('b',type(['List'],1),pos(5,39))],[field(identifier('a',type(['List'],1),pos(9,39)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(14,39)),type(['List'],1),[],pos(9,39)),field(identifier('b',type(['List'],1),pos(11,39)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(14,39)),type(['List'],1),[],pos(9,39))],'iff'('in'(identifier('a',type(['List'],1),pos(9,39)),'join'(identifier('b',type(['List'],1),pos(11,39)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(36,39)),type(['PrimitiveBoolean'],0),pos(32,39)),'and'(['equal'('join'(identifier('a',type(['List'],1),pos(9,39)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(53,39)),'join'(identifier('b',type(['List'],1),pos(11,39)),identifier('car',type(['List', 'Thing'],2),pos(5,19)),type(['Thing'],1),pos(61,39)),type(['PrimitiveBoolean'],0),pos(58,39)), 'in'('join'(identifier('a',type(['List'],1),pos(9,39)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(54,40)),'join'('join'(identifier('b',type(['List'],1),pos(11,39)),identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['List'],1),pos(63,40)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(67,40)),type(['PrimitiveBoolean'],0),pos(59,40)), 'less'('card'('join'(identifier('a',type(['List'],1),pos(9,39)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(56,41)),type(['univ'],1),pos(55,41)),type(['Int'],1),pos(53,41)),'card'('join'(identifier('b',type(['List'],1),pos(11,39)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(66,41)),type(['univ'],1),pos(65,41)),type(['Int'],1),pos(63,41)),type(['PrimitiveBoolean'],0),pos(61,41))],pos(49,41)),type(['PrimitiveBoolean'],0),pos(47,39)),type(['PrimitiveBoolean'],0),pos(5,39)), 'some'([identifier('a',type(['List'],1),pos(5,45)), identifier('b',type(['List'],1),pos(5,45))],[field(identifier('a',type(['List'],1),pos(10,45)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(16,45)),type(['List'],1),[],pos(10,45)),field(identifier('b',type(['List'],1),pos(13,45)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(16,45)),type(['List'],1),[],pos(10,45))],'and'(['not_equal'(identifier('a',type(['List'],1),pos(10,45)),identifier('b',type(['List'],1),pos(13,45)),type(['PrimitiveBoolean'],0),pos(32,45)), 'in'(identifier('b',type(['List'],1),pos(13,45)),'join'(identifier('a',type(['List'],1),pos(10,45)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(45,45)),type(['PrimitiveBoolean'],0),pos(41,45))],pos(36,45)),type(['PrimitiveBoolean'],0),pos(5,45))],pos(1,12)),global_scope(-1),exact_scopes([('List',4)]),upper_bound_scopes([('Thing',1)]),bitwidth(-1),maxseq(-1),index(0),pos(1,48))]),functions([predicate('isFinite',[identifier('L',type(['List'],1),pos(16,24))],[field(identifier('L',type(['List'],1),pos(16,24)),identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),[],pos(16,24))],'some'([identifier('e',type(['List'],1),pos(25,24))],[field(identifier('e',type(['List'],1),pos(30,24)),'oneof'(identifier('EmptyList',type(['List'],1),pos(5,22)),type(['List'],1),pos(33,24)),type(['List'],1),[],pos(30,24))],'in'(identifier('e',type(['List'],1),pos(30,24)),'join'(identifier('L',type(['List'],1),pos(16,24)),'closure'(identifier('cdr',type(['List', 'List'],2),pos(5,20)),type(['univ', 'univ'],2),pos(52,24)),type(['univ'],1),pos(51,24)),type(['PrimitiveBoolean'],0),pos(47,24)),type(['PrimitiveBoolean'],0),pos(25,24)),pos(1,24)),predicate('show',[],[],'some'([identifier('a',type(['List'],1),pos(5,45)), identifier('b',type(['List'],1),pos(5,45))],[field(identifier('a',type(['List'],1),pos(10,45)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(16,45)),type(['List'],1),[],pos(10,45)),field(identifier('b',type(['List'],1),pos(13,45)),'oneof'(identifier('NonEmptyList',type(['List'],1),pos(5,18)),type(['List'],1),pos(16,45)),type(['List'],1),[],pos(10,45))],'and'(['not_equal'(identifier('a',type(['List'],1),pos(10,45)),identifier('b',type(['List'],1),pos(13,45)),type(['PrimitiveBoolean'],0),pos(32,45)), 'in'(identifier('b',type(['List'],1),pos(13,45)),'join'(identifier('a',type(['List'],1),pos(10,45)),identifier('prefixes',type(['List', 'List'],2),pos(5,16)),type(['List'],1),pos(45,45)),type(['PrimitiveBoolean'],0),pos(41,45))],pos(36,45)),type(['PrimitiveBoolean'],0),pos(5,45)),pos(1,44))]),signatures([signature('Thing',[],[],[],pos(5,11)),signature('List',[field(identifier('equivTo',type(['List', 'List'],2),pos(5,15)),'setof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(14,15)),type(['List'],1),[],pos(5,15)),field(identifier('prefixes',type(['List', 'List'],2),pos(5,16)),'setof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(15,16)),type(['List'],1),[],pos(5,16))],[],[abstract],pos(14,14)),signature('NonEmptyList',[field(identifier('car',type(['List', 'Thing'],2),pos(5,19)),'oneof'(identifier('Thing',type(['Thing'],1),pos(5,11)),type(['Thing'],1),pos(10,19)),type(['Thing'],1),[],pos(5,19)),field(identifier('cdr',type(['List', 'List'],2),pos(5,20)),'oneof'(identifier('List',type(['List'],1),pos(14,14)),type(['List'],1),pos(10,20)),type(['List'],1),[],pos(5,20))],[],[subsig('List')],pos(5,18)),signature('EmptyList',[],[],[subsig('List')],pos(5,22))]),ordered_signatures([]),[sequences:false,parent_types:[],ordering_successors_only:true]),alloy_model(('util''integer','integer'),facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field(identifier('n1',type(['Int'],1),pos(11,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field(identifier('n2',type(['Int'],1),pos(15,11)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field(identifier('n1',type(['Int'],1),pos(11,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field(identifier('n2',type(['Int'],1),pos(15,12)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'cast2sigint'('integer''plus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,12)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,12)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,12)),type(['Int'],1),pos(30,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field(identifier('n1',type(['Int'],1),pos(12,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field(identifier('n2',type(['Int'],1),pos(16,14)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field(identifier('n1',type(['Int'],1),pos(12,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field(identifier('n2',type(['Int'],1),pos(16,15)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'cast2sigint'('integer''minus'('cast2int'(identifier('n1',type(['Int'],1),pos(12,15)),type(['Int'],1),pos(33,15)),'cast2int'(identifier('n2',type(['Int'],1),pos(16,15)),type(['Int'],1),pos(44,15)),type(['Int'],1),pos(36,15)),type(['Int'],1),pos(31,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field(identifier('n1',type(['Int'],1),pos(10,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field(identifier('n2',type(['Int'],1),pos(14,17)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'cast2sigint'('closure'('cast2int'(identifier('n1',type(['Int'],1),pos(10,17)),type(['Int'],1),pos(31,17)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,17)),type(['Int'],1),pos(42,17)),type(['Int'],1),pos(34,17)),type(['Int'],1),pos(29,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field(identifier('n1',type(['Int'],1),pos(10,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field(identifier('n2',type(['Int'],1),pos(14,25)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'cast2sigint'('integer''div'('cast2int'(identifier('n1',type(['Int'],1),pos(10,25)),type(['Int'],1),pos(31,25)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,25)),type(['Int'],1),pos(42,25)),type(['Int'],1),pos(34,25)),type(['Int'],1),pos(29,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field(identifier('n1',type(['Int'],1),pos(10,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field(identifier('n2',type(['Int'],1),pos(14,28)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'cast2sigint'('integer''rem'('cast2int'(identifier('n1',type(['Int'],1),pos(10,28)),type(['Int'],1),pos(31,28)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,28)),type(['Int'],1),pos(42,28)),type(['Int'],1),pos(34,28)),type(['Int'],1),pos(29,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field(identifier('n',type(['Int'],1),pos(13,31)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'cast2sigint'('integer''minus'(integer(0,pos(29,31)),'cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),type(['Int'],1),pos(31,31)),type(['Int'],1),pos(27,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field(identifier('n1',type(['Int'],1),pos(10,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field(identifier('n2',type(['Int'],1),pos(14,34)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'('cast2sigint'('cast2int'(identifier('n1',type(['Int'],1),pos(10,34)),type(['Int'],1),pos(25,34)),type(['Int'],1),pos(25,34)),'cast2sigint'('cast2int'(identifier('n2',type(['Int'],1),pos(14,34)),type(['Int'],1),pos(35,34)),type(['Int'],1),pos(35,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field(identifier('n1',type(['Int'],1),pos(10,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field(identifier('n2',type(['Int'],1),pos(14,37)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'('cast2int'(identifier('n1',type(['Int'],1),pos(10,37)),type(['Int'],1),pos(25,37)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,37)),type(['Int'],1),pos(30,37)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field(identifier('n1',type(['Int'],1),pos(10,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field(identifier('n2',type(['Int'],1),pos(14,40)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'('cast2int'(identifier('n1',type(['Int'],1),pos(10,40)),type(['Int'],1),pos(25,40)),'cast2int'(identifier('n2',type(['Int'],1),pos(14,40)),type(['Int'],1),pos(30,40)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field(identifier('n1',type(['Int'],1),pos(11,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field(identifier('n2',type(['Int'],1),pos(15,43)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,43)),type(['Int'],1),pos(26,43)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,43)),type(['Int'],1),pos(32,43)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field(identifier('n1',type(['Int'],1),pos(11,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field(identifier('n2',type(['Int'],1),pos(15,46)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,46)),type(['Int'],1),pos(26,46)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,46)),type(['Int'],1),pos(32,46)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field(identifier('n',type(['Int'],1),pos(12,49)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(12,49)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field(identifier('n',type(['Int'],1),pos(12,52)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'('cast2int'(identifier('n',type(['Int'],1),pos(12,52)),type(['Int'],1),pos(22,52)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field(identifier('n',type(['Int'],1),pos(12,55)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'('cast2int'(identifier('n',type(['Int'],1),pos(12,55)),type(['Int'],1),pos(22,55)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field(identifier('n',type(['Int'],1),pos(14,58)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,58)),type(['Int'],1),pos(24,58)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field(identifier('n',type(['Int'],1),pos(14,61)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'('cast2int'(identifier('n',type(['Int'],1),pos(14,61)),type(['Int'],1),pos(24,61)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field(identifier('n',type(['Int'],1),pos(13,64)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(29,64)),integer(0,pos(31,64)),type(['PrimitiveBoolean'],0),pos(30,64)),'integer''minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,64)),type(['Int'],1),pos(56,64)),integer(0,pos(58,64)),type(['PrimitiveBoolean'],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field(identifier('i',type(['Int'],1),pos(14,71)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field(identifier('next',type(['univ', 'univ'],2),pos(22,71)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field(identifier('s',type(['univ'],1),pos(40,71)),'setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'([identifier('e',type(['univ'],1),pos(3,72))],[field(identifier('e',type(['univ'],1),pos(4,72)),'oneof'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),'cast2sigint'('cast2int'(identifier('i',type(['Int'],1),pos(14,71)),type(['Int'],1),pos(22,72)),type(['Int'],1),pos(22,72)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field(identifier('e',type(['univ'],1),pos(14,80)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field(identifier('next',type(['univ', 'univ'],2),pos(23,80)),'cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(33,80)),type(['univ', 'univ'],2),[],pos(23,80))],'cast2sigint'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(23,80)),type(['univ', 'univ'],2),pos(8,81)),identifier('e',type(['univ'],1),pos(14,80)),type(['univ'],1),pos(13,81)),type(['Int'],1),pos(7,81)),type(['Int'],1),pos(52,80)),pos(1,80)),function('integer''max',[],[],'cast2sigint'(integer(max,pos(19,85)),type(['Int'],1),pos(17,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field(identifier('es',type(['Int'],1),pos(10,97)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],'cast2sigint'(integer(min,pos(19,88)),type(['Int'],1),pos(17,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field(identifier('es',type(['Int'],1),pos(10,100)),'setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,100)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,100)),'join'(identifier('es',type(['Int'],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field(identifier('e',type(['Int'],1),pos(12,103)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['Int'],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(34,103)),type(['Int', 'Int'],2),pos(33,103)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field(identifier('e',type(['Int'],1),pos(12,106)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['Int'],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(34,106)),type(['Int', 'Int'],2),pos(33,106)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field(identifier('e1',type(['Int'],1),pos(13,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field(identifier('e2',type(['Int'],1),pos(17,109)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let(identifier('a',type(['Int'],1),pos(37,109)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let(identifier('b',type(['Int'],1),pos(48,109)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field(identifier('e1',type(['Int'],1),pos(14,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field(identifier('e2',type(['Int'],1),pos(18,112)),identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let(identifier('a',type(['Int'],1),pos(38,112)),'cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(14,112)),type(['Int'],1),pos(40,112)),type(['Int'],1),pos(40,112)),let(identifier('b',type(['Int'],1),pos(49,112)),'cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(18,112)),type(['Int'],1),pos(51,112)),type(['Int'],1),pos(51,112)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(38,112)),type(['Int'],1),pos(62,112)),'cast2int'(identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(64,112)),type(['PrimitiveBoolean'],0),pos(63,112)),identifier('a',type(['Int'],1),pos(38,112)),identifier('b',type(['Int'],1),pos(49,112)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false,parent_types:[],ordering_successors_only:true])]).
