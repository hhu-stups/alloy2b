alloy('unknown',[alloy_model('unknown',facts([fact('and'(['all'(['r'],[field('r','oneof'(identifier('Row',type([['Row']],1),pos(5,10)),type([['Row']],1),pos(11,17)),type([['Row']],1),[],pos(9,17))],'and'(['or'(['in'(fun_call('integer''minus',['join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(24,17)), integer(1,pos(28,17))],type([['Int']],1),pos(17,17)),'join'(identifier('Row',type([['Row']],1),pos(5,10)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(37,17)),type([untyped],0),pos(31,17)), 'equal'('join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(45,17)),integer(1,pos(49,17)),type([untyped],0),pos(48,17))],pos(41,17)), 'greater'('join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(57,17)),integer(0,pos(61,17)),type([untyped],0),pos(60,17))],pos(52,17)),type([untyped],0),pos(5,17)), 'all'(['c'],[field('c','oneof'(identifier('Column',type([['Column']],1),pos(5,21)),type([['Column']],1),pos(11,18)),type([['Column']],1),[],pos(9,18))],'and'(['or'(['in'(fun_call('integer''minus',['join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(27,18)), integer(1,pos(31,18))],type([['Int']],1),pos(20,18)),'join'(identifier('Column',type([['Column']],1),pos(5,21)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(43,18)),type([untyped],0),pos(34,18)), 'equal'('join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(51,18)),integer(1,pos(55,18)),type([untyped],0),pos(54,18))],pos(47,18)), 'greater'('join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(63,18)),integer(0,pos(67,18)),type([untyped],0),pos(66,18))],pos(58,18)),type([untyped],0),pos(5,18))],pos(1,1)),(1,16))]),assertions([]),commands([run('and'(['all'(['r'],[field('r','oneof'(identifier('Row',type([['Row']],1),pos(5,10)),type([['Row']],1),pos(11,17)),type([['Row']],1),[],pos(9,17))],'and'(['or'(['in'(fun_call('integer''minus',['join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(24,17)), integer(1,pos(28,17))],type([['Int']],1),pos(17,17)),'join'(identifier('Row',type([['Row']],1),pos(5,10)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(37,17)),type([untyped],0),pos(31,17)), 'equal'('join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(45,17)),integer(1,pos(49,17)),type([untyped],0),pos(48,17))],pos(41,17)), 'greater'('join'(identifier('r',type([['Row']],1),pos(9,17)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(57,17)),integer(0,pos(61,17)),type([untyped],0),pos(60,17))],pos(52,17)),type([untyped],0),pos(5,17)), 'all'(['c'],[field('c','oneof'(identifier('Column',type([['Column']],1),pos(5,21)),type([['Column']],1),pos(11,18)),type([['Column']],1),[],pos(9,18))],'and'(['or'(['in'(fun_call('integer''minus',['join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(27,18)), integer(1,pos(31,18))],type([['Int']],1),pos(20,18)),'join'(identifier('Column',type([['Column']],1),pos(5,21)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(43,18)),type([untyped],0),pos(34,18)), 'equal'('join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(51,18)),integer(1,pos(55,18)),type([untyped],0),pos(54,18))],pos(47,18)), 'greater'('join'(identifier('c',type([['Column']],1),pos(9,18)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(63,18)),integer(0,pos(67,18)),type([untyped],0),pos(66,18))],pos(58,18)),type([untyped],0),pos(5,18)), 'no'(['q1','q2'],[field('q1','oneof'(identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),pos(18,41)),type([['Queen']],1),[disj],pos(12,41)),field('q2','oneof'(identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),pos(18,41)),type([['Queen']],1),[disj],pos(12,41))],pred_call('aligned',[identifier('q1',type([['Queen']],1),pos(12,41)), identifier('q2',type([['Queen']],1),pos(15,41))],type([untyped],0),pos(25,41)),type([untyped],0),pos(4,41))],pos(5,17)),global_scope(-1),exact_scopes([('Row',4), ('Column',4), ('Queen',4)]),upper_bound_scopes([]),bitwidth(4),maxseq(-1),index(0),pos(1,43))]),functions([predicate('aligned',[identifier('q1',type([['Queen']],1),pos(14,30)), identifier('q2',type([['Queen']],1),pos(17,30))],[field('q1',identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),[],pos(14,30)),field('q2',identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),[],pos(14,30))],'or'(['equal'('join'(identifier('q1',type([['Queen']],1),pos(14,30)),identifier('y',type([['Queen', 'Row']],2),pos(5,28)),type([['Row']],1),pos(6,31)),'join'(identifier('q2',type([['Queen']],1),pos(17,30)),identifier('y',type([['Queen', 'Row']],2),pos(5,28)),type([['Row']],1),pos(11,31)),type([untyped],0),pos(8,31)), 'equal'('join'(identifier('q1',type([['Queen']],1),pos(14,30)),identifier('x',type([['Queen', 'Column']],2),pos(5,27)),type([['Column']],1),pos(6,33)),'join'(identifier('q2',type([['Queen']],1),pos(17,30)),identifier('x',type([['Queen', 'Column']],2),pos(5,27)),type([['Column']],1),pos(11,33)),type([untyped],0),pos(8,33)), 'some'(['a','b'],[field('a','oneof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(16,34)),type([['Int']],1),[],pos(12,34)),field('b','oneof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(16,34)),type([['Int']],1),[],pos(12,34))],'and'(['equal'(fun_call('integer''plus',['join'('join'(identifier('q1',type([['Queen']],1),pos(14,30)),identifier('x',type([['Queen', 'Column']],2),pos(5,27)),type([['Column']],1),pos(15,35)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(17,35)), identifier('a',type([['Int']],1),pos(12,34))],type([['Int']],1),pos(8,35)),'join'('join'(identifier('q2',type([['Queen']],1),pos(17,30)),identifier('x',type([['Queen', 'Column']],2),pos(5,27)),type([['Column']],1),pos(27,35)),identifier('id',type([['Column', 'Int']],2),pos(3,22)),type([['Int']],1),pos(29,35)),type([untyped],0),pos(23,35)), 'equal'(fun_call('integer''plus',['join'('join'(identifier('q1',type([['Queen']],1),pos(14,30)),identifier('y',type([['Queen', 'Row']],2),pos(5,28)),type([['Row']],1),pos(15,36)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(17,36)), identifier('b',type([['Int']],1),pos(14,34))],type([['Int']],1),pos(8,36)),'join'('join'(identifier('q2',type([['Queen']],1),pos(17,30)),identifier('y',type([['Queen', 'Row']],2),pos(5,28)),type([['Row']],1),pos(27,36)),identifier('id',type([['Row', 'Int']],2),pos(3,11)),type([['Int']],1),pos(29,36)),type([untyped],0),pos(23,36)), 'or'(['equal'(identifier('a',type([['Int']],1),pos(12,34)),identifier('b',type([['Int']],1),pos(14,34)),type([untyped],0),pos(9,37)), 'equal'(identifier('a',type([['Int']],1),pos(12,34)),fun_call('integer''minus',[integer(0,pos(23,37)), identifier('b',type([['Int']],1),pos(14,34))],type([['Int']],1),pos(17,37)),type([untyped],0),pos(16,37))],pos(12,37))],pos(1,1)),type([untyped],0),pos(7,34))],pos(4,34)),pos(1,30)),predicate('Show',[],[],'no'(['q1','q2'],[field('q1','oneof'(identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),pos(18,41)),type([['Queen']],1),[disj],pos(12,41)),field('q2','oneof'(identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),pos(18,41)),type([['Queen']],1),[disj],pos(12,41))],pred_call('aligned',[identifier('q1',type([['Queen']],1),pos(12,41)), identifier('q2',type([['Queen']],1),pos(15,41))],type([untyped],0),pos(25,41)),type([untyped],0),pos(4,41)),pos(1,40))]),signatures([signature('Board',[field('cols','setof'(identifier('Column',type([['Column']],1),pos(5,21)),type([['Column']],1),pos(11,4)),type([['Column']],1),[],pos(5,4)),field('rows','setof'(identifier('Row',type([['Row']],1),pos(5,10)),type([['Row']],1),pos(11,5)),type([['Row']],1),[],pos(5,5)),field('q','setof'(identifier('Queen',type([['Queen']],1),pos(5,26)),type([['Queen']],1),pos(8,6)),type([['Queen']],1),[],pos(5,6))],[],[one],pos(9,3)),signature('Row',[field('id','oneof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(12,11)),type([['Int']],1),[disj],pos(3,11))],['in'(identifier(this,type([['Row']],1),pos(1,1)),'join'(identifier('Board',type([['Board']],1),pos(9,3)),identifier('rows',type([['Board', 'Row']],2),pos(5,5)),type([['Row']],1),pos(17,13)),type([untyped],0),pos(9,13))],[],pos(5,10)),signature('Column',[field('id','oneof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(12,22)),type([['Int']],1),[disj],pos(3,22))],['in'(identifier(this,type([['Column']],1),pos(1,1)),'join'(identifier('Board',type([['Board']],1),pos(9,3)),identifier('cols',type([['Board', 'Column']],2),pos(5,4)),type([['Column']],1),pos(16,24)),type([untyped],0),pos(8,24))],[],pos(5,21)),signature('Queen',[field('x','oneof'(identifier('Column',type([['Column']],1),pos(5,21)),type([['Column']],1),pos(7,27)),type([['Column']],1),[],pos(5,27)),field('y','oneof'(identifier('Row',type([['Row']],1),pos(5,10)),type([['Row']],1),pos(7,28)),type([['Row']],1),[],pos(5,28))],[],[],pos(5,26))]),ordered_signatures([]),[sequences:false]),alloy_model('util''integer',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type([['Int']],1),pos(11,11)), identifier('n2',type([['Int']],1),pos(15,11))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,11)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type([['Int']],1),pos(11,11)), identifier('n2',type([['Int']],1),pos(15,11))],type([['Int']],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type([['Int']],1),pos(11,12)), identifier('n2',type([['Int']],1),pos(15,12))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,12)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,12))],'@+'(identifier('n1',type([['Int']],1),pos(11,12)),identifier('n2',type([['Int']],1),pos(15,12)),type([['Int']],1),pos(35,12)),pos(1,12)),function('integer''sub',[identifier('n1',type([['Int']],1),pos(12,14)), identifier('n2',type([['Int']],1),pos(16,14))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,14)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type([['Int']],1),pos(12,14)), identifier('n2',type([['Int']],1),pos(16,14))],type([['Int']],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type([['Int']],1),pos(12,15)), identifier('n2',type([['Int']],1),pos(16,15))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,15)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,15))],'@-'(identifier('n1',type([['Int']],1),pos(12,15)),identifier('n2',type([['Int']],1),pos(16,15)),type([['Int']],1),pos(36,15)),pos(1,15)),function('integer''mul',[identifier('n1',type([['Int']],1),pos(10,17)), identifier('n2',type([['Int']],1),pos(14,17))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,17)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,17))],'closure'(identifier('n1',type([['Int']],1),pos(10,17)),identifier('n2',type([['Int']],1),pos(14,17)),type([['Int']],1),pos(34,17)),pos(1,17)),function('integer''div',[identifier('n1',type([['Int']],1),pos(10,25)), identifier('n2',type([['Int']],1),pos(14,25))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,25)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,25))],'/'(identifier('n1',type([['Int']],1),pos(10,25)),identifier('n2',type([['Int']],1),pos(14,25)),type([['Int']],1),pos(34,25)),pos(1,25)),function('integer''rem',[identifier('n1',type([['Int']],1),pos(10,28)), identifier('n2',type([['Int']],1),pos(14,28))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,28)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,28))],'%'(identifier('n1',type([['Int']],1),pos(10,28)),identifier('n2',type([['Int']],1),pos(14,28)),type([['Int']],1),pos(34,28)),pos(1,28)),function('integer''negate',[identifier('n',type([['Int']],1),pos(13,31))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(13,31))],'@-'(integer(0,pos(29,31)),identifier('n',type([['Int']],1),pos(13,31)),type([['Int']],1),pos(31,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type([['Int']],1),pos(10,34)), identifier('n2',type([['Int']],1),pos(14,34))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,34)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,34))],'equal'(identifier('n1',type([['Int']],1),pos(10,34)),identifier('n2',type([['Int']],1),pos(14,34)),type([untyped],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type([['Int']],1),pos(10,37)), identifier('n2',type([['Int']],1),pos(14,37))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,37)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,37))],'greater'(identifier('n1',type([['Int']],1),pos(10,37)),identifier('n2',type([['Int']],1),pos(14,37)),type([untyped],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type([['Int']],1),pos(10,40)), identifier('n2',type([['Int']],1),pos(14,40))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,40)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(10,40))],'less'(identifier('n1',type([['Int']],1),pos(10,40)),identifier('n2',type([['Int']],1),pos(14,40)),type([untyped],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type([['Int']],1),pos(11,43)), identifier('n2',type([['Int']],1),pos(15,43))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,43)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,43))],'greater_equal'(identifier('n1',type([['Int']],1),pos(11,43)),identifier('n2',type([['Int']],1),pos(15,43)),type([untyped],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type([['Int']],1),pos(11,46)), identifier('n2',type([['Int']],1),pos(15,46))],[field('n1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,46)),field('n2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(11,46))],'less_equal'(identifier('n1',type([['Int']],1),pos(11,46)),identifier('n2',type([['Int']],1),pos(15,46)),type([untyped],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type([['Int']],1),pos(12,49))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,49))],'equal'(identifier('n',type([['Int']],1),pos(12,49)),integer(0,pos(26,49)),type([untyped],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type([['Int']],1),pos(12,52))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,52))],'greater'(identifier('n',type([['Int']],1),pos(12,52)),integer(0,pos(26,52)),type([untyped],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type([['Int']],1),pos(12,55))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,55))],'less'(identifier('n',type([['Int']],1),pos(12,55)),integer(0,pos(26,55)),type([untyped],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type([['Int']],1),pos(14,58))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(14,58))],'less_equal'(identifier('n',type([['Int']],1),pos(14,58)),integer(0,pos(29,58)),type([untyped],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type([['Int']],1),pos(14,61))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(14,61))],'greater_equal'(identifier('n',type([['Int']],1),pos(14,61)),integer(0,pos(29,61)),type([untyped],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type([['Int']],1),pos(13,64))],[field('n',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(13,64))],if_then_else('less'(identifier('n',type([['Int']],1),pos(13,64)),integer(0,pos(31,64)),type([untyped],0),pos(30,64)),'@-'(integer(0,pos(37,64)),integer(1,pos(47,64)),type([['Int']],1),pos(39,64)),if_then_else('greater'(identifier('n',type([['Int']],1),pos(13,64)),integer(0,pos(58,64)),type([untyped],0),pos(57,64)),integer(1,pos(63,64)),integer(0,pos(70,64)),type([['Int']],1),pos(60,64)),type([['Int']],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type([['Int']],1),pos(14,71)), identifier('next',type([['univ', 'univ']],2),pos(22,71)), identifier('s',type([['univ']],1),pos(40,71))],[field('i',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type([['univ']],1),pos(1,1)),identifier('univ',type([['univ']],1),pos(1,1)),type([['univ', 'univ']],2),pos(32,71)),type([['univ', 'univ']],2),[],pos(22,71)), field('s','setof'(identifier('univ',type([['univ']],1),pos(1,1)),type([['univ']],1),pos(43,71)),type([['univ']],1),[],pos(40,71))],'comprehension'(['e'],[field('e','oneof'(identifier('s',type([['univ']],1),pos(40,71)),type([['univ']],1),pos(7,72)),type([['univ']],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type([['univ', 'univ']],2),pos(22,71)),type([['univ', 'univ']],2),pos(12,72)),identifier('e',type([['univ']],1),pos(4,72)),type([['univ']],1),pos(17,72)),type([['Int']],1),pos(11,72)),identifier('i',type([['Int']],1),pos(14,71)),type([untyped],0),pos(20,72)),type([['univ']],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type([['univ']],1),pos(14,80)), identifier('next',type([['univ', 'univ']],2),pos(23,80))],[field('e',identifier('univ',type([['univ']],1),pos(1,1)),type([['univ']],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type([['univ']],1),pos(1,1)),identifier('univ',type([['univ']],1),pos(1,1)),type([['univ', 'univ']],2),pos(33,80)),type([['univ', 'univ']],2),[],pos(23,80))],'card'('join'('closure1'(identifier('next',type([['univ', 'univ']],2),pos(23,80)),type([['univ', 'univ']],2),pos(8,81)),identifier('e',type([['univ']],1),pos(14,80)),type([['univ']],1),pos(13,81)),type([['Int']],1),pos(7,81)),pos(1,80)),function('integer''max',[],[],integer(max,pos(19,85)),pos(1,85)),function('integer''max',[identifier('es',type([['Int']],1),pos(10,97))],[field('es','setof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(14,97)),type([['Int']],1),[],pos(10,97))],'minus'(identifier('es',type([['Int']],1),pos(10,97)),'join'(identifier('es',type([['Int']],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type([['Int', 'Int']],2),pos(44,97)),type([['Int', 'Int']],2),pos(43,97)),type([['Int']],1),pos(42,97)),type([['Int']],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],integer(min,pos(19,88)),pos(1,88)),function('integer''min',[identifier('es',type([['Int']],1),pos(10,100))],[field('es','setof'(identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),pos(14,100)),type([['Int']],1),[],pos(10,100))],'minus'(identifier('es',type([['Int']],1),pos(10,100)),'join'(identifier('es',type([['Int']],1),pos(10,100)),'closure1'(fun_call('integer''next',[],type([['Int', 'Int']],2),pos(44,100)),type([['Int', 'Int']],2),pos(43,100)),type([['Int']],1),pos(42,100)),type([['Int']],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],next(pos(21,91)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type([['Int', 'Int']],2),pos(22,94)),type([['Int', 'Int']],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type([['Int']],1),pos(12,103))],[field('e',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,103))],'join'(identifier('e',type([['Int']],1),pos(12,103)),'closure1'(fun_call('integer''prev',[],type([['Int', 'Int']],2),pos(34,103)),type([['Int', 'Int']],2),pos(33,103)),type([['Int']],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type([['Int']],1),pos(12,106))],[field('e',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(12,106))],'join'(identifier('e',type([['Int']],1),pos(12,106)),'closure1'(fun_call('integer''next',[],type([['Int', 'Int']],2),pos(34,106)),type([['Int', 'Int']],2),pos(33,106)),type([['Int']],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type([['Int']],1),pos(13,109)), identifier('e2',type([['Int']],1),pos(17,109))],[field('e1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(13,109)),field('e2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(13,109))],let('a',identifier('e1',type([['Int']],1),pos(13,109)),let('b',identifier('e2',type([['Int']],1),pos(17,109)),if_then_else('less'(identifier('a',type([['Int']],1),pos(37,109)),identifier('b',type([['Int']],1),pos(48,109)),type([untyped],0),pos(62,109)),identifier('b',type([['Int']],1),pos(48,109)),identifier('a',type([['Int']],1),pos(37,109)),type([['Int']],1),pos(65,109)),type([['Int']],1),pos(49,109)),type([['Int']],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type([['Int']],1),pos(14,112)), identifier('e2',type([['Int']],1),pos(18,112))],[field('e1',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(14,112)),field('e2',identifier('Int',type([['Int']],1),pos(1,1)),type([['Int']],1),[],pos(14,112))],let('a',identifier('e1',type([['Int']],1),pos(14,112)),let('b',identifier('e2',type([['Int']],1),pos(18,112)),if_then_else('less'(identifier('a',type([['Int']],1),pos(38,112)),identifier('b',type([['Int']],1),pos(49,112)),type([untyped],0),pos(63,112)),identifier('a',type([['Int']],1),pos(38,112)),identifier('b',type([['Int']],1),pos(49,112)),type([['Int']],1),pos(66,112)),type([['Int']],1),pos(50,112)),type([['Int']],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false])]).
