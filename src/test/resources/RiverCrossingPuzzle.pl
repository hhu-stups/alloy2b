alloy_model(facts([fact(equal(identifier('eats',type([['Object', 'Object']],2),pos(23,5)),plus(cartesian(identifier('Fox',type([['Fox']],1),pos(17,6)),identifier('Chicken',type([['Chicken']],1),pos(22,6)),type([['Fox', 'Chicken']],2),pos(18,9)),cartesian(identifier('Chicken',type([['Chicken']],1),pos(22,6)),identifier('Grain',type([['Grain']],1),pos(31,6)),type([['Chicken', 'Grain']],2),pos(37,9)),type([['Fox', 'Chicken'], ['Chicken', 'Grain']],2),pos(28,9)),type([untyped],0),pos(13,9)),(1,9)),fact(and([equal(join(fun_call('ordering''first',[],type([['State']],1),pos(8,15)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(13,15)),identifier('Object',type([['Object']],1),pos(14,5)),type([untyped],0),pos(19,15)), no(join(fun_call('ordering''first',[],type([['State']],1),pos(34,15)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(39,15)),type([untyped],0),pos(31,15))],pos(28,15)),(1,15)),fact(all(['s', 's_'],[field('s',oneof(identifier('State',type([['State']],1),pos(5,12)),type([['State']],1),pos(10,27)),type([['State']],1),[],pos(7,27)), field('s_',oneof(join(identifier('s',type([['State']],1),pos(7,27)),fun_call('ordering''next',[],type([['State', 'State']],2),pos(23,27)),type([['State']],1),pos(22,27)),type([['State']],1),pos(21,27)),type([['State']],1),[],pos(17,27))],if_then_else(in(identifier('Farmer',type([['Farmer']],1),pos(9,6)),join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(16,28)),type([untyped],0),pos(12,28)),pred_call('crossRiver',[join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(20,29)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(29,29)), join(identifier('s',type([['State']],1),pos(7,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(37,29)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(45,29))],type([untyped],0),pos(7,29)),pred_call('crossRiver',[join(identifier('s',type([['State']],1),pos(7,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(20,31)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(28,31)), join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(35,31)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(44,31))],type([untyped],0),pos(7,31)),type([untyped],0),pos(22,28)),type([untyped],0),pos(3,27)),(1,26))]),assertions([]),commands([run(and([equal(identifier('eats',type([['Object', 'Object']],2),pos(23,5)),plus(cartesian(identifier('Fox',type([['Fox']],1),pos(17,6)),identifier('Chicken',type([['Chicken']],1),pos(22,6)),type([['Fox', 'Chicken']],2),pos(18,9)),cartesian(identifier('Chicken',type([['Chicken']],1),pos(22,6)),identifier('Grain',type([['Grain']],1),pos(31,6)),type([['Chicken', 'Grain']],2),pos(37,9)),type([['Fox', 'Chicken'], ['Chicken', 'Grain']],2),pos(28,9)),type([untyped],0),pos(13,9)), equal(join(fun_call('ordering''first',[],type([['State']],1),pos(8,15)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(13,15)),identifier('Object',type([['Object']],1),pos(14,5)),type([untyped],0),pos(19,15)), no(join(fun_call('ordering''first',[],type([['State']],1),pos(34,15)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(39,15)),type([untyped],0),pos(31,15)), all(['s', 's_'],[field('s',oneof(identifier('State',type([['State']],1),pos(5,12)),type([['State']],1),pos(10,27)),type([['State']],1),[],pos(7,27)), field('s_',oneof(join(identifier('s',type([['State']],1),pos(7,27)),fun_call('ordering''next',[],type([['State', 'State']],2),pos(23,27)),type([['State']],1),pos(22,27)),type([['State']],1),pos(21,27)),type([['State']],1),[],pos(17,27))],if_then_else(in(identifier('Farmer',type([['Farmer']],1),pos(9,6)),join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(16,28)),type([untyped],0),pos(12,28)),pred_call('crossRiver',[join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(20,29)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(29,29)), join(identifier('s',type([['State']],1),pos(7,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(37,29)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(45,29))],type([untyped],0),pos(7,29)),pred_call('crossRiver',[join(identifier('s',type([['State']],1),pos(7,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(20,31)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(28,31)), join(identifier('s',type([['State']],1),pos(7,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(35,31)), join(identifier('s_',type([['State']],1),pos(17,27)),identifier('near',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(44,31))],type([untyped],0),pos(7,31)),type([untyped],0),pos(22,28)),type([untyped],0),pos(3,27)), equal(join(fun_call('ordering''last',[],type([['State']],1),pos(7,36)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(11,36)),identifier('Object',type([['Object']],1),pos(14,5)),type([untyped],0),pos(15,36))],pos(1,9)),global_scope(-1),exact_scopes([('State',8)]),bitwidth(-1),pos(1,36))]),functions([predicate('crossRiver',[identifier('from',type([['Object']],1),pos(18,18)), identifier('from_',type([['Object']],1),pos(24,18)), identifier('to',type([['Object']],1),pos(31,18)), identifier('to_',type([['Object']],1),pos(35,18))],[field('from',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(40,18)),type([['Object']],1),[],pos(18,18)),field('from_',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(40,18)),type([['Object']],1),[],pos(18,18)),field('to',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(40,18)),type([['Object']],1),[],pos(18,18)),field('to_',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(40,18)),type([['Object']],1),[],pos(18,18))],one(['x'],[field('x',oneof(identifier('from',type([['Object']],1),pos(18,18)),type([['Object']],1),pos(10,19)),type([['Object']],1),[],pos(7,19))],and([equal(identifier('from_',type([['Object']],1),pos(24,18)),minus(minus(minus(identifier('from',type([['Object']],1),pos(18,18)),identifier('x',type([['Object']],1),pos(7,19)),type([['Object']],1),pos(18,20)),identifier('Farmer',type([['Farmer']],1),pos(9,6)),type([['Object']],1),pos(22,20)),join(identifier('from_',type([['Object']],1),pos(24,18)),identifier('eats',type([['Object', 'Object']],2),pos(23,5)),type([['Object']],1),pos(38,20)),type([['Object']],1),pos(31,20)),type([untyped],0),pos(11,20)), equal(identifier('to_',type([['Object']],1),pos(35,18)),plus(plus(identifier('to',type([['Object']],1),pos(31,18)),identifier('x',type([['Object']],1),pos(7,19)),type([['Object']],1),pos(14,21)),identifier('Farmer',type([['Farmer']],1),pos(9,6)),type([['Object']],1),pos(18,21)),type([untyped],0),pos(9,21))],pos(1,1)),type([untyped],0),pos(3,19)),pos(1,18)),predicate('run$1',[],[],equal(join(fun_call('ordering''last',[],type([['State']],1),pos(7,36)),identifier('far',type([['State', 'Object']],2),pos(13,12)),type([['Object']],1),pos(11,36)),identifier('Object',type([['Object']],1),pos(14,5)),type([untyped],0),pos(15,36)),pos(1,36))]),signatures([signature('Object',[field('eats',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(29,5)),type([['Object']],1),[],pos(23,5))],[],[abstract],pos(14,5)),signature('Farmer',[],[],[one, subsig('Object')],pos(9,6)),signature('Fox',[],[],[one, subsig('Object')],pos(17,6)),signature('Chicken',[],[],[one, subsig('Object')],pos(22,6)),signature('Grain',[],[],[one, subsig('Object')],pos(31,6)),signature('State',[field('near',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(24,12)),type([['Object']],1),[],pos(13,12)),field('far',setof(identifier('Object',type([['Object']],1),pos(14,5)),type([['Object']],1),pos(24,12)),type([['Object']],1),[],pos(13,12))],[],[ordered],pos(5,12))])).