alloy_model(facts([fact(no(['p'],[field('p',oneof(identifier('Person',type([['Person']],1),pos(14,3)),type([['Person']],1),pos(12,17)),type([['Person']],1),[],pos(8,17))],in(identifier('p',type([['Person']],1),pos(8,17)),join(identifier('p',type([['Person']],1),pos(8,17)),closure1(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(37,17)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(28,17)),type([['Woman'], ['Man']],1),pos(27,17)),type([untyped],0),pos(23,17)),type([untyped],0),pos(5,17)),(1,16)),fact(equal(identifier('wife',type([['Man', 'Woman']],2),pos(5,9)),inverse(identifier('husband',type([['Woman', 'Man']],2),pos(5,13)),type([['Man', 'Woman']],2),pos(12,21)),type([untyped],0),pos(10,21)),(1,20)),fact(and([no(intersection(identifier('wife',type([['Man', 'Woman']],2),pos(5,9)),join(closure(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(24,25)),type([['univ', 'univ']],2),pos(15,25)),identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),type([['univ', 'Woman']],2),pos(33,25)),type([['Man', 'Woman']],2),pos(13,25)),type([untyped],0),pos(5,25)), no(intersection(identifier('husband',type([['Woman', 'Man']],2),pos(5,13)),join(closure(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(27,26)),type([['univ', 'univ']],2),pos(18,26)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['univ', 'Man']],2),pos(36,26)),type([['Woman', 'Man']],2),pos(16,26)),type([untyped],0),pos(5,26))],pos(1,1)),(1,24))]),assertions([]),commands([run(and([no(['p'],[field('p',oneof(identifier('Person',type([['Person']],1),pos(14,3)),type([['Person']],1),pos(12,17)),type([['Person']],1),[],pos(8,17))],in(identifier('p',type([['Person']],1),pos(8,17)),join(identifier('p',type([['Person']],1),pos(8,17)),closure1(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(37,17)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(28,17)),type([['Woman'], ['Man']],1),pos(27,17)),type([untyped],0),pos(23,17)),type([untyped],0),pos(5,17)), equal(identifier('wife',type([['Man', 'Woman']],2),pos(5,9)),inverse(identifier('husband',type([['Woman', 'Man']],2),pos(5,13)),type([['Man', 'Woman']],2),pos(12,21)),type([untyped],0),pos(10,21)), no(intersection(identifier('wife',type([['Man', 'Woman']],2),pos(5,9)),join(closure(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(24,25)),type([['univ', 'univ']],2),pos(15,25)),identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),type([['univ', 'Woman']],2),pos(33,25)),type([['Man', 'Woman']],2),pos(13,25)),type([untyped],0),pos(5,25)), no(intersection(identifier('husband',type([['Woman', 'Man']],2),pos(5,13)),join(closure(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(27,26)),type([['univ', 'univ']],2),pos(18,26)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['univ', 'Man']],2),pos(36,26)),type([['Woman', 'Man']],2),pos(16,26)),type([untyped],0),pos(5,26)), some(['m'],[field('m',identifier('Man',type([['Man']],1),pos(5,8)),type([['Man']],1),[],pos(17,34))],in(identifier('m',type([['Man']],1),pos(17,34)),fun_call('grandpas',[identifier('m',type([['Man']],1),pos(17,34))],type([['Person']],1),pos(10,35)),type([untyped],0),pos(7,35)),type([untyped],0),pos(1,1))],pos(1,16)),global_scope(-1),exact_scopes([]),upper_bound_scopes([('Person',4)]),bitwidth(-1),pos(1,38))]),functions([function('grandpas',[identifier('p',type([['Person']],1),pos(14,29))],[field('p',identifier('Person',type([['Person']],1),pos(14,3)),type([['Person']],1),[],pos(14,29))],let('parent',plus(plus(plus(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('father',type([['Person', 'Man']],2),pos(5,4)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(25,30)),join(identifier('father',type([['Person', 'Man']],2),pos(5,4)),identifier('wife',type([['Man', 'Woman']],2),pos(5,9)),type([['Person', 'Woman']],2),pos(42,30)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(34,30)),join(identifier('mother',type([['Person', 'Woman']],2),pos(5,5)),identifier('husband',type([['Woman', 'Man']],2),pos(5,13)),type([['Person', 'Man']],2),pos(56,30)),type([['Person', 'Woman'], ['Person', 'Man']],2),pos(48,30)),intersection(join(join(identifier('p',type([['Person']],1),pos(14,29)),identifier('parent',type([['Person', 'Woman'], ['Person', 'Man']],2),pos(9,30)),type([['Woman'], ['Man']],1),pos(8,31)),identifier('parent',type([['Person', 'Woman'], ['Person', 'Man']],2),pos(9,30)),type([['Woman'], ['Man']],1),pos(15,31)),identifier('Man',type([['Man']],1),pos(5,8)),type([['Man']],1),pos(23,31)),type([['Man']],1),pos(16,30)),pos(1,29)),predicate('ownGrandpa',[identifier('m',type([['Man']],1),pos(17,34))],[field('m',identifier('Man',type([['Man']],1),pos(5,8)),type([['Man']],1),[],pos(17,34))],in(identifier('m',type([['Man']],1),pos(17,34)),fun_call('grandpas',[identifier('m',type([['Man']],1),pos(17,34))],type([['Person']],1),pos(10,35)),type([untyped],0),pos(7,35)),pos(1,34))]),signatures([signature('Person',[field('father',loneof(identifier('Man',type([['Man']],1),pos(5,8)),type([['Man']],1),pos(14,4)),type([['Man']],1),[],pos(5,4)),field('mother',loneof(identifier('Woman',type([['Woman']],1),pos(5,12)),type([['Woman']],1),pos(14,5)),type([['Woman']],1),[],pos(5,5))],[],[abstract],pos(14,3)),signature('Man',[field('wife',loneof(identifier('Woman',type([['Woman']],1),pos(5,12)),type([['Woman']],1),pos(12,9)),type([['Woman']],1),[],pos(5,9))],[],[subsig('Person')],pos(5,8)),signature('Woman',[field('husband',loneof(identifier('Man',type([['Man']],1),pos(5,8)),type([['Man']],1),pos(15,13)),type([['Man']],1),[],pos(5,13))],[],[subsig('Person')],pos(5,12))])).