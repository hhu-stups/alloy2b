alloy('JobsPuzzle',[alloy_model('JobsPuzzle',facts([fact('and'(['all'(['p','p_'],[field('p','one_of'(identifier('Person',type(['Person'],1),pos(14,34)),type(['Person'],1),pos(19,42)),type(['Person'],1),[disj],pos(11,42)),field('p_','one_of'(identifier('Person',type(['Person'],1),pos(14,34)),type(['Person'],1),pos(19,42)),type(['Person'],1),[disj],pos(11,42))],'no'('intersection'('join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),'join'(identifier('p_',type(['Person'],1),pos(14,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(42,42)),type(['Job'],1),pos(38,42)),type(['PrimitiveBoolean'],0),pos(28,42)),type(['PrimitiveBoolean'],0),pos(2,42)), 'one'(['p'],[field('p','one_of'(identifier('Male',type(['Male'],1),pos(14,25)),type(['Male'],1),pos(10,44)),type(['Male'],1),[],pos(6,44))],'in'(identifier('actor',type(['Job'],1),pos(42,52)),'join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),type(['PrimitiveBoolean'],0),pos(23,44)),type(['PrimitiveBoolean'],0),pos(2,44))],pos(1,1)),(1,40)),fact('one'(['p'],[field('p','one_of'(identifier('Male',type(['Male'],1),pos(14,25)),type(['Male'],1),pos(10,44)),type(['Male'],1),[],pos(6,56))],'in'(identifier('nurse',type(['Job'],1),pos(9,51)),'join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),type(['PrimitiveBoolean'],0),pos(23,56)),type(['PrimitiveBoolean'],0),pos(2,56)),(1,55)),fact('and'([pred_call('relation''injective',[identifier('husband',type(['Female', 'Male'],2),pos(2,21)), identifier('Male',type(['Male'],1),pos(14,25))],type(['PrimitiveBoolean'],0),pos(2,64)), 'equal'('join'('join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('chef',type(['Job'],1),pos(9,52)),type(['Person'],1),pos(14,65)),identifier('husband',type(['Female', 'Male'],2),pos(2,21)),type(['Male'],1),pos(2,65)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('telephone_operator',type(['Job'],1),pos(22,52)),type(['Person'],1),pos(26,65)),type(['PrimitiveBoolean'],0),pos(20,65))],pos(1,1)),(1,63)),fact('not_in'(identifier('boxer',type(['Job'],1),pos(49,52)),'join'(identifier('Roberta',type(['Person'],1),pos(9,31)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(22,70)),type(['PrimitiveBoolean'],0),pos(8,70)),(1,69)),fact('in'('join'(identifier('Pete',type(['Person'],1),pos(16,32)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(6,76)),'minus'(identifier('Job',type(['Job'],1),pos(14,49)),identifier('QualifiedJob',type(['Job'],1),pos(14,50)),type(['Job'],1),pos(19,76)),type(['PrimitiveBoolean'],0),pos(12,76)),(1,75)),fact('equal'('card'('plus'('plus'(identifier('Roberta',type(['Person'],1),pos(9,31)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('chef',type(['Job'],1),pos(9,52)),type(['Person'],1),pos(14,65)),type(['Person'],1),pos(12,83)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('police_officer',type(['Job'],1),pos(25,51)),type(['Person'],1),pos(30,83)),type(['Person'],1),pos(24,83)),type(['Int'],1),pos(2,83)),integer(3,pos(49,83)),type(['PrimitiveBoolean'],0),pos(47,83)),(1,82))]),assertions([fact(boolean(true,pos(12,86)),(1,86))]),commands([run('and'(['all'(['p','p_'],[field('p','one_of'(identifier('Person',type(['Person'],1),pos(14,34)),type(['Person'],1),pos(19,42)),type(['Person'],1),[disj],pos(11,42)),field('p_','one_of'(identifier('Person',type(['Person'],1),pos(14,34)),type(['Person'],1),pos(19,42)),type(['Person'],1),[disj],pos(11,42))],'no'('intersection'('join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),'join'(identifier('p_',type(['Person'],1),pos(14,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(42,42)),type(['Job'],1),pos(38,42)),type(['PrimitiveBoolean'],0),pos(28,42)),type(['PrimitiveBoolean'],0),pos(2,42)), 'one'(['p'],[field('p','one_of'(identifier('Male',type(['Male'],1),pos(14,25)),type(['Male'],1),pos(10,44)),type(['Male'],1),[],pos(6,44))],'in'(identifier('actor',type(['Job'],1),pos(42,52)),'join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),type(['PrimitiveBoolean'],0),pos(23,44)),type(['PrimitiveBoolean'],0),pos(2,44)), 'one'(['p'],[field('p','one_of'(identifier('Male',type(['Male'],1),pos(14,25)),type(['Male'],1),pos(10,44)),type(['Male'],1),[],pos(6,56))],'in'(identifier('nurse',type(['Job'],1),pos(9,51)),'join'(identifier('p',type(['Person'],1),pos(11,42)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(32,42)),type(['PrimitiveBoolean'],0),pos(23,56)),type(['PrimitiveBoolean'],0),pos(2,56)), pred_call('relation''injective',[identifier('husband',type(['Female', 'Male'],2),pos(2,21)), identifier('Male',type(['Male'],1),pos(14,25))],type(['PrimitiveBoolean'],0),pos(2,64)), 'equal'('join'('join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('chef',type(['Job'],1),pos(9,52)),type(['Person'],1),pos(14,65)),identifier('husband',type(['Female', 'Male'],2),pos(2,21)),type(['Male'],1),pos(2,65)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('telephone_operator',type(['Job'],1),pos(22,52)),type(['Person'],1),pos(26,65)),type(['PrimitiveBoolean'],0),pos(20,65)), 'not_in'(identifier('boxer',type(['Job'],1),pos(49,52)),'join'(identifier('Roberta',type(['Person'],1),pos(9,31)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(22,70)),type(['PrimitiveBoolean'],0),pos(8,70)), 'in'('join'(identifier('Pete',type(['Person'],1),pos(16,32)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(6,76)),'minus'(identifier('Job',type(['Job'],1),pos(14,49)),identifier('QualifiedJob',type(['Job'],1),pos(14,50)),type(['Job'],1),pos(19,76)),type(['PrimitiveBoolean'],0),pos(12,76)), 'equal'('card'('plus'('plus'(identifier('Roberta',type(['Person'],1),pos(9,31)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('chef',type(['Job'],1),pos(9,52)),type(['Person'],1),pos(14,65)),type(['Person'],1),pos(12,83)),'join'(identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),identifier('police_officer',type(['Job'],1),pos(25,51)),type(['Person'],1),pos(30,83)),type(['Person'],1),pos(24,83)),type(['Int'],1),pos(2,83)),integer(3,pos(49,83)),type(['PrimitiveBoolean'],0),pos(47,83))],pos(2,42)),global_scope(-1),exact_scopes([]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(0),pos(1,90))]),functions([predicate('solve',[],[],boolean(true,pos(13,89)),pos(1,89))]),signatures([signature('Female',[field('husband','lone_of'(identifier('Male',type(['Male'],1),pos(14,25)),type(['Male'],1),pos(12,21)),type(['Male'],1),[],pos(2,21))],[],[abstract, subsig('Person')],pos(14,20)),signature('Male',[],[],[abstract, subsig('Person')],pos(14,25)),signature('Roberta',[],[],[one, subsig('Female')],pos(9,31)),signature('Thelma',[],[],[one, subsig('Female')],pos(18,31)),signature('Steve',[],[],[one, subsig('Male')],pos(9,32)),signature('Pete',[],[],[one, subsig('Male')],pos(16,32)),signature('Person',[field('jobs','some_of'(identifier('Job',type(['Job'],1),pos(14,49)),type(['Job'],1),pos(9,35)),type(['Job'],1),[],pos(2,35))],['equal'('card'('join'(identifier(this,type(['Person'],1),pos(1,1)),identifier('jobs',type(['Person', 'Job'],2),pos(2,35)),type(['Job'],1),pos(3,37)),type(['Int'],1),pos(2,37)),integer(2,pos(11,37)),type(['PrimitiveBoolean'],0),pos(9,37))],[abstract],pos(14,34)),signature('Job',[],[],[abstract],pos(14,49)),signature('QualifiedJob',[],[],[abstract, subsig('Job')],pos(14,50)),signature('nurse',[],[],[one, subsig('QualifiedJob')],pos(9,51)),signature('teacher',[],[],[one, subsig('QualifiedJob')],pos(16,51)),signature('police_officer',[],[],[one, subsig('QualifiedJob')],pos(25,51)),signature('chef',[],[],[one, subsig('Job')],pos(9,52)),signature('guard',[],[],[one, subsig('Job')],pos(15,52)),signature('telephone_operator',[],[],[one, subsig('Job')],pos(22,52)),signature('actor',[],[],[one, subsig('Job')],pos(42,52)),signature('boxer',[],[],[one, subsig('Job')],pos(49,52))]),ordered_signatures([]),[sequences:false]),alloy_model('util''integer',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],cast2sigint('integer'plus'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['Int'],1),pos(35,12))),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],cast2sigint('integer'minus'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['Int'],1),pos(36,15))),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],cast2sigint('closure'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['Int'],1),pos(34,17))),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],cast2sigint('integer'div'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['Int'],1),pos(34,25))),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],cast2sigint('integer'rem'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['Int'],1),pos(34,28))),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],cast2sigint('integer'minus'(integer(0,pos(29,31)),cast2int(identifier('n',type(['Int'],1),pos(13,31))),type(['Int'],1),pos(31,31))),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'(cast2sigint(cast2int(identifier('n1',type(['Int'],1),pos(11,11)))),cast2sigint(cast2int(identifier('n2',type(['Int'],1),pos(15,11)))),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'(cast2int(identifier('n1',type(['Int'],1),pos(11,11))),cast2int(identifier('n2',type(['Int'],1),pos(15,11))),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(13,31)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),'integer'minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'(cast2int(identifier('n',type(['Int'],1),pos(13,31))),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'(['e'],[field('e','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),cast2sigint(cast2int(identifier('i',type(['Int'],1),pos(14,71)))),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(23,80))],cast2sigint('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['univ'],1),pos(4,72)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72))),pos(1,80)),function('integer''max',[],[],cast2sigint(integer(max,pos(19,85))),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field('es','set_of'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],cast2sigint(integer(min,pos(19,88))),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field('es','set_of'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],identifier('next',type(['univ', 'univ'],2),pos(22,71)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['univ'],1),pos(4,72)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['univ'],1),pos(4,72)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let('a',cast2sigint(cast2int(identifier('e1',type(['Int'],1),pos(13,109)))),let('b',cast2sigint(cast2int(identifier('e2',type(['Int'],1),pos(17,109)))),if_then_else('less'(cast2int(identifier('a',type(['Int'],1),pos(37,109))),cast2int(identifier('b',type(['Int'],1),pos(48,109))),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let('a',cast2sigint(cast2int(identifier('e1',type(['Int'],1),pos(13,109)))),let('b',cast2sigint(cast2int(identifier('e2',type(['Int'],1),pos(17,109)))),if_then_else('less'(cast2int(identifier('a',type(['Int'],1),pos(37,109))),cast2int(identifier('b',type(['Int'],1),pos(48,109))),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('a',type(['Int'],1),pos(37,109)),identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false]),alloy_model('util''relation',facts([]),assertions([]),commands([]),functions([function('relation''dom',[identifier('r',type(['univ', 'univ'],2),pos(10,14))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(10,14))],'join'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,14)),pos(1,14)),function('relation''ran',[identifier('r',type(['univ', 'univ'],2),pos(10,17))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(10,17))],'join'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ'],1),pos(46,17)),pos(1,17)),predicate('relation''total',[identifier('r',type(['univ', 'univ'],2),pos(13,20)), identifier('s',type(['univ'],1),pos(28,20))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(13,20)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(28,20))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,21))],'some'('join'(identifier('x',type(['univ'],1),pos(7,21)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ'],1),pos(20,21)),type(['PrimitiveBoolean'],0),pos(14,21)),type(['PrimitiveBoolean'],0),pos(3,21)),pos(1,20)),predicate('relation''functional',[identifier('r',type(['univ', 'univ'],2),pos(18,25)), identifier('s',type(['univ'],1),pos(33,25))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(18,25)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(33,25))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,26))],'lone'('join'(identifier('x',type(['univ'],1),pos(7,21)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ'],1),pos(20,21)),type(['PrimitiveBoolean'],0),pos(14,26)),type(['PrimitiveBoolean'],0),pos(3,26)),pos(1,25)),predicate('relation''function',[identifier('r',type(['univ', 'univ'],2),pos(16,30)), identifier('s',type(['univ'],1),pos(31,30))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(16,30)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(31,30))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,31))],'one'('join'(identifier('x',type(['univ'],1),pos(7,21)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ'],1),pos(20,21)),type(['PrimitiveBoolean'],0),pos(14,31)),type(['PrimitiveBoolean'],0),pos(3,31)),pos(1,30)),predicate('relation''surjective',[identifier('r',type(['univ', 'univ'],2),pos(18,35)), identifier('s',type(['univ'],1),pos(33,35))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(18,35)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(33,35))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,36))],'some'('join'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),identifier('x',type(['univ'],1),pos(7,21)),type(['univ'],1),pos(20,36)),type(['PrimitiveBoolean'],0),pos(14,36)),type(['PrimitiveBoolean'],0),pos(3,36)),pos(1,35)),predicate('relation''injective',[identifier('r',type(['univ', 'univ'],2),pos(17,40)), identifier('s',type(['univ'],1),pos(32,40))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(17,40)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(32,40))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,41))],'lone'('join'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),identifier('x',type(['univ'],1),pos(7,21)),type(['univ'],1),pos(20,36)),type(['PrimitiveBoolean'],0),pos(14,41)),type(['PrimitiveBoolean'],0),pos(3,41)),pos(1,40)),predicate('relation''bijective',[identifier('r',type(['univ', 'univ'],2),pos(16,45)), identifier('s',type(['univ'],1),pos(31,45))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(16,45)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(31,45))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,46))],'one'('join'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),identifier('x',type(['univ'],1),pos(7,21)),type(['univ'],1),pos(20,36)),type(['PrimitiveBoolean'],0),pos(14,46)),type(['PrimitiveBoolean'],0),pos(3,46)),pos(1,45)),predicate('relation''bijection',[identifier('r',type(['univ', 'univ'],2),pos(16,50)), identifier('d',type(['univ'],1),pos(31,50)), identifier('c',type(['univ'],1),pos(34,50))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(16,50)), field('d','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(31,50)),field('c','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(31,50))],'and'([pred_call('relation''function',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('d',type(['univ'],1),pos(31,50))],type(['PrimitiveBoolean'],0),pos(3,51)), pred_call('relation''bijective',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('c',type(['univ'],1),pos(34,50))],type(['PrimitiveBoolean'],0),pos(21,51))],pos(18,51)),pos(1,50)),predicate('relation''reflexive',[identifier('r',type(['univ', 'univ'],2),pos(17,55)), identifier('s',type(['univ'],1),pos(34,55))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(17,55)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(34,55))],'in'('dom_restr'(identifier('s',type(['univ'],1),pos(40,71)),iden(pos(51,55)),type(['univ', 'univ'],2),pos(49,55)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['PrimitiveBoolean'],0),pos(56,55)),pos(1,55)),predicate('relation''irreflexive',[identifier('r',type(['univ', 'univ'],2),pos(19,58))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(19,58))],'no'('intersection'(iden(pos(40,58)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(45,58)),type(['PrimitiveBoolean'],0),pos(37,58)),pos(1,58)),predicate('relation''symmetric',[identifier('r',type(['univ', 'univ'],2),pos(17,61))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(17,61))],'in'('inverse'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(35,61)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['PrimitiveBoolean'],0),pos(38,61)),pos(1,61)),predicate('relation''antisymmetric',[identifier('r',type(['univ', 'univ'],2),pos(21,64))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(21,64))],'in'('intersection'('inverse'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(35,61)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(42,64)),iden(pos(49,64)),type(['PrimitiveBoolean'],0),pos(46,64)),pos(1,64)),predicate('relation''transitive',[identifier('r',type(['univ', 'univ'],2),pos(18,67))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(18,67))],'in'('join'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(37,67)),identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['PrimitiveBoolean'],0),pos(40,67)),pos(1,67)),predicate('relation''acyclic',[identifier('r',type(['univ', 'univ'],2),pos(14,70)), identifier('s',type(['univ'],1),pos(29,70))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(14,70)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(29,70))],'all'(['x'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,71))],'not_in'(identifier('x',type(['univ'],1),pos(7,21)),'join'(identifier('x',type(['univ'],1),pos(7,21)),'closure1'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(22,71)),type(['univ'],1),pos(21,71)),type(['PrimitiveBoolean'],0),pos(16,71)),type(['PrimitiveBoolean'],0),pos(3,71)),pos(1,70)),predicate('relation''complete',[identifier('r',type(['univ', 'univ'],2),pos(15,75)), identifier('s',type(['univ'],1),pos(30,75))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(15,75)), field('s',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(30,75))],'all'(['x','y'],[field('x','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,76)),field('y','one_of'(identifier('s',type(['univ'],1),pos(40,71)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(7,76))],'implication'('not_equal'(identifier('x',type(['univ'],1),pos(7,21)),identifier('y',type(['univ'],1),pos(9,76)),type(['PrimitiveBoolean'],0),pos(17,76)),'in'('cartesian'(identifier('x',type(['univ'],1),pos(7,21)),identifier('y',type(['univ'],1),pos(9,76)),type(['univ', 'univ'],2),pos(25,76)),'plus'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),'inverse'(identifier('r',type(['univ', 'univ'],2),pos(10,14)),type(['univ', 'univ'],2),pos(35,61)),type(['univ', 'univ'],2),pos(35,76)),type(['PrimitiveBoolean'],0),pos(29,76)),type(['PrimitiveBoolean'],0),pos(21,76)),type(['PrimitiveBoolean'],0),pos(3,76)),pos(1,75)),predicate('relation''preorder',[identifier('r',type(['univ', 'univ'],2),pos(16,80)), identifier('s',type(['univ'],1),pos(33,80))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(16,80)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(33,80))],'and'([pred_call('relation''reflexive',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('s',type(['univ'],1),pos(40,71))],type(['PrimitiveBoolean'],0),pos(3,81)), pred_call('relation''transitive',[identifier('r',type(['univ', 'univ'],2),pos(10,14))],type(['PrimitiveBoolean'],0),pos(3,82))],pos(1,1)),pos(1,80)),predicate('relation''equivalence',[identifier('r',type(['univ', 'univ'],2),pos(19,86)), identifier('s',type(['univ'],1),pos(34,86))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(19,86)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(34,86))],'and'([pred_call('relation''preorder',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('s',type(['univ'],1),pos(40,71))],type(['PrimitiveBoolean'],0),pos(3,87)), pred_call('relation''symmetric',[identifier('r',type(['univ', 'univ'],2),pos(10,14))],type(['PrimitiveBoolean'],0),pos(3,88))],pos(1,1)),pos(1,86)),predicate('relation''partialOrder',[identifier('r',type(['univ', 'univ'],2),pos(20,92)), identifier('s',type(['univ'],1),pos(37,92))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(20,92)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(37,92))],'and'([pred_call('relation''preorder',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('s',type(['univ'],1),pos(40,71))],type(['PrimitiveBoolean'],0),pos(3,93)), pred_call('relation''antisymmetric',[identifier('r',type(['univ', 'univ'],2),pos(10,14))],type(['PrimitiveBoolean'],0),pos(3,94))],pos(1,1)),pos(1,92)),predicate('relation''totalOrder',[identifier('r',type(['univ', 'univ'],2),pos(18,98)), identifier('s',type(['univ'],1),pos(35,98))],[field('r','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(18,98)), field('s','set_of'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(35,98))],'and'([pred_call('relation''partialOrder',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('s',type(['univ'],1),pos(40,71))],type(['PrimitiveBoolean'],0),pos(3,99)), pred_call('relation''complete',[identifier('r',type(['univ', 'univ'],2),pos(10,14)), identifier('s',type(['univ'],1),pos(40,71))],type(['PrimitiveBoolean'],0),pos(3,100))],pos(1,1)),pos(1,98))]),signatures([]),ordered_signatures([]),[sequences:false])]).