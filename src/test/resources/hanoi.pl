alloy_model(facts([]),assertions([]),commands([run(and([in('this'/'Disc',fun_call('this'/'discsOnStake',[fun_call('states'/'first',[],type(['this'/'State']),pos(12,98), fun_call('stakes'/'first',[],type(['this'/'Stake']),pos(38,98)],type(['this'/'Disc']),pos(25,98),type([]),pos(9,98)), some([field('finalState',oneof('this'/'State',type(['this'/'State']),pos(21,99)),pos(9,99))],type([]),in('this'/'Disc',fun_call('this'/'discsOnStake',['finalState', fun_call('stakes'/'last',[],type(['this'/'Stake']),pos(61,99)],type(['this'/'Disc']),pos(48,99),type([]),pos(34,99)),pos(4,99), all([field('preState',oneof(minus('this'/'State',fun_call('states'/'last',[],type(['this'/'State']),pos(26,102),type(['this'/'State']),pos(24,102)),type(['this'/'State']),pos(18,102)),pos(8,102))],type([]),let(join('preState',fun_call('states'/'next',[],type(['this'/'State->this'/'State']),pos(24,103),type(['this'/'State']),pos(24,103)),some([field('fromStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(27,104)),pos(16,104))],type([]),and([some(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(28,107),type([]),pos(14,107)), some([field('toStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(28,109)),pos(19,109))],type([]),pred_call('this'/'Move',['preState', 'fromStake', 'toStake', 'postState'],type([]),pos(45,109),pos(14,109)],pos(1,1)),pos(11,104),type([]),pos(22,103),pos(4,102)],pos(12,95)),global_scope(1),exact_scopes(['this'/'State', 'this'/'Stake', 'this'/'Disc']),bitwidth(-1),pos(1,139),run(and([in('this'/'Disc',fun_call('this'/'discsOnStake',[fun_call('states'/'first',[],type(['this'/'State']),pos(12,116), fun_call('stakes'/'first',[],type(['this'/'Stake']),pos(38,116)],type(['this'/'Disc']),pos(25,116),type([]),pos(9,116)), some([field('finalState',oneof('this'/'State',type(['this'/'State']),pos(21,117)),pos(9,117))],type([]),in('this'/'Disc',fun_call('this'/'discsOnStake',['finalState', fun_call('stakes'/'last',[],type(['this'/'Stake']),pos(61,117)],type(['this'/'Disc']),pos(48,117),type([]),pos(34,117)),pos(4,117), all([field('preState',oneof(minus('this'/'State',fun_call('states'/'last',[],type(['this'/'State']),pos(26,120),type(['this'/'State']),pos(24,120)),type(['this'/'State']),pos(18,120)),pos(8,120))],type([]),let(join('preState',fun_call('states'/'next',[],type(['this'/'State->this'/'State']),pos(24,121),type(['this'/'State']),pos(24,121)),some([field('fromStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(27,122)),pos(16,122))],type([]),let(fun_call('this'/'topDisc',['preState', 'fromStake'],type(['this'/'Disc']),pos(31,123),and([some(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(30,126),type([]),pos(16,126)), equal(fun_call('this'/'discsOnStake',['postState', 'fromStake'],type(['this'/'Disc']),pos(26,127),minus(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(61,127),'d',type(['this'/'Disc']),pos(85,127)),type([]),pos(50,127)), some([field('toStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(30,128)),pos(21,128))],type([]),and([in(fun_call('this'/'discsOnStake',['preState', 'toStake'],type(['this'/'Disc']),pos(27,130),fun_call('discs'/'nexts',['d'],type(['this'/'Disc']),pos(52,130),type([]),pos(49,130)), equal(fun_call('this'/'discsOnStake',['postState', 'toStake'],type(['this'/'Disc']),pos(28,131),plus(fun_call('this'/'discsOnStake',['preState', 'toStake'],type(['this'/'Disc']),pos(61,131),'d',type(['this'/'Disc']),pos(83,131)),type([]),pos(50,131)), let(minus(minus('this'/'Stake','fromStake',type(['this'/'Stake']),pos(40,133)),'toStake',type(['this'/'Stake']),pos(52,133)),equal(fun_call('this'/'discsOnStake',['postState', 'otherStake'],type(['this'/'Disc']),pos(31,134),fun_call('this'/'discsOnStake',['preState', 'otherStake'],type(['this'/'Disc']),pos(67,134),type([]),pos(56,134)),type([]),pos(32,133)],pos(1,1)),pos(16,128)],pos(1,1)),type([]),pos(20,123),pos(11,122),type([]),pos(22,121),pos(4,120)],pos(13,113)),global_scope(1),exact_scopes(['this'/'State', 'this'/'Stake', 'this'/'Disc']),bitwidth(-1),pos(1,140)]),functions([function('this'/'discsOnStake',['this', 'stake'],[field('this','this'/'State',pos(5,61)), field('stake','this'/'Stake',pos(24,61))],join('stake',inverse(join('this','on',type(['this'/'Disc->this'/'Stake']),pos(15,64)),type(['this'/'Stake->this'/'Disc']),pos(9,64)),type(['this'/'Disc']),pos(8,64)),pos(1,61)),function('this'/'topDisc',['this', 'stake'],[field('this','this'/'State',pos(5,67)), field('stake','this'/'Stake',pos(19,67))],comprehension([field('d',oneof(fun_call('this'/'discsOnStake',['this', 'stake'],type(['this'/'Disc']),pos(13,70),type(['this'/'Disc']),pos(8,70)),pos(5,70))],type(['this'/'Disc']),in(fun_call('this'/'discsOnStake',['this', 'stake'],type(['this'/'Disc']),pos(40,70),plus(fun_call('discs'/'nexts',['d'],type(['this'/'Disc']),pos(63,70),'d',type(['this'/'Disc']),pos(78,70)),type([]),pos(60,70)),pos(3,70),pos(1,67)),predicate('this'/'Move',['this', 'fromStake', 'toStake', 's_'],[field('this','this'/'State',pos(6,73)), field('fromStake','this'/'Stake',pos(18,73)), field('s_','this'/'State',pos(45,73))],let(fun_call('this'/'topDisc',['this', 'fromStake'],type(['this'/'Disc']),pos(17,81),and([in(fun_call('this'/'discsOnStake',['this', 'toStake'],type(['this'/'Disc']),pos(12,84),fun_call('discs'/'nexts',['d'],type(['this'/'Disc']),pos(37,84),type([]),pos(34,84)), equal(fun_call('this'/'discsOnStake',['s_', 'fromStake'],type(['this'/'Disc']),pos(10,86),minus(fun_call('this'/'discsOnStake',['this', 'fromStake'],type(['this'/'Disc']),pos(41,86),'d',type(['this'/'Disc']),pos(65,86)),type([]),pos(34,86)), equal(fun_call('this'/'discsOnStake',['s_', 'toStake'],type(['this'/'Disc']),pos(10,88),plus(fun_call('this'/'discsOnStake',['this', 'toStake'],type(['this'/'Disc']),pos(39,88),'d',type(['this'/'Disc']),pos(61,88)),type([]),pos(32,88)), let(minus(minus('this'/'Stake','fromStake',type(['this'/'Stake']),pos(30,90)),'toStake',type(['this'/'Stake']),pos(42,90)),equal(fun_call('this'/'discsOnStake',['s_', 'otherStake'],type(['this'/'Disc']),pos(12,91),fun_call('this'/'discsOnStake',['this', 'otherStake'],type(['this'/'Disc']),pos(44,91),type([]),pos(37,91)),type([]),pos(22,90)],pos(1,1)),type([]),pos(10,81),pos(1,73)),predicate('this'/'Game1',[],[],and([in('this'/'Disc',fun_call('this'/'discsOnStake',[fun_call('states'/'first',[],type(['this'/'State']),pos(12,98), fun_call('stakes'/'first',[],type(['this'/'Stake']),pos(38,98)],type(['this'/'Disc']),pos(25,98),type([]),pos(9,98)), some([field('finalState',oneof('this'/'State',type(['this'/'State']),pos(21,99)),pos(9,99))],type([]),in('this'/'Disc',fun_call('this'/'discsOnStake',['finalState', fun_call('stakes'/'last',[],type(['this'/'Stake']),pos(61,99)],type(['this'/'Disc']),pos(48,99),type([]),pos(34,99)),pos(4,99), all([field('preState',oneof(minus('this'/'State',fun_call('states'/'last',[],type(['this'/'State']),pos(26,102),type(['this'/'State']),pos(24,102)),type(['this'/'State']),pos(18,102)),pos(8,102))],type([]),let(join('preState',fun_call('states'/'next',[],type(['this'/'State->this'/'State']),pos(24,103),type(['this'/'State']),pos(24,103)),some([field('fromStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(27,104)),pos(16,104))],type([]),and([some(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(28,107),type([]),pos(14,107)), some([field('toStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(28,109)),pos(19,109))],type([]),pred_call('this'/'Move',['preState', 'fromStake', 'toStake', 'postState'],type([]),pos(45,109),pos(14,109)],pos(1,1)),pos(11,104),type([]),pos(22,103),pos(4,102)],pos(1,1)),pos(1,95)),predicate('this'/'Game2',[],[],and([in('this'/'Disc',fun_call('this'/'discsOnStake',[fun_call('states'/'first',[],type(['this'/'State']),pos(12,116), fun_call('stakes'/'first',[],type(['this'/'Stake']),pos(38,116)],type(['this'/'Disc']),pos(25,116),type([]),pos(9,116)), some([field('finalState',oneof('this'/'State',type(['this'/'State']),pos(21,117)),pos(9,117))],type([]),in('this'/'Disc',fun_call('this'/'discsOnStake',['finalState', fun_call('stakes'/'last',[],type(['this'/'Stake']),pos(61,117)],type(['this'/'Disc']),pos(48,117),type([]),pos(34,117)),pos(4,117), all([field('preState',oneof(minus('this'/'State',fun_call('states'/'last',[],type(['this'/'State']),pos(26,120),type(['this'/'State']),pos(24,120)),type(['this'/'State']),pos(18,120)),pos(8,120))],type([]),let(join('preState',fun_call('states'/'next',[],type(['this'/'State->this'/'State']),pos(24,121),type(['this'/'State']),pos(24,121)),some([field('fromStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(27,122)),pos(16,122))],type([]),let(fun_call('this'/'topDisc',['preState', 'fromStake'],type(['this'/'Disc']),pos(31,123),and([some(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(30,126),type([]),pos(16,126)), equal(fun_call('this'/'discsOnStake',['postState', 'fromStake'],type(['this'/'Disc']),pos(26,127),minus(fun_call('this'/'discsOnStake',['preState', 'fromStake'],type(['this'/'Disc']),pos(61,127),'d',type(['this'/'Disc']),pos(85,127)),type([]),pos(50,127)), some([field('toStake',oneof('this'/'Stake',type(['this'/'Stake']),pos(30,128)),pos(21,128))],type([]),and([in(fun_call('this'/'discsOnStake',['preState', 'toStake'],type(['this'/'Disc']),pos(27,130),fun_call('discs'/'nexts',['d'],type(['this'/'Disc']),pos(52,130),type([]),pos(49,130)), equal(fun_call('this'/'discsOnStake',['postState', 'toStake'],type(['this'/'Disc']),pos(28,131),plus(fun_call('this'/'discsOnStake',['preState', 'toStake'],type(['this'/'Disc']),pos(61,131),'d',type(['this'/'Disc']),pos(83,131)),type([]),pos(50,131)), let(minus(minus('this'/'Stake','fromStake',type(['this'/'Stake']),pos(40,133)),'toStake',type(['this'/'Stake']),pos(52,133)),equal(fun_call('this'/'discsOnStake',['postState', 'otherStake'],type(['this'/'Disc']),pos(31,134),fun_call('this'/'discsOnStake',['preState', 'otherStake'],type(['this'/'Disc']),pos(67,134),type([]),pos(56,134)),type([]),pos(32,133)],pos(1,1)),pos(16,128)],pos(1,1)),type([]),pos(20,123),pos(11,122),type([]),pos(22,121),pos(4,120)],pos(1,1)),pos(1,113))]),signatures([signature('this'/'Stake',[],[],[],pos(5,45)),signature('this'/'Disc',[],[],[],pos(5,47)),signature('this'/'State',[field('on',total_function('this'/'Disc','this'/'Stake',type(['this'/'Disc->this'/'Stake']),pos(12,53)),pos(3,53))],[],[],pos(5,52))])).