alloy_model(facts([]),assertions([]),commands([run(and([in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[fun_call('states''first',[],type([['State']],1),pos(12,98)), fun_call('stakes''first',[],type([['Stake']],1),pos(38,98))],type([['Disc']],1),pos(25,98)),type([untyped],0),pos(9,98)), some(['finalState'],[field('finalState',oneof(identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),pos(21,99)),type([['State']],1),[],pos(9,99))],in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[identifier('finalState',type([['State']],1),pos(9,99)), fun_call('stakes''last',[],type([['Stake']],1),pos(61,99))],type([['Disc']],1),pos(48,99)),type([untyped],0),pos(34,99)),type([untyped],0),pos(4,99)), all(['preState'],[field('preState',oneof(minus(identifier('State',type([['State']],1),pos(5,52)),fun_call('states''last',[],type([['State']],1),pos(26,102)),type([['State']],1),pos(24,102)),type([['State']],1),pos(18,102)),type([['State']],1),[],pos(8,102))],let('postState',join(identifier('preState',type([['State']],1),pos(8,102)),fun_call('states''next',[],type([['State', 'State']],2),pos(24,103)),type([['State']],1),pos(24,103)),some(['fromStake'],[field('fromStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(27,104)),type([['Stake']],1),[],pos(16,104))],and([some(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,102)), identifier('fromStake',type([['Stake']],1),pos(16,104))],type([['Disc']],1),pos(28,107)),type([untyped],0),pos(14,107)), some(['toStake'],[field('toStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(28,109)),type([['Stake']],1),[],pos(19,109))],pred_call('Move',[identifier('preState',type([['State']],1),pos(8,102)), identifier('fromStake',type([['Stake']],1),pos(16,104)), identifier('toStake',type([['Stake']],1),pos(19,109)), identifier('postState',type([['State']],1),pos(12,103))],type([untyped],0),pos(45,109)),type([untyped],0),pos(14,109))],pos(1,1)),type([untyped],0),pos(11,104)),type([untyped],0),pos(22,103)),type([untyped],0),pos(4,102))],pos(12,95)),global_scope(1),exact_scopes([]),bitwidth(-1),pos(1,139)),run(and([in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[fun_call('states''first',[],type([['State']],1),pos(12,116)), fun_call('stakes''first',[],type([['Stake']],1),pos(38,116))],type([['Disc']],1),pos(25,116)),type([untyped],0),pos(9,116)), some(['finalState'],[field('finalState',oneof(identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),pos(21,117)),type([['State']],1),[],pos(9,117))],in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[identifier('finalState',type([['State']],1),pos(9,117)), fun_call('stakes''last',[],type([['Stake']],1),pos(61,117))],type([['Disc']],1),pos(48,117)),type([untyped],0),pos(34,117)),type([untyped],0),pos(4,117)), all(['preState'],[field('preState',oneof(minus(identifier('State',type([['State']],1),pos(5,52)),fun_call('states''last',[],type([['State']],1),pos(26,120)),type([['State']],1),pos(24,120)),type([['State']],1),pos(18,120)),type([['State']],1),[],pos(8,120))],let('postState',join(identifier('preState',type([['State']],1),pos(8,120)),fun_call('states''next',[],type([['State', 'State']],2),pos(24,121)),type([['State']],1),pos(24,121)),some(['fromStake'],[field('fromStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(27,122)),type([['Stake']],1),[],pos(16,122))],let('d',fun_call('topDisc',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(31,123)),and([some(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(30,126)),type([untyped],0),pos(16,126)), equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(26,127)),minus(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(61,127)),identifier('d',type([['Disc']],1),pos(18,123)),type([['Disc']],1),pos(85,127)),type([untyped],0),pos(50,127)), some(['toStake'],[field('toStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(30,128)),type([['Stake']],1),[],pos(21,128))],and([in(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(27,130)),fun_call('discs''nexts',[identifier('d',type([['Disc']],1),pos(18,123))],type([['Disc']],1),pos(52,130)),type([untyped],0),pos(49,130)), equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(28,131)),plus(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(61,131)),identifier('d',type([['Disc']],1),pos(18,123)),type([['Disc']],1),pos(83,131)),type([untyped],0),pos(50,131)), let('otherStake',minus(minus(identifier('Stake',type([['Stake']],1),pos(5,45)),identifier('fromStake',type([['Stake']],1),pos(16,122)),type([['Stake']],1),pos(40,133)),identifier('toStake',type([['Stake']],1),pos(21,128)),type([['Stake']],1),pos(52,133)),equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('otherStake',type([['Stake']],1),pos(21,133))],type([['Disc']],1),pos(31,134)),fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('otherStake',type([['Stake']],1),pos(21,133))],type([['Disc']],1),pos(67,134)),type([untyped],0),pos(56,134)),type([untyped],0),pos(32,133))],pos(1,1)),type([untyped],0),pos(16,128))],pos(1,1)),type([untyped],0),pos(20,123)),type([untyped],0),pos(11,122)),type([untyped],0),pos(22,121)),type([untyped],0),pos(4,120))],pos(13,113)),global_scope(1),exact_scopes([]),bitwidth(-1),pos(1,140))]),functions([function('discsOnStake',[identifier(this,type([['State']],1),pos(5,61)), identifier('stake',type([['Stake']],1),pos(24,61))],[field(this,identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),[],pos(5,61)), field('stake',identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),[],pos(24,61))],join(identifier('stake',type([['Stake']],1),pos(24,61)),inverse(join(identifier(this,type([['State']],1),pos(5,61)),identifier('on',type([['State', 'Disc', 'Stake']],3),pos(3,53)),type([['Disc', 'Stake']],2),pos(15,64)),type([['Stake', 'Disc']],2),pos(9,64)),type([['Disc']],1),pos(8,64)),pos(1,61)),function('topDisc',[identifier(this,type([['State']],1),pos(5,67)), identifier('stake',type([['Stake']],1),pos(19,67))],[field(this,identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),[],pos(5,67)), field('stake',identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),[],pos(19,67))],comprehension(['d'],[field('d',oneof(fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(5,67)), identifier('stake',type([['Stake']],1),pos(19,67))],type([['Disc']],1),pos(13,70)),type([['Disc']],1),pos(8,70)),type([['Disc']],1),[],pos(5,70))],in(fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(5,67)), identifier('stake',type([['Stake']],1),pos(19,67))],type([['Disc']],1),pos(40,70)),plus(fun_call('discs''nexts',[identifier('d',type([['Disc']],1),pos(5,70))],type([['Disc']],1),pos(63,70)),identifier('d',type([['Disc']],1),pos(5,70)),type([['Disc']],1),pos(78,70)),type([untyped],0),pos(60,70)),type([['Disc']],1),pos(3,70)),pos(1,67)),predicate('Move',[identifier(this,type([['State']],1),pos(6,73)), identifier('fromStake',type([['Stake']],1),pos(18,73)), identifier('toStake',type([['Stake']],1),pos(29,73)), identifier('s_',type([['State']],1),pos(45,73))],[field(this,identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),[],pos(6,73)), field('fromStake',identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),[],pos(18,73)),field('toStake',identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),[],pos(18,73)), field('s_',identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),[],pos(45,73))],let('d',fun_call('topDisc',[identifier(this,type([['State']],1),pos(6,73)), identifier('fromStake',type([['Stake']],1),pos(18,73))],type([['Disc']],1),pos(17,81)),and([in(fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(6,73)), identifier('toStake',type([['Stake']],1),pos(29,73))],type([['Disc']],1),pos(12,84)),fun_call('discs''nexts',[identifier('d',type([['Disc']],1),pos(8,81))],type([['Disc']],1),pos(37,84)),type([untyped],0),pos(34,84)), equal(fun_call('discsOnStake',[identifier('s_',type([['State']],1),pos(45,73)), identifier('fromStake',type([['Stake']],1),pos(18,73))],type([['Disc']],1),pos(10,86)),minus(fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(6,73)), identifier('fromStake',type([['Stake']],1),pos(18,73))],type([['Disc']],1),pos(41,86)),identifier('d',type([['Disc']],1),pos(8,81)),type([['Disc']],1),pos(65,86)),type([untyped],0),pos(34,86)), equal(fun_call('discsOnStake',[identifier('s_',type([['State']],1),pos(45,73)), identifier('toStake',type([['Stake']],1),pos(29,73))],type([['Disc']],1),pos(10,88)),plus(fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(6,73)), identifier('toStake',type([['Stake']],1),pos(29,73))],type([['Disc']],1),pos(39,88)),identifier('d',type([['Disc']],1),pos(8,81)),type([['Disc']],1),pos(61,88)),type([untyped],0),pos(32,88)), let('otherStake',minus(minus(identifier('Stake',type([['Stake']],1),pos(5,45)),identifier('fromStake',type([['Stake']],1),pos(18,73)),type([['Stake']],1),pos(30,90)),identifier('toStake',type([['Stake']],1),pos(29,73)),type([['Stake']],1),pos(42,90)),equal(fun_call('discsOnStake',[identifier('s_',type([['State']],1),pos(45,73)), identifier('otherStake',type([['Stake']],1),pos(11,90))],type([['Disc']],1),pos(12,91)),fun_call('discsOnStake',[identifier(this,type([['State']],1),pos(6,73)), identifier('otherStake',type([['Stake']],1),pos(11,90))],type([['Disc']],1),pos(44,91)),type([untyped],0),pos(37,91)),type([untyped],0),pos(22,90))],pos(1,1)),type([untyped],0),pos(10,81)),pos(1,73)),predicate('Game1',[],[],and([in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[fun_call('states''first',[],type([['State']],1),pos(12,98)), fun_call('stakes''first',[],type([['Stake']],1),pos(38,98))],type([['Disc']],1),pos(25,98)),type([untyped],0),pos(9,98)), some(['finalState'],[field('finalState',oneof(identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),pos(21,99)),type([['State']],1),[],pos(9,99))],in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[identifier('finalState',type([['State']],1),pos(9,99)), fun_call('stakes''last',[],type([['Stake']],1),pos(61,99))],type([['Disc']],1),pos(48,99)),type([untyped],0),pos(34,99)),type([untyped],0),pos(4,99)), all(['preState'],[field('preState',oneof(minus(identifier('State',type([['State']],1),pos(5,52)),fun_call('states''last',[],type([['State']],1),pos(26,102)),type([['State']],1),pos(24,102)),type([['State']],1),pos(18,102)),type([['State']],1),[],pos(8,102))],let('postState',join(identifier('preState',type([['State']],1),pos(8,102)),fun_call('states''next',[],type([['State', 'State']],2),pos(24,103)),type([['State']],1),pos(24,103)),some(['fromStake'],[field('fromStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(27,104)),type([['Stake']],1),[],pos(16,104))],and([some(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,102)), identifier('fromStake',type([['Stake']],1),pos(16,104))],type([['Disc']],1),pos(28,107)),type([untyped],0),pos(14,107)), some(['toStake'],[field('toStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(28,109)),type([['Stake']],1),[],pos(19,109))],pred_call('Move',[identifier('preState',type([['State']],1),pos(8,102)), identifier('fromStake',type([['Stake']],1),pos(16,104)), identifier('toStake',type([['Stake']],1),pos(19,109)), identifier('postState',type([['State']],1),pos(12,103))],type([untyped],0),pos(45,109)),type([untyped],0),pos(14,109))],pos(1,1)),type([untyped],0),pos(11,104)),type([untyped],0),pos(22,103)),type([untyped],0),pos(4,102))],pos(1,1)),pos(1,95)),predicate('Game2',[],[],and([in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[fun_call('states''first',[],type([['State']],1),pos(12,116)), fun_call('stakes''first',[],type([['Stake']],1),pos(38,116))],type([['Disc']],1),pos(25,116)),type([untyped],0),pos(9,116)), some(['finalState'],[field('finalState',oneof(identifier('State',type([['State']],1),pos(5,52)),type([['State']],1),pos(21,117)),type([['State']],1),[],pos(9,117))],in(identifier('Disc',type([['Disc']],1),pos(5,47)),fun_call('discsOnStake',[identifier('finalState',type([['State']],1),pos(9,117)), fun_call('stakes''last',[],type([['Stake']],1),pos(61,117))],type([['Disc']],1),pos(48,117)),type([untyped],0),pos(34,117)),type([untyped],0),pos(4,117)), all(['preState'],[field('preState',oneof(minus(identifier('State',type([['State']],1),pos(5,52)),fun_call('states''last',[],type([['State']],1),pos(26,120)),type([['State']],1),pos(24,120)),type([['State']],1),pos(18,120)),type([['State']],1),[],pos(8,120))],let('postState',join(identifier('preState',type([['State']],1),pos(8,120)),fun_call('states''next',[],type([['State', 'State']],2),pos(24,121)),type([['State']],1),pos(24,121)),some(['fromStake'],[field('fromStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(27,122)),type([['Stake']],1),[],pos(16,122))],let('d',fun_call('topDisc',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(31,123)),and([some(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(30,126)),type([untyped],0),pos(16,126)), equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(26,127)),minus(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('fromStake',type([['Stake']],1),pos(16,122))],type([['Disc']],1),pos(61,127)),identifier('d',type([['Disc']],1),pos(18,123)),type([['Disc']],1),pos(85,127)),type([untyped],0),pos(50,127)), some(['toStake'],[field('toStake',oneof(identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Stake']],1),pos(30,128)),type([['Stake']],1),[],pos(21,128))],and([in(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(27,130)),fun_call('discs''nexts',[identifier('d',type([['Disc']],1),pos(18,123))],type([['Disc']],1),pos(52,130)),type([untyped],0),pos(49,130)), equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(28,131)),plus(fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('toStake',type([['Stake']],1),pos(21,128))],type([['Disc']],1),pos(61,131)),identifier('d',type([['Disc']],1),pos(18,123)),type([['Disc']],1),pos(83,131)),type([untyped],0),pos(50,131)), let('otherStake',minus(minus(identifier('Stake',type([['Stake']],1),pos(5,45)),identifier('fromStake',type([['Stake']],1),pos(16,122)),type([['Stake']],1),pos(40,133)),identifier('toStake',type([['Stake']],1),pos(21,128)),type([['Stake']],1),pos(52,133)),equal(fun_call('discsOnStake',[identifier('postState',type([['State']],1),pos(12,121)), identifier('otherStake',type([['Stake']],1),pos(21,133))],type([['Disc']],1),pos(31,134)),fun_call('discsOnStake',[identifier('preState',type([['State']],1),pos(8,120)), identifier('otherStake',type([['Stake']],1),pos(21,133))],type([['Disc']],1),pos(67,134)),type([untyped],0),pos(56,134)),type([untyped],0),pos(32,133))],pos(1,1)),type([untyped],0),pos(16,128))],pos(1,1)),type([untyped],0),pos(20,123)),type([untyped],0),pos(11,122)),type([untyped],0),pos(22,121)),type([untyped],0),pos(4,120))],pos(1,1)),pos(1,113))]),signatures([signature('Stake',[],[],[ordered],pos(5,45)),signature('Disc',[],[],[ordered],pos(5,47)),signature('State',[field('on',total_function(identifier('Disc',type([['Disc']],1),pos(5,47)),identifier('Stake',type([['Stake']],1),pos(5,45)),type([['Disc', 'Stake']],2),pos(12,53)),type([['Disc', 'Stake']],2),[],pos(3,53))],[],[ordered],pos(5,52))])).