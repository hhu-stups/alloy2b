alloy('unknown',[alloy_model('unknown',facts([fact('and'(['in'('plus'('join'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(8,14)),'join'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(24,14)),type(['EndPoint'],1),pos(14,14)),identifier('Client',type(['EndPoint'],1),pos(5,4)),type(['PrimitiveBoolean'],0),pos(28,14)), 'in'('plus'('join'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(45,14)),'join'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(59,14)),type(['EndPoint'],1),pos(49,14)),identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['PrimitiveBoolean'],0),pos(65,14))],pos(1,1)),(1,13)),fact('and'(['all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,16))],'one'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),type(['PrimitiveBoolean'],0),pos(19,16)),type(['PrimitiveBoolean'],0),pos(1,16)), 'all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(38,16))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(18,17)),type(['PrimitiveBoolean'],0),pos(6,17)), 'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(24,18)),type(['PrimitiveBoolean'],0),pos(12,18))],pos(1,18)),type(['PrimitiveBoolean'],0),pos(34,16)), 'all'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(32,18))],'not_in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'closure1'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent', 'HTTPEvent'],2),pos(22,19)),type(['HTTPEvent', 'HTTPEvent'],2),pos(12,19)),type(['HTTPEvent'],1),pos(11,19)),type(['PrimitiveBoolean'],0),pos(3,19)),type(['PrimitiveBoolean'],0),pos(28,18))],pos(1,1)),(1,15)),fact('all'(['e', 's'],[field('e','oneof'(identifier('HTTPEvent',type(['HTTPEvent'],1),pos(46,4)),type(['HTTPEvent'],1),pos(8,21)),type(['HTTPEvent'],1),[],pos(5,21)), field('s','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(19,21))],'or'(['iff'('in'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'join'(identifier('s',type(['EndPoint'],1),pos(19,21)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(7,22)),type(['PrimitiveBoolean'],0),pos(3,22)),'equal'('join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(20,22)),identifier('s',type(['EndPoint'],1),pos(19,21)),type(['PrimitiveBoolean'],0),pos(26,22)),type(['PrimitiveBoolean'],0),pos(15,22)), 'some'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(38,22))],'and'(['in'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent'],1),pos(7,23)),type(['PrimitiveBoolean'],0),pos(3,23)), 'in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('s',type(['EndPoint'],1),pos(19,21)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(7,22)),type(['PrimitiveBoolean'],0),pos(21,23))],pos(15,23)),type(['PrimitiveBoolean'],0),pos(33,22))],pos(30,22)),type(['PrimitiveBoolean'],0),pos(1,21)),(1,20)),fact('and'(['all'(['r', 'e'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,25)), field('e','oneof'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent'],1),pos(7,23)),type(['HTTPEvent'],1),pos(21,25)),type(['HTTPEvent'],1),[],pos(18,25))],'equal'('join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),type(['PrimitiveBoolean'],0),pos(10,26)),type(['PrimitiveBoolean'],0),pos(1,25)), 'all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,27))],'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),if_then_else('in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('Redirect',type(['HTTPEvent'],1),pos(5,10)),type(['PrimitiveBoolean'],0),pos(4,28)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(34,28)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),type(['EndPoint'],1),pos(16,28)),type(['PrimitiveBoolean'],0),pos(28,27)),type(['PrimitiveBoolean'],0),pos(1,27)), 'all'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(5,29))],'implication'('no'('join'(identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(10,30)),type(['PrimitiveBoolean'],0),pos(1,30)),'in'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),type(['PrimitiveBoolean'],0),pos(10,31)),type(['PrimitiveBoolean'],0),pos(13,30)),type(['PrimitiveBoolean'],0),pos(1,29))],pos(1,1)),(1,24))]),assertions([fact('no'(['good','bad'],[field('good','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(4,38)),field('bad','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(4,38))],'and'([pred_call('ObeysOrigins',[identifier('good',type(['EndPoint'],1),pos(4,38))],type(['PrimitiveBoolean'],0),pos(6,39)), 'no'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(22,39))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),identifier('bad',type(['EndPoint'],1),pos(10,38)),type(['PrimitiveBoolean'],0),pos(6,40)), 'in'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),identifier('Client',type(['EndPoint'],1),pos(5,4)),type(['PrimitiveBoolean'],0),pos(25,40))],pos(12,40)),type(['PrimitiveBoolean'],0),pos(19,39)), 'some'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(40,40))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),identifier('good',type(['EndPoint'],1),pos(4,38)),type(['PrimitiveBoolean'],0),pos(6,41)), 'in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('bad',type(['EndPoint'],1),pos(10,38)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(25,41)),type(['PrimitiveBoolean'],0),pos(19,41))],pos(13,41)),type(['PrimitiveBoolean'],0),pos(35,40))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(1,38)),(1,37))]),commands([check('and'(['in'('plus'('join'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(8,14)),'join'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(24,14)),type(['EndPoint'],1),pos(14,14)),identifier('Client',type(['EndPoint'],1),pos(5,4)),type(['PrimitiveBoolean'],0),pos(28,14)), 'in'('plus'('join'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(45,14)),'join'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(59,14)),type(['EndPoint'],1),pos(49,14)),identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['PrimitiveBoolean'],0),pos(65,14)), 'all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,16))],'one'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),type(['PrimitiveBoolean'],0),pos(19,16)),type(['PrimitiveBoolean'],0),pos(1,16)), 'all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(38,16))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(18,17)),type(['PrimitiveBoolean'],0),pos(6,17)), 'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(24,18)),type(['PrimitiveBoolean'],0),pos(12,18))],pos(1,18)),type(['PrimitiveBoolean'],0),pos(34,16)), 'all'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(32,18))],'not_in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'closure1'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent', 'HTTPEvent'],2),pos(22,19)),type(['HTTPEvent', 'HTTPEvent'],2),pos(12,19)),type(['HTTPEvent'],1),pos(11,19)),type(['PrimitiveBoolean'],0),pos(3,19)),type(['PrimitiveBoolean'],0),pos(28,18)), 'all'(['e', 's'],[field('e','oneof'(identifier('HTTPEvent',type(['HTTPEvent'],1),pos(46,4)),type(['HTTPEvent'],1),pos(8,21)),type(['HTTPEvent'],1),[],pos(5,21)), field('s','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(19,21))],'or'(['iff'('in'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'join'(identifier('s',type(['EndPoint'],1),pos(19,21)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(7,22)),type(['PrimitiveBoolean'],0),pos(3,22)),'equal'('join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(20,22)),identifier('s',type(['EndPoint'],1),pos(19,21)),type(['PrimitiveBoolean'],0),pos(26,22)),type(['PrimitiveBoolean'],0),pos(15,22)), 'some'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(38,22))],'and'(['in'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent'],1),pos(7,23)),type(['PrimitiveBoolean'],0),pos(3,23)), 'in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('s',type(['EndPoint'],1),pos(19,21)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(7,22)),type(['PrimitiveBoolean'],0),pos(21,23))],pos(15,23)),type(['PrimitiveBoolean'],0),pos(33,22))],pos(30,22)),type(['PrimitiveBoolean'],0),pos(1,21)), 'all'(['r', 'e'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,25)), field('e','oneof'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),type(['HTTPEvent'],1),pos(7,23)),type(['HTTPEvent'],1),pos(21,25)),type(['HTTPEvent'],1),[],pos(18,25))],'equal'('join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),type(['PrimitiveBoolean'],0),pos(10,26)),type(['PrimitiveBoolean'],0),pos(1,25)), 'all'(['r'],[field('r','oneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(8,16)),type(['HTTPEvent'],1),[],pos(5,27))],'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),if_then_else('in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('Redirect',type(['HTTPEvent'],1),pos(5,10)),type(['PrimitiveBoolean'],0),pos(4,28)),'join'('join'(identifier('response',type(['HTTPEvent', 'HTTPEvent'],2),pos(33,6)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(31,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(34,28)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),type(['EndPoint'],1),pos(16,28)),type(['PrimitiveBoolean'],0),pos(28,27)),type(['PrimitiveBoolean'],0),pos(1,27)), 'all'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(5,29))],'implication'('no'('join'(identifier('embeds',type(['HTTPEvent', 'HTTPEvent'],2),pos(34,8)),identifier('r',type(['HTTPEvent'],1),pos(5,16)),type(['HTTPEvent'],1),pos(10,30)),type(['PrimitiveBoolean'],0),pos(1,30)),'in'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),type(['PrimitiveBoolean'],0),pos(10,31)),type(['PrimitiveBoolean'],0),pos(13,30)),type(['PrimitiveBoolean'],0),pos(1,29)), 'not'('no'(['good','bad'],[field('good','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(4,38)),field('bad','oneof'(identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),pos(22,21)),type(['EndPoint'],1),[],pos(4,38))],'and'([pred_call('ObeysOrigins',[identifier('good',type(['EndPoint'],1),pos(4,38))],type(['PrimitiveBoolean'],0),pos(6,39)), 'no'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(22,39))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),identifier('bad',type(['EndPoint'],1),pos(10,38)),type(['PrimitiveBoolean'],0),pos(6,40)), 'in'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),identifier('Client',type(['EndPoint'],1),pos(5,4)),type(['PrimitiveBoolean'],0),pos(25,40))],pos(12,40)),type(['PrimitiveBoolean'],0),pos(19,39)), 'some'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(40,40))],'and'(['equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),identifier('good',type(['EndPoint'],1),pos(4,38)),type(['PrimitiveBoolean'],0),pos(6,41)), 'in'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),'join'(identifier('bad',type(['EndPoint'],1),pos(10,38)),identifier('causes',type(['EndPoint', 'HTTPEvent'],2),pos(1,3)),type(['HTTPEvent'],1),pos(25,41)),type(['PrimitiveBoolean'],0),pos(19,41))],pos(13,41)),type(['PrimitiveBoolean'],0),pos(35,40))],pos(1,1)),type(['PrimitiveBoolean'],0),pos(1,38)),type(['PrimitiveBoolean'],0),pos(1,37))],pos(1,14)),global_scope(2),exact_scopes([]),upper_bound_scopes([]),bitwidth(-1),maxseq(-1),index(0),pos(1,37))]),functions([predicate('ObeysOrigins',[identifier('s',type(['EndPoint'],1),pos(20,32))],[field('s',identifier('Server',type(['EndPoint'],1),pos(5,2)),type(['EndPoint'],1),[],pos(20,32))],'all'(['r'],[field('r','oneof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(35,18)),type(['HTTPEvent'],1),[],pos(37,32))],'or'(['implication'('equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),identifier('s',type(['EndPoint'],1),pos(19,21)),type(['PrimitiveBoolean'],0),pos(6,33)),'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('to',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(2,17)),type(['PrimitiveBoolean'],0),pos(10,34)),type(['PrimitiveBoolean'],0),pos(10,33)), 'equal'('join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('origin',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(13,26)),'join'(identifier('r',type(['HTTPEvent'],1),pos(5,16)),identifier('from',type(['HTTPEvent', 'EndPoint'],2),pos(1,5)),type(['EndPoint'],1),pos(6,18)),type(['PrimitiveBoolean'],0),pos(29,34))],pos(17,34)),type(['PrimitiveBoolean'],0),pos(33,32)),pos(1,32))]),signatures([signature('EndPoint',[],[],[abstract],pos(14,1)),signature('Server',[field('causes','setof'(identifier('HTTPEvent',type(['HTTPEvent'],1),pos(46,4)),type(['HTTPEvent'],1),pos(9,3)),type(['HTTPEvent'],1),[],pos(1,3))],[],[subsig('EndPoint')],pos(5,2)),signature('Client',[],[],[subsig('EndPoint')],pos(5,4)),signature('HTTPEvent',[field('from','oneof'(identifier('EndPoint',type(['EndPoint'],1),pos(14,1)),type(['EndPoint'],1),pos(19,5)),type(['EndPoint'],1),[],pos(1,5)),field('to','oneof'(identifier('EndPoint',type(['EndPoint'],1),pos(14,1)),type(['EndPoint'],1),pos(19,5)),type(['EndPoint'],1),[],pos(1,5)),field('origin','oneof'(identifier('EndPoint',type(['EndPoint'],1),pos(14,1)),type(['EndPoint'],1),pos(19,5)),type(['EndPoint'],1),[],pos(1,5))],[],[abstract],pos(46,4)),signature('Request',[field('response','loneof'(identifier('Response',type(['HTTPEvent'],1),pos(5,8)),type(['HTTPEvent'],1),pos(43,6)),type(['HTTPEvent'],1),[],pos(33,6))],[],[subsig('HTTPEvent')],pos(5,6)),signature('Response',[field('embeds','setof'(identifier('Request',type(['HTTPEvent'],1),pos(5,6)),type(['HTTPEvent'],1),pos(42,8)),type(['HTTPEvent'],1),[],pos(34,8))],[],[subsig('HTTPEvent')],pos(5,8)),signature('Redirect',[],[],[subsig('Response')],pos(5,10))]),ordered_signatures([]),[sequences:false]),alloy_model('util''integer',facts([]),assertions([]),commands([]),functions([function('integer''add',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,11))],fun_call('integer''plus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(32,11)),pos(1,11)),function('integer''plus',[identifier('n1',type(['Int'],1),pos(11,12)), identifier('n2',type(['Int'],1),pos(15,12))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,12))],'cast2sigint'('integer''plus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,12)),type(['Int'],1),pos(30,12)),pos(1,12)),function('integer''sub',[identifier('n1',type(['Int'],1),pos(12,14)), identifier('n2',type(['Int'],1),pos(16,14))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,14))],fun_call('integer''minus',[identifier('n1',type(['Int'],1),pos(11,11)), identifier('n2',type(['Int'],1),pos(15,11))],type(['Int'],1),pos(33,14)),pos(1,14)),function('integer''minus',[identifier('n1',type(['Int'],1),pos(12,15)), identifier('n2',type(['Int'],1),pos(16,15))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,15))],'cast2sigint'('integer''minus'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(36,15)),type(['Int'],1),pos(31,15)),pos(1,15)),function('integer''mul',[identifier('n1',type(['Int'],1),pos(10,17)), identifier('n2',type(['Int'],1),pos(14,17))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,17))],'cast2sigint'('closure'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(34,17)),type(['Int'],1),pos(29,17)),pos(1,17)),function('integer''div',[identifier('n1',type(['Int'],1),pos(10,25)), identifier('n2',type(['Int'],1),pos(14,25))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,25))],'cast2sigint'('integer''div'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(34,25)),type(['Int'],1),pos(29,25)),pos(1,25)),function('integer''rem',[identifier('n1',type(['Int'],1),pos(10,28)), identifier('n2',type(['Int'],1),pos(14,28))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,28))],'cast2sigint'('integer''rem'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(34,28)),type(['Int'],1),pos(29,28)),pos(1,28)),function('integer''negate',[identifier('n',type(['Int'],1),pos(13,31))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,31))],'cast2sigint'('integer''minus'(integer(0,pos(29,31)),'cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),type(['Int'],1),pos(31,31)),type(['Int'],1),pos(27,31)),pos(1,31)),predicate('integer''eq',[identifier('n1',type(['Int'],1),pos(10,34)), identifier('n2',type(['Int'],1),pos(14,34))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,34))],'equal'('cast2sigint'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),type(['Int'],1),pos(25,34)),'cast2sigint'('cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['Int'],1),pos(35,34)),type(['PrimitiveBoolean'],0),pos(33,34)),pos(1,34)),predicate('integer''gt',[identifier('n1',type(['Int'],1),pos(10,37)), identifier('n2',type(['Int'],1),pos(14,37))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,37))],'greater'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['PrimitiveBoolean'],0),pos(28,37)),pos(1,37)),predicate('integer''lt',[identifier('n1',type(['Int'],1),pos(10,40)), identifier('n2',type(['Int'],1),pos(14,40))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(10,40))],'less'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['PrimitiveBoolean'],0),pos(28,40)),pos(1,40)),predicate('integer''gte',[identifier('n1',type(['Int'],1),pos(11,43)), identifier('n2',type(['Int'],1),pos(15,43))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,43))],'greater_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['PrimitiveBoolean'],0),pos(29,43)),pos(1,43)),predicate('integer''lte',[identifier('n1',type(['Int'],1),pos(11,46)), identifier('n2',type(['Int'],1),pos(15,46))],[field('n1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46)),field('n2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(11,46))],'less_equal'('cast2int'(identifier('n1',type(['Int'],1),pos(11,11)),type(['Int'],1),pos(32,12)),'cast2int'(identifier('n2',type(['Int'],1),pos(15,11)),type(['Int'],1),pos(43,12)),type(['PrimitiveBoolean'],0),pos(29,46)),pos(1,46)),predicate('integer''zero',[identifier('n',type(['Int'],1),pos(12,49))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,49))],'equal'(identifier('n',type(['Int'],1),pos(13,31)),integer(0,pos(26,49)),type(['PrimitiveBoolean'],0),pos(24,49)),pos(1,49)),predicate('integer''pos',[identifier('n',type(['Int'],1),pos(12,52))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,52))],'greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),pos(1,52)),predicate('integer''neg',[identifier('n',type(['Int'],1),pos(12,55))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,55))],'less'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),pos(1,55)),predicate('integer''nonpos',[identifier('n',type(['Int'],1),pos(14,58))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,58))],'less_equal'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(29,58)),type(['PrimitiveBoolean'],0),pos(26,58)),pos(1,58)),predicate('integer''nonneg',[identifier('n',type(['Int'],1),pos(14,61))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,61))],'greater_equal'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(29,61)),type(['PrimitiveBoolean'],0),pos(26,61)),pos(1,61)),function('integer''signum',[identifier('n',type(['Int'],1),pos(13,64))],[field('n',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,64))],if_then_else('less'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(26,55)),type(['PrimitiveBoolean'],0),pos(24,55)),'integer''minus'(integer(0,pos(37,64)),integer(1,pos(47,64)),type(['Int'],1),pos(39,64)),if_then_else('greater'('cast2int'(identifier('n',type(['Int'],1),pos(13,31)),type(['Int'],1),pos(39,31)),integer(0,pos(26,52)),type(['PrimitiveBoolean'],0),pos(24,52)),integer(1,pos(63,64)),integer(0,pos(70,64)),type(['Int'],1),pos(60,64)),type(['Int'],1),pos(33,64)),pos(1,64)),function('integer''int2elem',[identifier('i',type(['Int'],1),pos(14,71)), identifier('next',type(['univ', 'univ'],2),pos(22,71)), identifier('s',type(['univ'],1),pos(40,71))],[field('i',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,71)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(22,71)), field('s','setof'(identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),pos(43,71)),type(['univ'],1),[],pos(40,71))],'comprehension'(['e'],[field('e','oneof'(identifier('s',type(['EndPoint'],1),pos(19,21)),type(['univ'],1),pos(7,72)),type(['univ'],1),[],pos(4,72))],'equal'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['HTTPEvent'],1),pos(5,21)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),'cast2sigint'('cast2int'(identifier('i',type(['Int'],1),pos(14,71)),type(['Int'],1),pos(22,72)),type(['Int'],1),pos(22,72)),type(['PrimitiveBoolean'],0),pos(20,72)),type(['univ'],1),pos(3,72)),pos(1,71)),function('integer''elem2int',[identifier('e',type(['univ'],1),pos(14,80)), identifier('next',type(['univ', 'univ'],2),pos(23,80))],[field('e',identifier('univ',type(['univ'],1),pos(1,1)),type(['univ'],1),[],pos(14,80)), field('next','cartesian'(identifier('univ',type(['univ'],1),pos(1,1)),identifier('univ',type(['univ'],1),pos(1,1)),type(['univ', 'univ'],2),pos(32,71)),type(['univ', 'univ'],2),[],pos(23,80))],'cast2sigint'('card'('join'('closure1'(identifier('next',type(['univ', 'univ'],2),pos(22,71)),type(['univ', 'univ'],2),pos(12,72)),identifier('e',type(['HTTPEvent'],1),pos(5,21)),type(['univ'],1),pos(17,72)),type(['Int'],1),pos(11,72)),type(['Int'],1),pos(52,80)),pos(1,80)),function('integer''max',[],[],'cast2sigint'(integer(max,pos(19,85)),type(['Int'],1),pos(17,85)),pos(1,85)),function('integer''max',[identifier('es',type(['Int'],1),pos(10,97))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,97))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(42,97)),type(['Int'],1),pos(38,97)),pos(1,97)),function('integer''min',[],[],'cast2sigint'(integer(min,pos(19,88)),type(['Int'],1),pos(17,88)),pos(1,88)),function('integer''min',[identifier('es',type(['Int'],1),pos(10,100))],[field('es','setof'(identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),pos(14,97)),type(['Int'],1),[],pos(10,100))],'minus'(identifier('es',type(['Int'],1),pos(10,97)),'join'(identifier('es',type(['Int'],1),pos(10,97)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(42,100)),type(['Int'],1),pos(38,100)),pos(1,100)),function('integer''next',[],[],identifier('next',type(['univ', 'univ'],2),pos(22,71)),pos(1,91)),function('integer''prev',[],[],'inverse'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(22,94)),type(['Int', 'Int'],2),pos(21,94)),pos(1,94)),function('integer''prevs',[identifier('e',type(['Int'],1),pos(12,103))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,103))],'join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'closure1'(fun_call('integer''prev',[],type(['Int', 'Int'],2),pos(44,97)),type(['Int', 'Int'],2),pos(43,97)),type(['Int'],1),pos(32,103)),pos(1,103)),function('integer''nexts',[identifier('e',type(['Int'],1),pos(12,106))],[field('e',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(12,106))],'join'(identifier('e',type(['HTTPEvent'],1),pos(5,21)),'closure1'(fun_call('integer''next',[],type(['Int', 'Int'],2),pos(44,100)),type(['Int', 'Int'],2),pos(43,100)),type(['Int'],1),pos(32,106)),pos(1,106)),function('integer''larger',[identifier('e1',type(['Int'],1),pos(13,109)), identifier('e2',type(['Int'],1),pos(17,109))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(13,109))],let('a','cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let('b','cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('b',type(['Int'],1),pos(48,109)),identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(65,109)),type(['Int'],1),pos(49,109)),type(['Int'],1),pos(38,109)),pos(1,109)),function('integer''smaller',[identifier('e1',type(['Int'],1),pos(14,112)), identifier('e2',type(['Int'],1),pos(18,112))],[field('e1',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112)),field('e2',identifier('Int',type(['Int'],1),pos(1,1)),type(['Int'],1),[],pos(14,112))],let('a','cast2sigint'('cast2int'(identifier('e1',type(['Int'],1),pos(13,109)),type(['Int'],1),pos(39,109)),type(['Int'],1),pos(39,109)),let('b','cast2sigint'('cast2int'(identifier('e2',type(['Int'],1),pos(17,109)),type(['Int'],1),pos(50,109)),type(['Int'],1),pos(50,109)),if_then_else('less'('cast2int'(identifier('a',type(['Int'],1),pos(37,109)),type(['Int'],1),pos(61,109)),'cast2int'(identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(63,109)),type(['PrimitiveBoolean'],0),pos(62,109)),identifier('a',type(['Int'],1),pos(37,109)),identifier('b',type(['Int'],1),pos(48,109)),type(['Int'],1),pos(66,112)),type(['Int'],1),pos(50,112)),type(['Int'],1),pos(39,112)),pos(1,112))]),signatures([]),ordered_signatures([]),[sequences:false])]).
