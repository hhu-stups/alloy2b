alloy_model(facts([fact(all(['d_', 'o_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(15,11)),type([['Dir_']],1),pos(12,11)), field('o_',oneof(join(identifier('d_',type([['Dir_']],1),pos(12,11)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(24,11)),type([['FSObject_']],1),pos(23,11)),type([['FSObject_']],1),pos(20,11))],equal(join(identifier('o_',type([['FSObject_']],1),pos(20,11)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(37,11)),identifier('d_',type([['Dir_']],1),pos(12,11)),type([untyped],0),pos(45,11)),type([untyped],0),pos(8,11)),(1,11)),fact(equal(plus(identifier('File_',type([['File_']],1),pos(5,8)),identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['File_'], ['Dir_']],1),pos(13,14)),identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([untyped],0),pos(19,14)),(1,14)),fact(in(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),join(identifier('Root_',type([['Root_']],1),pos(9,17)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['univ_', 'univ_']],2),pos(25,20)),type([['univ_']],1),pos(24,20)),type([untyped],0),pos(17,20)),(1,20))]),assertions([fact(no(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(24,23)),type([['Dir_']],1),pos(21,23))],in(identifier('d_',type([['Dir_']],1),pos(21,23)),join(identifier('d_',type([['Dir_']],1),pos(21,23)),closure1(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['Dir_', 'FSObject_']],2),pos(37,23)),type([['FSObject_']],1),pos(36,23)),type([untyped],0),pos(32,23)),type([untyped],0),pos(18,23)),(1,23)),fact(one(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(25,29)),type([['Dir_']],1),pos(22,29))],no(join(identifier('d_',type([['Dir_']],1),pos(22,29)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(35,29)),type([untyped],0),pos(31,29)),type([untyped],0),pos(18,29)),(1,29)),fact(all(['o_'],[field('o_',oneof(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([['FSObject_']],1),pos(29,35)),type([['FSObject_']],1),pos(26,35))],lone(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(48,35)),type([['Dir_']],1),pos(45,35))],in(identifier('o_',type([['FSObject_']],1),pos(26,35)),join(identifier('d_',type([['Dir_']],1),pos(45,35)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(60,35)),type([untyped],0),pos(56,35)),type([untyped],0),pos(40,35)),type([untyped],0),pos(22,35)),(1,35))]),commands([check(and([all(['d_', 'o_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(15,11)),type([['Dir_']],1),pos(12,11)), field('o_',oneof(join(identifier('d_',type([['Dir_']],1),pos(12,11)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(24,11)),type([['FSObject_']],1),pos(23,11)),type([['FSObject_']],1),pos(20,11))],equal(join(identifier('o_',type([['FSObject_']],1),pos(20,11)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(37,11)),identifier('d_',type([['Dir_']],1),pos(12,11)),type([untyped],0),pos(45,11)),type([untyped],0),pos(8,11)), equal(plus(identifier('File_',type([['File_']],1),pos(5,8)),identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['File_'], ['Dir_']],1),pos(13,14)),identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([untyped],0),pos(19,14)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),join(identifier('Root_',type([['Root_']],1),pos(9,17)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['univ_', 'univ_']],2),pos(25,20)),type([['univ_']],1),pos(24,20)),type([untyped],0),pos(17,20)), not(no(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(24,23)),type([['Dir_']],1),pos(21,23))],in(identifier('d_',type([['Dir_']],1),pos(21,23)),join(identifier('d_',type([['Dir_']],1),pos(21,23)),closure1(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['Dir_', 'FSObject_']],2),pos(37,23)),type([['FSObject_']],1),pos(36,23)),type([untyped],0),pos(32,23)),type([untyped],0),pos(18,23)),type([untyped],0),pos(1,23))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,26)),check(and([all(['d_', 'o_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(15,11)),type([['Dir_']],1),pos(12,11)), field('o_',oneof(join(identifier('d_',type([['Dir_']],1),pos(12,11)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(24,11)),type([['FSObject_']],1),pos(23,11)),type([['FSObject_']],1),pos(20,11))],equal(join(identifier('o_',type([['FSObject_']],1),pos(20,11)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(37,11)),identifier('d_',type([['Dir_']],1),pos(12,11)),type([untyped],0),pos(45,11)),type([untyped],0),pos(8,11)), equal(plus(identifier('File_',type([['File_']],1),pos(5,8)),identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['File_'], ['Dir_']],1),pos(13,14)),identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([untyped],0),pos(19,14)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),join(identifier('Root_',type([['Root_']],1),pos(9,17)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['univ_', 'univ_']],2),pos(25,20)),type([['univ_']],1),pos(24,20)),type([untyped],0),pos(17,20)), not(one(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(25,29)),type([['Dir_']],1),pos(22,29))],no(join(identifier('d_',type([['Dir_']],1),pos(22,29)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(35,29)),type([untyped],0),pos(31,29)),type([untyped],0),pos(18,29)),type([untyped],0),pos(1,29))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,32)),check(and([all(['d_', 'o_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(15,11)),type([['Dir_']],1),pos(12,11)), field('o_',oneof(join(identifier('d_',type([['Dir_']],1),pos(12,11)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(24,11)),type([['FSObject_']],1),pos(23,11)),type([['FSObject_']],1),pos(20,11))],equal(join(identifier('o_',type([['FSObject_']],1),pos(20,11)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(37,11)),identifier('d_',type([['Dir_']],1),pos(12,11)),type([untyped],0),pos(45,11)),type([untyped],0),pos(8,11)), equal(plus(identifier('File_',type([['File_']],1),pos(5,8)),identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['File_'], ['Dir_']],1),pos(13,14)),identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([untyped],0),pos(19,14)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),join(identifier('Root_',type([['Root_']],1),pos(9,17)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['univ_', 'univ_']],2),pos(25,20)),type([['univ_']],1),pos(24,20)),type([untyped],0),pos(17,20)), not(all(['o_'],[field('o_',oneof(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([['FSObject_']],1),pos(29,35)),type([['FSObject_']],1),pos(26,35))],lone(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(48,35)),type([['Dir_']],1),pos(45,35))],in(identifier('o_',type([['FSObject_']],1),pos(26,35)),join(identifier('d_',type([['Dir_']],1),pos(45,35)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(28,5)),type([['FSObject_']],1),pos(60,35)),type([untyped],0),pos(56,35)),type([untyped],0),pos(40,35)),type([untyped],0),pos(22,35)),type([untyped],0),pos(1,35))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,38))]),functions([]),signatures([signature('FSObject_',[field('parent_',loneof(identifier('Dir_',type([['Dir_']],1),pos(5,5)),type([['Dir_']],1),pos(24,2)),type([['Dir_']],1),pos(16,2))],[],[],pos(5,2)),signature('Dir_',[field('contents_',setof(identifier('FSObject_',type([['FSObject_']],1),pos(5,2)),type([['FSObject_']],1),pos(38,5)),type([['FSObject_']],1),pos(28,5))],[],[subsig('FSObject_')],pos(5,5)),signature('File_',[],[],[subsig('FSObject_')],pos(5,8)),signature('Root_',[],[no(join(identifier('Root_',type([['Root_']],1),pos(9,17)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(16,2)),type([['Dir_']],1),pos(35,17)),type([untyped],0),pos(32,17))],[one, subsig('Dir_')],pos(9,17))])).