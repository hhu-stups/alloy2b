alloy_model(facts([fact(and([no(join(identifier('Root_',type([['Root_']],1),pos(9,13)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(12,16)),type([untyped],0),pos(5,16)), equal(identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),inverse(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_', 'Dir_']],2),pos(14,17)),type([untyped],0),pos(12,17)), equal(plus(identifier('File_',type([['File_']],1),pos(5,11)),identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['File_'], ['Dir_']],1),pos(10,18)),identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([untyped],0),pos(16,18)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),join(identifier('Root_',type([['Root_']],1),pos(9,13)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['univ_', 'univ_']],2),pos(22,19)),type([['univ_']],1),pos(21,19)),type([untyped],0),pos(14,19))],pos(1,1)),(1,15))]),assertions([fact(no(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(12,23)),type([['Dir_']],1),[],pos(8,23))],in(identifier('d_',type([['Dir_']],1),pos(8,23)),join(identifier('d_',type([['Dir_']],1),pos(8,23)),closure1(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['Dir_', 'FSObject_']],2),pos(25,23)),type([['FSObject_']],1),pos(24,23)),type([untyped],0),pos(20,23)),type([untyped],0),pos(5,23)),(1,22)),fact(one(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(13,27)),type([['Dir_']],1),[],pos(9,27))],no(join(identifier('d_',type([['Dir_']],1),pos(9,27)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(23,27)),type([untyped],0),pos(19,27)),type([untyped],0),pos(5,27)),(1,26)),fact(all(['f_'],[field('f_',oneof(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([['FSObject_']],1),pos(13,31)),type([['FSObject_']],1),[],pos(9,31))],lone(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(33,31)),type([['Dir_']],1),[],pos(29,31))],in(identifier('f_',type([['FSObject_']],1),pos(9,31)),join(identifier('d_',type([['Dir_']],1),pos(29,31)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_']],1),pos(45,31)),type([untyped],0),pos(41,31)),type([untyped],0),pos(24,31)),type([untyped],0),pos(5,31)),(1,30))]),commands([check(and([no(join(identifier('Root_',type([['Root_']],1),pos(9,13)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(12,16)),type([untyped],0),pos(5,16)), equal(identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),inverse(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_', 'Dir_']],2),pos(14,17)),type([untyped],0),pos(12,17)), equal(plus(identifier('File_',type([['File_']],1),pos(5,11)),identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['File_'], ['Dir_']],1),pos(10,18)),identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([untyped],0),pos(16,18)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),join(identifier('Root_',type([['Root_']],1),pos(9,13)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['univ_', 'univ_']],2),pos(22,19)),type([['univ_']],1),pos(21,19)),type([untyped],0),pos(14,19)), not(no(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(12,23)),type([['Dir_']],1),[],pos(8,23))],in(identifier('d_',type([['Dir_']],1),pos(8,23)),join(identifier('d_',type([['Dir_']],1),pos(8,23)),closure1(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['Dir_', 'FSObject_']],2),pos(25,23)),type([['FSObject_']],1),pos(24,23)),type([untyped],0),pos(20,23)),type([untyped],0),pos(5,23)),type([untyped],0),pos(1,22))],pos(5,16)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,34)),check(and([no(join(identifier('Root_',type([['Root_']],1),pos(9,13)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(12,16)),type([untyped],0),pos(5,16)), equal(identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),inverse(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_', 'Dir_']],2),pos(14,17)),type([untyped],0),pos(12,17)), equal(plus(identifier('File_',type([['File_']],1),pos(5,11)),identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['File_'], ['Dir_']],1),pos(10,18)),identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([untyped],0),pos(16,18)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),join(identifier('Root_',type([['Root_']],1),pos(9,13)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['univ_', 'univ_']],2),pos(22,19)),type([['univ_']],1),pos(21,19)),type([untyped],0),pos(14,19)), not(one(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(13,27)),type([['Dir_']],1),[],pos(9,27))],no(join(identifier('d_',type([['Dir_']],1),pos(9,27)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(23,27)),type([untyped],0),pos(19,27)),type([untyped],0),pos(5,27)),type([untyped],0),pos(1,26))],pos(5,16)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,36)),check(and([no(join(identifier('Root_',type([['Root_']],1),pos(9,13)),identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),type([['Dir_']],1),pos(12,16)),type([untyped],0),pos(5,16)), equal(identifier('parent_',type([['FSObject_', 'Dir_']],2),pos(5,4)),inverse(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_', 'Dir_']],2),pos(14,17)),type([untyped],0),pos(12,17)), equal(plus(identifier('File_',type([['File_']],1),pos(5,11)),identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['File_'], ['Dir_']],1),pos(10,18)),identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([untyped],0),pos(16,18)), in(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),join(identifier('Root_',type([['Root_']],1),pos(9,13)),closure(identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['univ_', 'univ_']],2),pos(22,19)),type([['univ_']],1),pos(21,19)),type([untyped],0),pos(14,19)), not(all(['f_'],[field('f_',oneof(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([['FSObject_']],1),pos(13,31)),type([['FSObject_']],1),[],pos(9,31))],lone(['d_'],[field('d_',oneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(33,31)),type([['Dir_']],1),[],pos(29,31))],in(identifier('f_',type([['FSObject_']],1),pos(9,31)),join(identifier('d_',type([['Dir_']],1),pos(29,31)),identifier('contents_',type([['Dir_', 'FSObject_']],2),pos(5,8)),type([['FSObject_']],1),pos(45,31)),type([untyped],0),pos(41,31)),type([untyped],0),pos(24,31)),type([untyped],0),pos(5,31)),type([untyped],0),pos(1,30))],pos(5,16)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,38))]),functions([]),signatures([signature('FSObject_',[field('parent_',loneof(identifier('Dir_',type([['Dir_']],1),pos(5,7)),type([['Dir_']],1),pos(14,4)),type([['Dir_']],1),[],pos(5,4))],[],[],pos(5,3)),signature('Dir_',[field('contents_',setof(identifier('FSObject_',type([['FSObject_']],1),pos(5,3)),type([['FSObject_']],1),pos(16,8)),type([['FSObject_']],1),[],pos(5,8))],[],[subsig('FSObject_')],pos(5,7)),signature('File_',[],[],[subsig('FSObject_')],pos(5,11)),signature('Root_',[],[],[one, subsig('Dir_')],pos(9,13))])).