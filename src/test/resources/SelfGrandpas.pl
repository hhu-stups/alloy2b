alloy_model(facts([fact(no(['p_'],[field('p_',oneof(identifier('this'/'Person_',type('this'/'Person_'),pos(14,3)),type('this'/'Person_'),pos(12,17)),type('this'/'Person_'),pos(8,17))],in(identifier('p_',type('this'/'Person_'),pos(8,17)),join(identifier('p_',type('this'/'Person_'),pos(8,17)),closure1(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(37,17)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(28,17)),type('this'/'Woman_','this'/'Man_'),pos(27,17)),type(untyped),pos(23,17)),type(untyped),pos(5,17)),(1,16)),fact(equal(identifier('wife_',type('this'/'Man->this_'/'Woman_'),pos(5,9)),inverse(identifier('husband_',type('this'/'Woman->this_'/'Man_'),pos(5,13)),type('this'/'Man->this_'/'Woman_'),pos(12,21)),type(untyped),pos(10,21)),(1,20)),fact(and([no(intersection(identifier('wife_',type('this'/'Man->this_'/'Woman_'),pos(5,9)),join(closure(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(24,25)),type('univ->univ_'),pos(15,25)),identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),type('univ->this_'/'Woman_'),pos(33,25)),type('this'/'Man->this_'/'Woman_'),pos(13,25)),type(untyped),pos(5,25)), no(intersection(identifier('husband_',type('this'/'Woman->this_'/'Man_'),pos(5,13)),join(closure(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(27,26)),type('univ->univ_'),pos(18,26)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('univ->this_'/'Man_'),pos(36,26)),type('this'/'Woman->this_'/'Man_'),pos(16,26)),type(untyped),pos(5,26))],pos(1,1)),(1,24))]),assertions([]),commands([run(and([no(['p_'],[field('p_',oneof(identifier('this'/'Person_',type('this'/'Person_'),pos(14,3)),type('this'/'Person_'),pos(12,17)),type('this'/'Person_'),pos(8,17))],in(identifier('p_',type('this'/'Person_'),pos(8,17)),join(identifier('p_',type('this'/'Person_'),pos(8,17)),closure1(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(37,17)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(28,17)),type('this'/'Woman_','this'/'Man_'),pos(27,17)),type(untyped),pos(23,17)),type(untyped),pos(5,17)), equal(identifier('wife_',type('this'/'Man->this_'/'Woman_'),pos(5,9)),inverse(identifier('husband_',type('this'/'Woman->this_'/'Man_'),pos(5,13)),type('this'/'Man->this_'/'Woman_'),pos(12,21)),type(untyped),pos(10,21)), no(intersection(identifier('wife_',type('this'/'Man->this_'/'Woman_'),pos(5,9)),join(closure(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(24,25)),type('univ->univ_'),pos(15,25)),identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),type('univ->this_'/'Woman_'),pos(33,25)),type('this'/'Man->this_'/'Woman_'),pos(13,25)),type(untyped),pos(5,25)), no(intersection(identifier('husband_',type('this'/'Woman->this_'/'Man_'),pos(5,13)),join(closure(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(27,26)),type('univ->univ_'),pos(18,26)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('univ->this_'/'Man_'),pos(36,26)),type('this'/'Woman->this_'/'Man_'),pos(16,26)),type(untyped),pos(5,26)), some(['m_'],[field('m_',identifier('this'/'Man_',type('this'/'Man_'),pos(5,8)),type('this'/'Man_'),pos(17,34))],in(identifier('m_',type('this'/'Man_'),pos(17,34)),fun_call('this'/'grandpas_',[identifier('m_',type('this'/'Man_'),pos(17,34))],type('this'/'Person_'),pos(10,35)),type(untyped),pos(7,35)),type(untyped),pos(1,1))],pos(1,16)),global_scope(-1),exact_scopes([]),bitwidth(-1),pos(1,38))]),functions([function('this'/'grandpas_',[identifier('p_',type('this'/'Person_'),pos(14,29))],[field('p_',identifier('this'/'Person_',type('this'/'Person_'),pos(14,3)),type('this'/'Person_'),pos(14,29))],let(plus(plus(plus(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(25,30)),join(identifier('father_',type('this'/'Person->this_'/'Man_'),pos(5,4)),identifier('wife_',type('this'/'Man->this_'/'Woman_'),pos(5,9)),type('this'/'Person->this_'/'Woman_'),pos(42,30)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(34,30)),join(identifier('mother_',type('this'/'Person->this_'/'Woman_'),pos(5,5)),identifier('husband_',type('this'/'Woman->this_'/'Man_'),pos(5,13)),type('this'/'Person->this_'/'Man_'),pos(56,30)),type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(48,30)),intersection(join(join(identifier('p_',type('this'/'Person_'),pos(14,29)),identifier('parent_',type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(9,30)),type('this'/'Woman_','this'/'Man_'),pos(8,31)),identifier('parent_',type('this'/'Person->this_'/'Woman_','this'/'Person->this_'/'Man_'),pos(9,30)),type('this'/'Woman_','this'/'Man_'),pos(15,31)),identifier('this'/'Man_',type('this'/'Man_'),pos(5,8)),type('this'/'Man_'),pos(23,31)),type('this'/'Man_'),pos(16,30)),pos(1,29)),predicate('this'/'ownGrandpa_',[identifier('m_',type('this'/'Man_'),pos(17,34))],[field('m_',identifier('this'/'Man_',type('this'/'Man_'),pos(5,8)),type('this'/'Man_'),pos(17,34))],in(identifier('m_',type('this'/'Man_'),pos(17,34)),fun_call('this'/'grandpas_',[identifier('m_',type('this'/'Man_'),pos(17,34))],type('this'/'Person_'),pos(10,35)),type(untyped),pos(7,35)),pos(1,34))]),signatures([signature('this'/'Person_',[field('father_',loneof(identifier('this'/'Man_',type('this'/'Man_'),pos(5,8)),type('this'/'Man_'),pos(14,4)),type('this'/'Man_'),pos(5,4)),field('mother_',loneof(identifier('this'/'Woman_',type('this'/'Woman_'),pos(5,12)),type('this'/'Woman_'),pos(14,5)),type('this'/'Woman_'),pos(5,5))],[],[abstract],pos(14,3)),signature('this'/'Man_',[field('wife_',loneof(identifier('this'/'Woman_',type('this'/'Woman_'),pos(5,12)),type('this'/'Woman_'),pos(12,9)),type('this'/'Woman_'),pos(5,9))],[],[subsig('this'/'Person_')],pos(5,8)),signature('this'/'Woman_',[field('husband_',loneof(identifier('this'/'Man_',type('this'/'Man_'),pos(5,8)),type('this'/'Man_'),pos(15,13)),type('this'/'Man_'),pos(5,13))],[],[subsig('this'/'Person_')],pos(5,12))])).