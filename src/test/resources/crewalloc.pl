alloy_model(facts([fact(equal(this/male,plus(plus(this/tom,this/david,type([this/tom,this/david]),pos(13,13)),this/jeremy,type([this/tom,this/david,this/jeremy]),pos(19,13)),type([]),pos(8,13)),(1,12)),fact(equal(speaks,plus(plus(plus(plus(plus(cartesian(this/tom,this/german,type([this/tom->this/german]),pos(14,16)),cartesian(this/david,this/french,type([this/david->this/french]),pos(30,16)),type([this/tom->this/german,this/david->this/french]),pos(23,16)),cartesian(this/jeremy,this/german,type([this/jeremy->this/german]),pos(47,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german]),pos(39,16)),cartesian(this/carol,this/spanish,type([this/carol->this/spanish]),pos(63,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish]),pos(56,16)),cartesian(this/janet,this/french,type([this/janet->this/french]),pos(80,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish,this/janet->this/french]),pos(73,16)),cartesian(this/tracy,this/spanish,type([this/tracy->this/spanish]),pos(96,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish,this/janet->this/french,this/tracy->this/spanish]),pos(89,16)),type([]),pos(10,16)),(1,15))]),assertions([]),commands([run(and([equal(this/male,plus(plus(this/tom,this/david,type([this/tom,this/david]),pos(13,13)),this/jeremy,type([this/tom,this/david,this/jeremy]),pos(19,13)),type([]),pos(8,13)), equal(speaks,plus(plus(plus(plus(plus(cartesian(this/tom,this/german,type([this/tom->this/german]),pos(14,16)),cartesian(this/david,this/french,type([this/david->this/french]),pos(30,16)),type([this/tom->this/german,this/david->this/french]),pos(23,16)),cartesian(this/jeremy,this/german,type([this/jeremy->this/german]),pos(47,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german]),pos(39,16)),cartesian(this/carol,this/spanish,type([this/carol->this/spanish]),pos(63,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish]),pos(56,16)),cartesian(this/janet,this/french,type([this/janet->this/french]),pos(80,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish,this/janet->this/french]),pos(73,16)),cartesian(this/tracy,this/spanish,type([this/tracy->this/spanish]),pos(96,16)),type([this/tom->this/german,this/david->this/french,this/jeremy->this/german,this/carol->this/spanish,this/janet->this/french,this/tracy->this/spanish]),pos(89,16)),type([]),pos(10,16)), pred_call(this/allLanguages,[],type([]),pos(3,31), pred_call(this/allSexes,[],type([]),pos(20,31), pred_call(this/scheduleOk,[],type([]),pos(33,31), pred_call(this/everybodyInSchedule,[],type([]),pos(48,31)],pos(1,12)),global_scope(-1),exact_scopes([this/Flight]),bitwidth(-1),pos(1,34)]),functions([predicate(this/allLanguages,all([field(f,oneof(this/Flight,type([this/Flight]),pos(9,19)),pos(7,19))],type([]),equal(join(join(f,assign,type([this/Personell]),pos(19,19)),speaks,type([this/Language]),pos(26,19)),this/Language,type([]),pos(34,19)),pos(3,19),pos(1,18)),predicate(this/allSexes,all([field(f,oneof(this/Flight,type([this/Flight]),pos(9,22)),pos(7,22))],type([]),and([some(minus(join(f,assign,type([this/Personell]),pos(25,22)),this/male,type([this/Personell]),pos(32,22)),type([]),pos(18,22)), some(intersection(join(f,assign,type([this/Personell]),pos(50,22)),this/male,type([this/Personell]),pos(58,22)),type([]),pos(43,22))],pos(39,22)),pos(3,22),pos(1,21)),predicate(this/scheduleOk,all([field(p,oneof(this/Personell,type([this/Personell]),pos(9,25)),pos(7,25)), field(f,oneof(this/Flight,type([this/Flight]),pos(24,25)),pos(20,25))],type([]),implication(and([in(p,join(f,assign,type([this/Personell]),pos(39,25)),type([]),pos(35,25)), in(p,join(join(f,fun_call(ordering/next,[],type([this/Flight->this/Flight]),pos(56,25),type([this/Flight]),pos(56,25)),assign,type([this/Personell]),pos(63,25)),type([]),pos(53,25))],pos(47,25)),not(in(p,join(join(join(f,fun_call(ordering/next,[],type([this/Flight->this/Flight]),pos(89,25),type([this/Flight]),pos(89,25)),fun_call(ordering/next,[],type([this/Flight->this/Flight]),pos(84,25),type([this/Flight]),pos(84,25)),assign,type([this/Personell]),pos(97,25)),type([]),pos(81,25)),type([]),pos(74,25)),type([]),pos(71,25)),pos(3,25),pos(1,24)),predicate(this/everybodyInSchedule,equal(join(univ,assign,type([this/Personell]),pos(7,28)),this/Personell,type([]),pos(15,28)),pos(1,27)),predicate(this/crewAlloc,and([pred_call(this/allLanguages,[],type([]),pos(3,31), pred_call(this/allSexes,[],type([]),pos(20,31), pred_call(this/scheduleOk,[],type([]),pos(33,31), pred_call(this/everybodyInSchedule,[],type([]),pos(48,31)],pos(44,31)),pos(1,30))]),signatures([signature(this/Language,[],[],[abstract],pos(14,3)),signature(this/french,[],[],[one, subsig(this/Language)],pos(9,4)),signature(this/german,[],[],[one, subsig(this/Language)],pos(17,4)),signature(this/spanish,[],[],[one, subsig(this/Language)],pos(25,4)),signature(this/Personell,[field(speaks,setof(this/Language,type([this/Language]),pos(35,5)),pos(26,5))],[],[abstract],pos(14,5)),signature(this/tom,[],[],[one, subsig(this/Personell)],pos(9,6)),signature(this/david,[],[],[one, subsig(this/Personell)],pos(14,6)),signature(this/jeremy,[],[],[one, subsig(this/Personell)],pos(21,6)),signature(this/carol,[],[],[one, subsig(this/Personell)],pos(29,6)),signature(this/janet,[],[],[one, subsig(this/Personell)],pos(36,6)),signature(this/tracy,[],[],[one, subsig(this/Personell)],pos(43,6)),signature(this/male,[],[],[subset([this/Personell]],pos(5,7)),signature(this/Flight,[field(assign,setof(this/Personell,type([this/Personell]),pos(12,9)),pos(3,9))],[],[],pos(5,8))])).