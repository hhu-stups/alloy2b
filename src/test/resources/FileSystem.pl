alloy_model(facts([fact(all([field(d,oneof(this/Dir,type([this/Dir]),pos(15,11)),pos(12,11)), field(o,oneof(join(d,contents,type([this/FSObject]),pos(24,11)),type([this/FSObject]),pos(23,11)),pos(20,11))],type([]),equal(join(o,parent,type([this/Dir]),pos(37,11)),d,type([]),pos(45,11)),pos(8,11),(1,11)),fact(equal(plus(this/File,this/Dir,type([this/File,this/Dir]),pos(13,14)),this/FSObject,type([]),pos(19,14)),(1,14)),fact(in(this/FSObject,join(this/Root,closure(contents,type([univ->univ]),pos(25,20)),type([univ]),pos(24,20)),type([]),pos(17,20)),(1,20))]),assertions([fact(no([field(d,oneof(this/Dir,type([this/Dir]),pos(24,23)),pos(21,23))],type([]),in(d,join(d,closure1(contents,type([this/Dir->this/FSObject]),pos(37,23)),type([this/FSObject]),pos(36,23)),type([]),pos(32,23)),pos(18,23),(1,23)),fact(one([field(d,oneof(this/Dir,type([this/Dir]),pos(25,29)),pos(22,29))],type([]),no(join(d,parent,type([this/Dir]),pos(35,29)),type([]),pos(31,29)),pos(18,29),(1,29)),fact(all([field(o,oneof(this/FSObject,type([this/FSObject]),pos(29,35)),pos(26,35))],type([]),lone([field(d,oneof(this/Dir,type([this/Dir]),pos(48,35)),pos(45,35))],type([]),in(o,join(d,contents,type([this/FSObject]),pos(60,35)),type([]),pos(56,35)),pos(40,35),pos(22,35),(1,35))]),commands([check(and([all([field(d,oneof(this/Dir,type([this/Dir]),pos(15,11)),pos(12,11)), field(o,oneof(join(d,contents,type([this/FSObject]),pos(24,11)),type([this/FSObject]),pos(23,11)),pos(20,11))],type([]),equal(join(o,parent,type([this/Dir]),pos(37,11)),d,type([]),pos(45,11)),pos(8,11), equal(plus(this/File,this/Dir,type([this/File,this/Dir]),pos(13,14)),this/FSObject,type([]),pos(19,14)), in(this/FSObject,join(this/Root,closure(contents,type([univ->univ]),pos(25,20)),type([univ]),pos(24,20)),type([]),pos(17,20)), not(no([field(d,oneof(this/Dir,type([this/Dir]),pos(24,23)),pos(21,23))],type([]),in(d,join(d,closure1(contents,type([this/Dir->this/FSObject]),pos(37,23)),type([this/FSObject]),pos(36,23)),type([]),pos(32,23)),pos(18,23),type([]),pos(1,23))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,26),check(and([all([field(d,oneof(this/Dir,type([this/Dir]),pos(15,11)),pos(12,11)), field(o,oneof(join(d,contents,type([this/FSObject]),pos(24,11)),type([this/FSObject]),pos(23,11)),pos(20,11))],type([]),equal(join(o,parent,type([this/Dir]),pos(37,11)),d,type([]),pos(45,11)),pos(8,11), equal(plus(this/File,this/Dir,type([this/File,this/Dir]),pos(13,14)),this/FSObject,type([]),pos(19,14)), in(this/FSObject,join(this/Root,closure(contents,type([univ->univ]),pos(25,20)),type([univ]),pos(24,20)),type([]),pos(17,20)), not(one([field(d,oneof(this/Dir,type([this/Dir]),pos(25,29)),pos(22,29))],type([]),no(join(d,parent,type([this/Dir]),pos(35,29)),type([]),pos(31,29)),pos(18,29),type([]),pos(1,29))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,32),check(and([all([field(d,oneof(this/Dir,type([this/Dir]),pos(15,11)),pos(12,11)), field(o,oneof(join(d,contents,type([this/FSObject]),pos(24,11)),type([this/FSObject]),pos(23,11)),pos(20,11))],type([]),equal(join(o,parent,type([this/Dir]),pos(37,11)),d,type([]),pos(45,11)),pos(8,11), equal(plus(this/File,this/Dir,type([this/File,this/Dir]),pos(13,14)),this/FSObject,type([]),pos(19,14)), in(this/FSObject,join(this/Root,closure(contents,type([univ->univ]),pos(25,20)),type([univ]),pos(24,20)),type([]),pos(17,20)), not(all([field(o,oneof(this/FSObject,type([this/FSObject]),pos(29,35)),pos(26,35))],type([]),lone([field(d,oneof(this/Dir,type([this/Dir]),pos(48,35)),pos(45,35))],type([]),in(o,join(d,contents,type([this/FSObject]),pos(60,35)),type([]),pos(56,35)),pos(40,35),pos(22,35),type([]),pos(1,35))],pos(1,11)),global_scope(5),exact_scopes([]),bitwidth(-1),pos(1,38)]),functions([]),signatures([signature(this/FSObject,[field(parent,loneof(this/Dir,type([this/Dir]),pos(24,2)),pos(16,2))],[],[],pos(5,2)),signature(this/Dir,[field(contents,setof(this/FSObject,type([this/FSObject]),pos(38,5)),pos(28,5))],[],[subsig(this/FSObject)],pos(5,5)),signature(this/File,[],[],[subsig(this/FSObject)],pos(5,8)),signature(this/Root,[],[no(join(this/Root,parent,type([this/Dir]),pos(35,17)),type([]),pos(32,17))],[one, subsig(this/Dir)],pos(9,17))])).