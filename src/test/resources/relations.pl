alloy_model(facts([fact(all(['b'],[field('b',oneof(identifier('BirthdayBook',type([['BirthdayBook']],1),pos(5,3)),type([['BirthdayBook']],1),pos(19,4)),type([['BirthdayBook']],1),[],pos(17,4))],in(join(identifier('b',type([['BirthdayBook']],1),pos(17,4)),identifier('date',type([['BirthdayBook', 'Name', 'Date']],3),pos(36,3)),type([['Name', 'Date']],2),pos(35,4)),relations(join(identifier('b',type([['BirthdayBook']],1),pos(17,4)),identifier('known',type([['BirthdayBook', 'Name']],2),pos(19,3)),type([['Name']],1),pos(45,4)),identifier('Date',type([['Date']],1),pos(5,2)),type([['Name', 'Date']],2),pos(52,4)),type([untyped],0),pos(41,4)),type([untyped],0),pos(13,4)),(1,4))]),assertions([]),commands([run(and([all(['b'],[field('b',oneof(identifier('BirthdayBook',type([['BirthdayBook']],1),pos(5,3)),type([['BirthdayBook']],1),pos(19,4)),type([['BirthdayBook']],1),[],pos(17,4))],in(join(identifier('b',type([['BirthdayBook']],1),pos(17,4)),identifier('date',type([['BirthdayBook', 'Name', 'Date']],3),pos(36,3)),type([['Name', 'Date']],2),pos(35,4)),relations(join(identifier('b',type([['BirthdayBook']],1),pos(17,4)),identifier('known',type([['BirthdayBook', 'Name']],2),pos(19,3)),type([['Name']],1),pos(45,4)),identifier('Date',type([['Date']],1),pos(5,2)),type([['Name', 'Date']],2),pos(52,4)),type([untyped],0),pos(41,4)),type([untyped],0),pos(13,4))],pos(1,4)),global_scope(-1),exact_scopes([('BirthdayBook',1), ('Name',2), ('Date',2)]),upper_bound_scopes([]),bitwidth(-1),pos(1,6))]),functions([predicate('show',[],[],boolean(true,pos(11,5)),pos(1,5))]),signatures([signature('Name',[],[],[],pos(5,1)),signature('Date',[],[],[],pos(5,2)),signature('BirthdayBook',[field('known',setof(identifier('Name',type([['Name']],1),pos(5,1)),type([['Name']],1),pos(26,3)),type([['Name']],1),[],pos(19,3)),field('date',cartesian(join(identifier(this,type([['BirthdayBook']],1),pos(1,1)),identifier('known',type([['BirthdayBook', 'Name']],2),pos(19,3)),type([['Name']],1),pos(42,3)),identifier('Date',type([['Date']],1),pos(5,2)),type([['Name', 'Date']],2),pos(48,3)),type([['Name', 'Date']],2),[],pos(36,3))],[],[],pos(5,3))])).