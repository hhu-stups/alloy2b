/*
1. There are four people: Roberta, Thelma, Steve, and Pete.
2. Among them, they hold eight different jobs.
3. Each holds exactly two jobs.
4. The jobs are: chef, guard, nurse, telephone operator, police officer (gender not implied), teacher, actor, and boxer.
5. The job of nurse is held by a male.
6. The husband of the chef is the telephone operator.
7. Roberta is not a boxer.
8. Pete has no education past the ninth grade.
9. Roberta, the chef, and the police officer went golfing together.
Question: Who holds which jobs?”
*/
module JobsPuzzle

open util/relation

// 1. There are four people: Roberta, Thelma, Steve, and Pete.
// inf: “if the four names did not clearly imply the sex of the people, [the puzzle] would be impossible to solve.” [p. 56]

abstract sig Female extends Person {
	husband : lone Male
} {
	//husband.wife = this
}
abstract sig Male extends Person {
	//wife : lone Female
} {
	//wife.husband = this
}

one sig Roberta, Thelma extends Female {} {}
one sig Steve, Pete extends Male {} {}

abstract sig Person {
	jobs : some Job,
} {
	#jobs  = 2 // 3. Each holds exactly two jobs.
}

fact implied {
	// inf: “No job is held by more than one person.” [p. 56]
	all disj p, p' : Person | no p.jobs & p'.jobs
	// inf: “everyday language distinguishes [actors and actresses] based on sex.” [p. 56]
	one p : Male | actor in p.jobs
	//all p : Person | actor in p.jobs implies p in Male
}

// 4. The jobs are: chef, guard, nurse, telephone operator, police officer (gender not implied), teacher, actor, and boxer.
abstract sig Job {}
abstract sig QualifiedJob extends Job {}
one sig nurse, teacher, police_officer extends QualifiedJob {}
one sig chef, guard, telephone_operator, actor, boxer extends Job {}

// 5. The job of nurse is held by a male.
fact fact5 {
	one p : Male | nurse in p.jobs
	//all p : Person | p in Female implies not nurse in p.jobs
}

// 6. The husband of the chef is the telephone operator.
// inf: “the implicit fact that husbands are male” [p. 57]
// inf: since the chef has a husband, she must be female. [p. 57]
fact fact6 {
	injective[husband,Male]
	husband[jobs.chef]= jobs.telephone_operator
}

// 7. Roberta is not a boxer.
fact fact7 {
	boxer not in Roberta.jobs
}

// 8. Pete has no education past the ninth grade.
// inf: “the jobs of nurse, police officer, and teacher each require more than a ninth-grade education.” [p. 57]
fact fact8 {
	Pete.jobs in Job - QualifiedJob
}

// 9. Roberta, the chef, and the police officer went golfing together.
// inf: “Thus, we know that Roberta is neither the chef nor the police officer.” [p. 57]
// inf: “Since they went golfing together, the chef and the police officer are not the same person.” [p. 57]
fact fact9 {
	#(Roberta + jobs.chef + jobs.police_officer) = 3
}

assert foo {

}
pred solve  {  }
run solve