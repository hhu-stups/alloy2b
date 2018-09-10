sig Name {}
sig Date {}
sig BirthdayBook {known: set Name, date: known -> Date}
fact test { all b:BirthdayBook | b.date in b.known some -> some Date}
pred show {}
run show for exactly 1 BirthdayBook, exactly 2 Name, exactly 2 Date