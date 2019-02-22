

open util/ordering[House]

sig House {
    color: Color,
    nationality: Nationality,
    drink: Drink,
    cigarette: Cigarette,
    pet: Pet
}

abstract sig Color {}
one sig red extends Color {}
one sig green extends Color {}
one sig yellow extends Color {}
one sig blue extends Color {}
one sig white extends Color {}

abstract sig Nationality {}
one sig Englishman extends Nationality {}
one sig Swede extends Nationality {}
one sig Dane extends Nationality {}
one sig German extends Nationality {}
one sig Norwegian extends Nationality {}

abstract sig Drink {}
one sig tea extends Drink {}
one sig coffee extends Drink {}
one sig milk extends Drink {}
one sig beer extends Drink {}
one sig water extends Drink {}

abstract sig Cigarette {}
one sig Pall_Mall extends Cigarette {}
one sig Dunhills extends Cigarette {}
one sig Blend extends Cigarette {}
one sig Blue_Masters extends Cigarette {}
one sig Prince extends Cigarette {}

abstract sig Pet {}
one sig dog extends Pet {}
one sig bird extends Pet {}
one sig horse extends Pet {}
one sig cat extends Pet {}
one sig fish extends Pet {}

pred Who_keeps_fish {
    some disj h1, h2, h3, h4, h5: House | h1.color = red and h2.color = green and h3.color = yellow and h4.color = blue and h5.color = white
    no disj h,h': House | h.nationality = h'.nationality
    some h: House | (h.nationality = Englishman) and (h.color = red)
    some h: House | (h.nationality = Swede) and (h.pet = dog)
    some disj h, h': House | (h.color = green) and (h'.color = white) and (h'.prev = h)
    some h: House | (h.color = green) and (h.drink = coffee)
    some h: House | (h.cigarette = Pall_Mall) and (h.pet = bird)
    some h: House | (h.color = yellow) and (h.cigarette = Dunhills)
    some h: House | (some h.prev.prev) and (some h.next.next) and (h.drink = milk)
    some h: House | (h = first) and (h.nationality = Norwegian)
    some h: House | (h.cigarette = Blue_Masters) and (h.drink = beer)
    some disj h,h': House | (h.pet = horse) and (h'.cigarette = Dunhills) and ((h.next = h') or (h.prev = h'))
    some h: House | (h.nationality = German) and (h.cigarette = Prince)
    some disj h,h': House | (h.nationality = Norwegian) and (h'.color= blue) and (h.next = h')
    some disj h,h': House | (h.cigarette = Blend) and (h'.drink = water) and (h.next = h')
    some h: House | h.pet = fish
}

run Who_keeps_fish for 5
