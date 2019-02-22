open util/ordering[House]

enum Color { red, green, yellow, blue, white}
enum Nationality { Englishman, Swede, Dane, German, Norwegian}
enum Drink { tea, coffee, milk, beer, water}
enum Cigarette {Pall_Mall, Dunhills, Blend, Blue_Masters, Prince}
enum Pet { dogs, birds, horses, cats, fish }

sig House {
    colored  : one Color,
    lives    : one Nationality,
    drinks   : one Drink,
    smoker   : one Cigarette,
    keeps    : one Pet
}

fact  ensureUniqueFieldsForEachHouse {

    # House = 5

    all disj h1, h2 : House |
        all f : House$.subfields |
            h1.(f.value) != h2.(f.value)
}

pred House.hasNeighbourWho[ other : House ] {
    other in this.(prev+next)
}

let centerHouse = first.next.next

fact {
    // 1.  The Englishman lives in the red house.
    Englishman.~lives in colored.red

    // 2.  The Swede keeps dogs.
    Swede.~lives in keeps.dogs

    // 3.  The Dane drinks tea.
    Dane.~lives in drinks.tea

    // 4.  The green house is just to the left of the white one.
    green.~colored = colored.white.prev

    // 5.  The owner of the green house drinks coffee.
    green.~colored = drinks.coffee

    // 6.  The Pall Mall smoker keeps birds.
    Pall_Mall.~smoker = keeps.birds

    // 7.  The owner of the yellow house smokes Dunhills.
    yellow.~colored = smoker.Dunhills

    // 8.  The man in the center house drinks milk.
    centerHouse = drinks.milk

    // 9.  The Norwegian lives in the first house.
    Norwegian.~lives  = first

    // 10. The Blend smoker has a neighbor who keeps cats.
    Blend.~smoker.hasNeighbourWho[ keeps.cats ]

    // 11. The man who smokes Blue Masters drinks beer.
    Blue_Masters.~smoker = drinks.beer

    // 12. The man who keeps horses lives next to the Dunhill smoker.
    keeps.horses.hasNeighbourWho[smoker.Dunhills]

    // 13. The German smokes Prince.
    German.~lives = smoker.Prince

    // 14. The Norwegian lives next to the blue house.
    Norwegian.~lives.hasNeighbourWho[colored.blue]

    // 15. The Blend smoker has a neighbor who drinks water.
    Blend.~smoker.hasNeighbourWho[drinks.water]
}

run { one keeps.fish } for 5