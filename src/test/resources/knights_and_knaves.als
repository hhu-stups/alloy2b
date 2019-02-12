
	enum Person { Alice, Bob, Claire, Desmond, Elena, Fernando, Gary, Horace, Ingrid }

	one sig World {
		people	 : set Person,
		knights  : set people,
		knaves   : set people
	} {
		knights = people - knaves
	}

	let says[p,e] 	= (p) in World.knights => (e) else not (e)
	let isKnave[p] 	= (p) in World.knaves
	let isKnight[p] = (p) in World.knights

	-- Problem 1: There are two native islanders, named Alice and Bob,
	-- standing next to each other. You do not know what type either of
	-- them is. Suddenly, Alice says “At least one of us is a Knave.”
	run BobAlice {
		World.people = Bob+Alice

		-- Alice says: At least one of us is a Knave
		Alice.says[ some p : Bob+Alice | p.isKnave ]
	}

	-- Problem 2: Again, there are two native islanders standing next
	-- to each other. One is named Claire and the other is named Desmond.
	-- You do not know what type either of them are. Claire suddenly says,
	-- “We are both Knights”. After this, Desmond says “Either Claire is a
	-- Knight or I am a Knight, but we are not both Knights.

	run ClaireDesmond {
		World.people = Claire+Desmond

		-- Claire says “We are both Knights”
		Claire.says[ (Claire+Desmond).isKnight ]

		-- Desmond says “Either Claire is a
		-- Knight or I am a Knight, but we are not both Knights.
		Desmond.says[ (Claire.isKnight or Desmond.isKnight) and not ( Claire.isKnight and Desmond.isKnight ) ]
	}

	-- Problem 3: There are three native islanders, named Elena, Fernando, and Gary,
	-- standing together. You ask Elena, “How many knights are among you?”, and Elena
	-- answered but you couldn’t quite hear what she said. You then ask Fernando, “What
	-- did Elena say?”, and Fernando replies, “Elena said there is one knight among us.”
	-- At this point, Gary says “Don’t believe Fernando; he is lying.”

	run ElenaFernandoGary {
		World.people = Elena + Fernando + Gary

		Gary.says[ not Fernando.says[ 	Claire.says[ one World.knights ] ] ]

	}

